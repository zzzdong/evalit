use std::{collections::HashMap, sync::OnceLock};

use pest::{
    Parser,
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
};

use super::ast::syntax::*;

#[derive(Debug)]
pub struct ParseError(Box<pest::error::Error<Rule>>);

impl ParseError {
    pub fn with_message(span: pest::Span<'_>, message: impl ToString) -> Self {
        Self(Box::new(pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: message.to_string(),
            },
            span,
        )))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        Self(Box::new(e))
    }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(pest_derive::Parser)]
#[grammar = "compiler/ast/grammar.pest"]
struct PestParser;

pub fn parse_file(input: &str) -> Result<Program> {
    let mut pairs = PestParser::parse(Rule::program, input)?;

    let pair = pairs.next().unwrap();
    parse_program(pair)
}

pub(crate) fn parse_expression_input(input: &str) -> Result<ExpressionNode> {
    let pairs = PestParser::parse(Rule::expression, input)?;
    parse_expression_pairs(pairs)
}

pub(crate) fn parse_statement_input(input: &str) -> Result<StatementNode> {
    let pair = PestParser::parse(Rule::statement, input)
        .unwrap()
        .next()
        .unwrap();
    parse_statement(pair)
}

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

fn pratt_parser() -> &'static PrattParser<Rule> {
    // reference: https://doc.rust-lang.org/nightly/reference/expressions.html
    PRATT_PARSER.get_or_init(|| {
        PrattParser::new()
            .op(Op::infix(Rule::assign_operator, Assoc::Right)
                | Op::infix(Rule::add_assign_operator, Assoc::Right)
                | Op::infix(Rule::sub_assign_operator, Assoc::Right)
                | Op::infix(Rule::mul_assign_operator, Assoc::Right)
                | Op::infix(Rule::div_assign_operator, Assoc::Right)
                | Op::infix(Rule::rem_assign_operator, Assoc::Right))
            .op(Op::infix(Rule::range_operator, Assoc::Left))
            .op(Op::infix(Rule::or_operator, Assoc::Left))
            .op(Op::infix(Rule::and_operator, Assoc::Left))
            .op(Op::infix(Rule::equal_operator, Assoc::Left)
                | Op::infix(Rule::not_equal_operator, Assoc::Left)
                | Op::infix(Rule::less_operator, Assoc::Left)
                | Op::infix(Rule::less_equal_operator, Assoc::Left)
                | Op::infix(Rule::greater_operator, Assoc::Left)
                | Op::infix(Rule::greater_equal_operator, Assoc::Left))
            .op(Op::infix(Rule::add_operator, Assoc::Left)
                | Op::infix(Rule::sub_operator, Assoc::Left))
            .op(Op::infix(Rule::mul_operator, Assoc::Left)
                | Op::infix(Rule::div_operator, Assoc::Left)
                | Op::infix(Rule::rem_operator, Assoc::Left))
            .op(Op::infix(Rule::pow_operator, Assoc::Right))
            .op(Op::prefix(Rule::negative_operator) | Op::prefix(Rule::not_operator))
            .op(Op::postfix(Rule::try_operator))
            .op(Op::infix(Rule::as_operator, Assoc::Left))
            .op(Op::infix(Rule::dot_operator, Assoc::Left))
            .op(Op::postfix(Rule::try_operator)
                | Op::postfix(Rule::await_operator)
                | Op::postfix(Rule::member_operator)
                | Op::postfix(Rule::call_operator)
                | Op::postfix(Rule::index_operator))
            .op(Op::postfix(Rule::slice_operator))
            .op(Op::infix(Rule::path_operator, Assoc::Left))
    })
}

fn parse_program(pair: Pair<Rule>) -> Result<Program> {
    let mut program = Program::new();
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::statement => {
                let statement = parse_statement(item)?;
                program.stmts.push(statement);
            }
            Rule::EOI => {
                break;
            }
            _ => unreachable!("{item:?}"),
        }
    }

    Ok(program)
}

fn parse_statement(pair: Pair<Rule>) -> Result<StatementNode> {
    let span = Span::from_pair(&pair);
    let pair = pair.into_inner().next().unwrap();

    let stmt = match pair.as_rule() {
        Rule::expression_statement => {
            let expression = parse_expression(pair)?;
            Statement::Expression(expression)
        }
        Rule::item_statement => {
            let stat = parse_item_statement(pair)?;
            Statement::Item(stat)
        }
        Rule::empty_statement => Statement::Empty,
        Rule::let_statement => {
            let stat = parse_let_statement(pair)?;
            Statement::Let(stat)
        }
        Rule::loop_statement => {
            let stat = parse_loop_statement(pair)?;
            Statement::Loop(stat)
        }
        Rule::while_statement => {
            let stat = parse_while_statement(pair)?;
            Statement::While(stat)
        }
        Rule::for_statement => {
            let stat = parse_for_statement(pair)?;
            Statement::For(stat)
        }
        Rule::if_statement => {
            let stat = parse_if_statement(pair)?;
            Statement::If(stat)
        }
        Rule::return_statement => {
            let stat = parse_return_statement(pair)?;
            Statement::Return(stat)
        }
        Rule::break_statement => Statement::Break,
        Rule::continue_statement => Statement::Continue,
        _ => unreachable!("unknown statement: {pair:?}"),
    };

    Ok(AstNode::new(stmt, span, Type::Unknown))
}

fn parse_item_statement(pair: Pair<Rule>) -> Result<ItemStatement> {
    let stat = pair.into_inner().next().unwrap();

    match stat.as_rule() {
        Rule::enum_item => Ok(ItemStatement::Enum(parse_enum_item(stat))),
        Rule::struct_item => Ok(ItemStatement::Struct(parse_struct_item(stat))),
        Rule::fn_item => parse_function_item(stat).map(ItemStatement::Fn),
        _ => unreachable!("unknown item statement: {stat:?}"),
    }
}

fn parse_enum_item(pair: Pair<Rule>) -> EnumItem {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let variants = pairs.next().unwrap().into_inner();

    let variants = variants.map(parse_enum_field).collect();

    EnumItem { name, variants }
}

fn parse_enum_field(pair: Pair<Rule>) -> EnumVariant {
    let field = pair.into_inner().next().unwrap();
    match field.as_rule() {
        Rule::simple_enum_field => EnumVariant::Simple(field.as_str().to_string()),
        Rule::tuple_enum_field => parse_tuple_enum_field(field),
        _ => unreachable!("{field:?}"),
    }
}

fn parse_tuple_enum_field(pair: Pair<Rule>) -> EnumVariant {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let tuple = pairs.map(|item| parse_type_expression(item)).collect();

    EnumVariant::Tuple(name, tuple)
}

fn parse_struct_item(pair: Pair<Rule>) -> StructItem {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let fields = pairs.next().unwrap();
    let fields = fields
        .into_inner()
        .map(|field| parse_struct_field(field))
        .collect();

    StructItem { name, fields }
}

fn parse_struct_field(pair: Pair<Rule>) -> StructField {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let ty = parse_type_expression(pairs.next().unwrap());

    StructField { name, ty }
}

fn parse_function_item(pair: Pair<Rule>) -> Result<FunctionItem> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let mut params = pairs.next().unwrap().into_inner();
    let params = if let Some(params) = params.next() {
        params
            .into_inner()
            .map(|param| parse_function_param(param))
            .collect()
    } else {
        Vec::new()
    };
    let mut return_type = pairs.next().unwrap().into_inner();
    let return_ty = return_type.next().map(parse_type_expression);

    let body = parse_block(pairs.next().unwrap())?;
    Ok(FunctionItem {
        name,
        params,
        return_ty,
        body,
    })
}

fn parse_function_param(pair: Pair<Rule>) -> FunctionParam {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let ty = pairs.next().map(|pair| parse_type_expression(pair));

    FunctionParam { name, ty }
}

fn parse_let_statement(pair: Pair<Rule>) -> Result<LetStatement> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let type_annotation = pairs.next().unwrap();

    let ty = type_annotation
        .into_inner()
        .next()
        .map(|pair| parse_type_expression(pair));

    let assignment = pairs.next().unwrap();

    let value = if let Some(pair) = assignment.into_inner().next() {
        Some(parse_expression(pair)?)
    } else {
        None
    };

    Ok(LetStatement { name, ty, value })
}

fn parse_loop_statement(pair: Pair<Rule>) -> Result<LoopStatement> {
    let inner = pair.into_inner().next().unwrap();
    let body = parse_block(inner)?;
    Ok(LoopStatement { body })
}

fn parse_while_statement(pair: Pair<Rule>) -> Result<WhileStatement> {
    let mut pairs = pair.into_inner();

    let condition = parse_expression(pairs.next().unwrap())?;
    let body = parse_block(pairs.next().unwrap())?;
    Ok(WhileStatement { condition, body })
}

fn parse_for_statement(pair: Pair<Rule>) -> Result<ForStatement> {
    let mut pairs = pair.into_inner();

    let pat = parse_pattern(pairs.next().unwrap())?;
    let iterable = parse_expression(pairs.next().unwrap())?;
    let body = parse_block(pairs.next().unwrap())?;

    Ok(ForStatement {
        pat,
        iterable,
        body,
    })
}

fn parse_if_statement(pair: Pair<Rule>) -> Result<IfStatement> {
    let mut pairs = pair.into_inner();

    let condition = parse_expression(pairs.next().unwrap())?;
    let then_branch = parse_block(pairs.next().unwrap())?;
    let else_branch = pairs
        .next()
        .map(|pair| {
            let span = Span::from_pair(&pair);
            match pair.as_rule() {
                Rule::block => parse_block(pair),
                Rule::if_statement => parse_if_statement(pair).map(|item| {
                    BlockStatement(vec![AstNode::new(Statement::If(item), span, Type::Unknown)])
                }),
                _ => unreachable!("unknown else_branch: {:?}", pair),
            }
        })
        .transpose()?;

    Ok(IfStatement {
        condition,
        then_branch,
        else_branch,
    })
}

fn parse_return_statement(pair: Pair<Rule>) -> Result<ReturnStatement> {
    let mut pairs = pair.into_inner();

    let value = pairs
        .next()
        .map(|pair| parse_expression(pair))
        .transpose()?;

    Ok(ReturnStatement { value })
}

fn parse_pattern(pair: Pair<Rule>) -> Result<Pattern> {
    let pat = pair.into_inner().next().unwrap();

    match pat.as_rule() {
        Rule::wildcard_pattern => Ok(Pattern::Wildcard),
        Rule::identifier => Ok(Pattern::Identifier(pat.as_str().to_string())),
        Rule::literal => Ok(Pattern::Literal(parse_literal(pat)?)),
        Rule::tuple_pattern => {
            let mut tuple = Vec::new();
            for item in pat.into_inner() {
                match item.as_rule() {
                    Rule::pattern => {
                        tuple.push(parse_pattern(item)?);
                    }
                    _ => unreachable!("unknown tuple pattern part: {item:?}"),
                }
            }
            Ok(Pattern::Tuple(tuple))
        }
        _ => unreachable!("unknown pattern: {pat:?}"),
    }
}

fn parse_block(pair: Pair<Rule>) -> Result<BlockStatement> {
    let statements = pair
        .into_inner()
        .map(parse_statement)
        .collect::<Result<_>>()?;
    Ok(BlockStatement(statements))
}

fn parse_type_expression(pair: Pair<Rule>) -> TypeExpression {
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::type_bool => TypeExpression::Boolean,
        Rule::type_byte => TypeExpression::Byte,
        Rule::type_int => TypeExpression::Integer,
        Rule::type_float => TypeExpression::Float,
        Rule::type_char => TypeExpression::Char,
        Rule::type_string => TypeExpression::String,
        Rule::type_user_defined => TypeExpression::UserDefined(pair.as_str().to_string()),
        Rule::type_array => TypeExpression::Array(Box::new(parse_type_expression(
            pair.into_inner().next().unwrap(),
        ))),
        Rule::type_tuple => {
            let tys = pair.into_inner().map(parse_type_expression).collect();
            TypeExpression::Tuple(tys)
        }
        Rule::type_generic => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let params = inner.map(parse_type_expression).collect();
            TypeExpression::Generic(name, params)
        }
        Rule::type_impl => TypeExpression::Impl(Box::new(parse_type_expression(
            pair.into_inner().next().unwrap(),
        ))),
        _ => unreachable!("unknown type expression: {pair:?}"),
    }
}

fn parse_expression(pair: Pair<Rule>) -> Result<ExpressionNode> {
    let pairs = pair.into_inner();

    parse_expression_pairs(pairs)
}

fn parse_expression_pairs(pairs: Pairs<Rule>) -> Result<ExpressionNode> {
    pratt_parser()
        .map_primary(parse_primary)
        .map_prefix(parse_prefix)
        .map_postfix(parse_postfix)
        .map_infix(parse_infix)
        .parse(pairs)
}

fn parse_primary(pair: Pair<Rule>) -> Result<ExpressionNode> {
    match pair.as_rule() {
        Rule::identifier | Rule::literal | Rule::atom => parse_atom(pair),
        Rule::expression => parse_expression(pair),
        Rule::grouped_expression => parse_expression(pair.into_inner().next().unwrap()),
        Rule::struct_expression => parse_struct_expression(pair),
        _ => unreachable!("unknown primary: {:?}", pair),
    }
}

fn parse_prefix(op: Pair<Rule>, rhs: Result<ExpressionNode>) -> Result<ExpressionNode> {
    let span = Span::from_pair(&op).merge(&rhs.as_ref().unwrap().span());

    let op = op.as_str().parse::<PrefixOp>().unwrap();

    let expr = Expression::Prefix(PrefixExpression {
        op,
        rhs: Box::new(rhs?),
    });

    Ok(AstNode::new(expr, span, Type::Unknown))
}

fn parse_infix(
    lhs: Result<ExpressionNode>,
    op: Pair<Rule>,
    rhs: Result<ExpressionNode>,
) -> Result<ExpressionNode> {
    let span = lhs
        .as_ref()
        .unwrap()
        .span()
        .merge(&rhs.as_ref().unwrap().span());

    let expr = match op.as_rule() {
        Rule::assign_operator => {
            let lhs_expr = lhs?;
            let rhs_expr = rhs?;

            match &lhs_expr.node {
                Expression::PropertyGet(PropertyGetExpression { object, property }) => {
                    Expression::PropertySet(PropertySetExpression {
                        object: object.clone(),
                        property: property.clone(),
                        value: Box::new(rhs_expr),
                    })
                }
                Expression::IndexGet(IndexGetExpression { object, index }) => {
                    Expression::IndexSet(IndexSetExpression {
                        object: object.clone(),
                        index: index.clone(),

                        value: Box::new(rhs_expr),
                    })
                }
                _ => Expression::Assign(AssignExpression {
                    object: Box::new(lhs_expr),
                    value: Box::new(rhs_expr),
                }),
            }

            // if let Expression::PropertyGet(PropertyGetExpression { object, property }) =
            //     &lhs_expr.node
            // {
            //     Expression::PropertySet(PropertySetExpression {
            //         object: object.clone(),
            //         property: property.clone(),
            //         value: Box::new(rhs_expr),
            //     })
            // } else {
            //     // Check if it's an index expression and handle it as index set
            //     if let Expression::Index(IndexExpression { ref object, ref index }) = lhs_expr.node {
            //         let index_expr = IndexExpression {
            //             object: object.clone(),
            //             index: index.clone(),
            //         };

            //         // Create an index set operation
            //         return Ok(AstNode::new(
            //             Expression::IndexSet(IndexSetExpression {
            //                 expr: Box::new(index_expr),
            //                 value: Box::new(rhs_expr)
            //             }),
            //             span,
            //             Type::Unknown
            //         ))
            //     }

            // }
        }
        Rule::add_assign_operator => create_assign_binary_expression(lhs, rhs, span, BinOp::Add),
        Rule::sub_assign_operator => create_assign_binary_expression(lhs, rhs, span, BinOp::Sub),
        Rule::mul_assign_operator => create_assign_binary_expression(lhs, rhs, span, BinOp::Mul),
        Rule::div_assign_operator => create_assign_binary_expression(lhs, rhs, span, BinOp::Div),
        Rule::rem_assign_operator => create_assign_binary_expression(lhs, rhs, span, BinOp::Rem),
        _ => Expression::Binary(BinaryExpression {
            op: op.as_str().parse::<BinOp>().unwrap(),
            lhs: Box::new(lhs?),
            rhs: Box::new(rhs?),
        }),
    };

    Ok(AstNode::new(expr, span, Type::Unknown))
}

fn create_assign_binary_expression(
    lhs: Result<ExpressionNode>,
    rhs: Result<ExpressionNode>,
    span: Span,
    bin_op: BinOp,
) -> Expression {
    let lhs_expr = lhs.unwrap();
    let rhs_expr = rhs.unwrap();

    if let Expression::PropertyGet(PropertyGetExpression { object, property }) =
        &lhs_expr.clone().node
    {
        // 构造 BinaryExpression 作为 PropertySet 的 value
        let binary = Expression::Binary(BinaryExpression {
            op: bin_op,
            lhs: Box::new(lhs_expr),
            rhs: Box::new(rhs_expr),
        });

        let binary_node = AstNode::new(binary, span, Type::Unknown);

        // 构造 PropertySetExpression
        Expression::PropertySet(PropertySetExpression {
            object: object.clone(),
            property: property.clone(),
            value: Box::new(binary_node),
        })
    } else {
        // 否则构造普通的 Assign + Binary 操作
        let binary = Expression::Binary(BinaryExpression {
            op: bin_op,
            lhs: Box::new(lhs_expr.clone()),
            rhs: Box::new(rhs_expr),
        });

        let binary_node = AstNode::new(binary, span, Type::Unknown);

        Expression::Assign(AssignExpression {
            object: Box::new(lhs_expr),
            value: Box::new(binary_node),
        })
    }
}

fn parse_postfix(lhs: Result<ExpressionNode>, op: Pair<Rule>) -> Result<ExpressionNode> {
    let span = lhs.as_ref().unwrap().span();
    let span = span.merge(&Span::from_pair(&op));

    let object = Box::new(lhs?);

    let expr = match op.as_rule() {
        Rule::try_operator => Expression::Try(object),
        Rule::member_operator => {
            let property = op.into_inner().next().unwrap().as_str().to_string();
            let expr = PropertyGetExpression { object, property };
            Expression::PropertyGet(expr)
        }
        Rule::call_operator => {
            let args = op.into_inner().next().unwrap();
            let args: Result<Vec<ExpressionNode>> =
                args.into_inner().map(parse_expression).collect();

            match object.node {
                Expression::PropertyGet(PropertyGetExpression { object, property }) => {
                    // Convert MemberExpression + Call to MethodCall
                    Expression::MethodCall(MethodCallExpression {
                        object,
                        method: property,
                        args: args?,
                    })
                }
                _ => {
                    // Normal function call
                    Expression::Call(CallExpression {
                        func: object,
                        args: args?,
                    })
                }
            }
        }
        Rule::index_operator => {
            let index = op.into_inner().next().unwrap();
            let inner = parse_expression(index.into_inner().next().unwrap())?;
            let span = inner.span();

            match inner.node {
                Expression::Binary(BinaryExpression {
                    op: BinOp::Range,
                    lhs,
                    rhs,
                }) => {
                    let range = RangeExpression {
                        op: BinOp::Range,
                        begin: Some(lhs),
                        end: Some(rhs),
                    };

                    let expr = SliceExpression {
                        object,
                        range: AstNode::new(range, span, Type::Range),
                    };

                    Expression::Slice(expr)
                }
                Expression::Binary(BinaryExpression {
                    op: BinOp::RangeInclusive,
                    lhs,
                    rhs,
                }) => {
                    let range = RangeExpression {
                        op: BinOp::RangeInclusive,
                        begin: Some(lhs),
                        end: Some(rhs),
                    };

                    let expr = SliceExpression {
                        object,
                        range: AstNode::new(range, span, Type::Range),
                    };

                    Expression::Slice(expr)
                }
                _ => {
                    let expr = IndexGetExpression {
                        object,
                        index: Box::new(inner),
                    };

                    Expression::IndexGet(expr)
                }
            }
        }
        Rule::slice_operator => {
            let mut begin = None;
            let mut end = None;
            let mut range = BinOp::Range;

            let span: Span = Span::from_pair(&op);

            for arg in op.into_inner() {
                match arg.as_rule() {
                    Rule::slice_begin => {
                        begin = Some(Box::new(parse_expression(
                            arg.into_inner().next().unwrap(),
                        )?))
                    }
                    Rule::slice_end => {
                        end = Some(Box::new(parse_expression(
                            arg.into_inner().next().unwrap(),
                        )?))
                    }
                    Rule::range_operator => {
                        range = arg.as_str().parse::<BinOp>().unwrap();
                    }
                    r => unreachable!("{r:?}"),
                }
            }

            let range = RangeExpression {
                op: range,
                begin,
                end,
            };

            let expr = SliceExpression {
                object,
                range: AstNode::new(range, span, Type::Range),
            };

            Expression::Slice(expr)
        }
        Rule::await_operator => Expression::Await(object),
        _ => unreachable!("unknown postfix: {:?}", op),
    };

    Ok(AstNode::new(expr, span, Type::Unknown))
}

fn parse_struct_expression(pair: Pair<Rule>) -> Result<ExpressionNode> {
    println!("parse_struct_expression, {pair:?}");

    let span = Span::from_pair(&pair);

    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap();
    assert_eq!(name.as_rule(), Rule::identifier);
    let name = name.as_str().to_string();

    let fields = pairs.next().unwrap().into_inner();

    let fields = fields
        .map(|pair| parse_struct_expression_field(pair))
        .collect::<Result<Vec<_>>>()?;

    let expr = StructExpression {
        name: name.clone(),
        fields,
    };



    Ok(AstNode::new(
        Expression::StructExpr(expr),
        span,
        Type::Unknown,
    ))
}

fn parse_struct_expression_field(pair: Pair<Rule>) -> Result<StructExprField> {
    println!("parse_struct_expression_field, {pair:?}");

    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap();
    assert_eq!(name.as_rule(), Rule::identifier);
    let name = name.as_str().to_string();

    let value = parse_expression(pairs.next().unwrap())?;

    Ok(StructExprField { name, value })
}

fn parse_atom(pair: Pair<Rule>) -> Result<ExpressionNode> {
    let pair = pair.into_inner().next().unwrap();
    let span = Span::from_pair(&pair);

    let expr = match pair.as_rule() {
        Rule::identifier => parse_identifier(pair).map(Expression::Identifier),
        Rule::literal => parse_literal(pair).map(Expression::Literal),
        Rule::tuple => parse_tuple(pair).map(Expression::Tuple),
        Rule::array => parse_array(pair).map(Expression::Array),
        Rule::map => parse_map(pair).map(Expression::Map),
        Rule::closure => parse_closure(pair).map(Expression::Closure),
        Rule::env => parse_env(pair).map(Expression::Environment),
        Rule::simple_path => parse_simple_path(pair).map(Expression::Path),
        _ => unreachable!("unknown atom: {:?}", pair),
    }?;

    Ok(AstNode::new(expr, span, Type::Unknown))
}

fn parse_closure(pair: Pair<Rule>) -> Result<ClosureExpression> {
    let mut pairs = pair.into_inner();

    let mut params = pairs.next().unwrap().into_inner();
    let params = if let Some(params) = params.next() {
        params
            .into_inner()
            .map(|param| parse_function_param(param))
            .collect()
    } else {
        Vec::new()
    };

    let body = parse_block(pairs.next().unwrap())?;

    Ok(ClosureExpression { params, body })
}

fn parse_identifier(pair: Pair<Rule>) -> Result<IdentifierExpression> {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap();

    Ok(IdentifierExpression(name.as_str().to_string()))
}

fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('"') => result.push('"'),
                Some('b') => result.push('\x08'), // 退格符
                Some('f') => result.push('\x0C'), // 换页符
                Some('0') => result.push('\0'),   // 空字符
                Some('\\') => result.push('\\'),  // 反斜杠
                Some('x') => {
                    // 处理 \x1F 格式的转义字符
                    let hex = chars.by_ref().take(2).collect::<String>();
                    if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                        result.push(byte as char);
                    } else {
                        result.push_str(&format!("\\x{hex}"));
                    }
                }
                Some('u') => {
                    // 处理 \u{1234} 格式的转义字符
                    if chars.next() == Some('{') {
                        let mut hex = String::new();
                        for d in chars.by_ref() {
                            if d == '}' {
                                break;
                            }
                            hex.push(d);
                        }
                        if let Ok(code_point) = u32::from_str_radix(&hex, 16) {
                            if let Some(c) = std::char::from_u32(code_point) {
                                result.push(c);
                            } else {
                                result.push_str(&format!("\\u{{{hex}}}"));
                            }
                        } else {
                            result.push_str(&format!("\\u{{{hex}}}"));
                        }
                    } else {
                        result.push_str("\\u");
                    }
                }
                Some(c) => result.push_str(&format!("\\{c}")), // 未知转义字符，保留原始形式
                None => result.push('\\'),                     // 单独的反斜杠，保留原始形式
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn parse_literal(pair: Pair<Rule>) -> Result<LiteralExpression> {
    let mut pairs = pair.into_inner();
    let value = pairs.next().unwrap();

    match value.as_rule() {
        Rule::null => Ok(LiteralExpression::Null),
        Rule::boolean => Ok(LiteralExpression::Boolean(value.as_str() == "true")),
        Rule::integer => Ok(LiteralExpression::Integer(value.as_str().parse().map_err(
            |err| ParseError::with_message(value.as_span(), format!("parse integer failed, {err}")),
        )?)),
        Rule::float => Ok(LiteralExpression::Float(value.as_str().parse().map_err(
            |err| ParseError::with_message(value.as_span(), format!("parse float failed, {err}")),
        )?)),
        Rule::string => {
            let s = value.as_str().trim_matches('"');
            // unescape string
            let s = unescape_string(s);

            Ok(LiteralExpression::String(s))
        }
        Rule::character => Ok(LiteralExpression::Char(
            value.as_str().chars().next().unwrap(),
        )),
        _ => unreachable!("unknown literal: {:?}", value),
    }
}

fn parse_tuple(pair: Pair<Rule>) -> Result<TupleExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<ExpressionNode>> = pairs.map(parse_expression).collect();

    Ok(TupleExpression(elements?))
}

fn parse_array(pair: Pair<Rule>) -> Result<ArrayExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<ExpressionNode>> = pairs.map(parse_expression).collect();

    Ok(ArrayExpression(elements?))
}

fn parse_map_item(pair: Pair<Rule>) -> Result<(ExpressionNode, ExpressionNode)> {
    let mut pairs = pair.into_inner();

    let key_pair = pairs.next().unwrap();
    let key = parse_expression(key_pair.clone())?;
    if !key.node().is_literal() {
        return Err(ParseError::with_message(
            key_pair.as_span(),
            "map key must be literal".to_string(),
        ));
    }

    let value = parse_expression(pairs.next().unwrap())?;

    Ok((key, value))
}

fn parse_map(pair: Pair<Rule>) -> Result<MapExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<(ExpressionNode, ExpressionNode)>> =
        pairs.map(parse_map_item).collect();

    Ok(MapExpression(elements?))
}

fn parse_env(pair: Pair<Rule>) -> Result<EnvironmentExpression> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap();

    Ok(EnvironmentExpression(name.as_str().to_string()))
}

fn parse_simple_path(pair: Pair<Rule>) -> Result<PathExpression> {
    let pairs = pair.into_inner();

    let segments: Result<Vec<PathSeg>> = pairs.map(parse_path_segment).collect();

    Ok(PathExpression(segments?))
}

fn parse_path_segment(pair: Pair<Rule>) -> Result<PathSeg> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap();
    match name.as_str() {
        "self" => return Ok(PathSeg::Self_),
        "super" => return Ok(PathSeg::Super),
        _ => {}
    }

    Ok(PathSeg::Name(name.as_str().to_string()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_literal_expression() {
        let input = r#"1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Literal(LiteralExpression::Integer(1))
        );

        let input = r#"2.0"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Literal(LiteralExpression::Float(2.0))
        );
    }

    #[test]
    fn test_identifier_expression() {
        let input = r#"foo"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Identifier(IdentifierExpression("foo".to_string()))
        );

        let input = r#"foo_bar"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Identifier(IdentifierExpression("foo_bar".to_string()))
        );

        let input = r#"_foo"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Identifier(IdentifierExpression("_foo".to_string()))
        );

        let input = r#"_"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Identifier(IdentifierExpression("_".to_string()))
        );
    }

    #[test]
    fn test_environment_expression() {
        let input = r#"${env}"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression.node,
            Expression::Environment(EnvironmentExpression("env".to_string()))
        );
    }

    #[test]
    fn test_tuple_expression() {
        let input = r#"(1, 2, 3)"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Tuple(TupleExpression(elements)) = expression.node {
            assert_eq!(elements.len(), 3);
            assert_eq!(
                elements[0].node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                elements[1].node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
            assert_eq!(
                elements[2].node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected tuple expression");
        }
    }

    #[test]
    fn test_array_expression() {
        let input = r#"[1, 2, 3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Array(ArrayExpression(elements)) = expression.node {
            assert_eq!(elements.len(), 3);
            assert_eq!(
                elements[0].node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                elements[1].node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
            assert_eq!(
                elements[2].node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected array expression");
        }
    }

    #[test]
    fn test_map_expression() {
        let input = r#"{"a": 1, "b": 2}"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Map(MapExpression(elements)) = expression.node {
            assert_eq!(elements.len(), 2);
            assert_eq!(
                elements[0].0.node,
                Expression::Literal(LiteralExpression::String("a".to_string()))
            );
            assert_eq!(
                elements[0].1.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                elements[1].0.node,
                Expression::Literal(LiteralExpression::String("b".to_string()))
            );
            assert_eq!(
                elements[1].1.node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
        } else {
            panic!("Expected map expression");
        }
    }

    #[test]
    fn test_closure_expression() {
        let input = r#"|x| { return x + 1; }"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Closure(closure) = expression.node {
            // 检查参数
            assert_eq!(closure.params.len(), 1);
            assert_eq!(closure.params[0].name, "x");
            assert!(closure.params[0].ty.is_none());

            // 检查函数体
            assert_eq!(closure.body.0.len(), 1);
            if let Statement::Return(return_stmt) = &closure.body.0[0].node {
                if let Some(value) = &return_stmt.value {
                    if let Expression::Binary(binary) = &value.node {
                        assert_eq!(binary.op, BinOp::Add);
                        assert_eq!(
                            binary.lhs.node,
                            Expression::Identifier(IdentifierExpression("x".to_string()))
                        );
                        assert_eq!(
                            binary.rhs.node,
                            Expression::Literal(LiteralExpression::Integer(1))
                        );
                    } else {
                        panic!("Expected binary expression in return value");
                    }
                } else {
                    panic!("Expected value in return statement");
                }
            } else {
                panic!("Expected return statement in closure body");
            }
        } else {
            panic!("Expected closure expression");
        }
    }

    #[test]
    fn test_index_get_expression() {
        let input = r#"a[1]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::IndexGet(index) = expression.node {
            assert_eq!(
                index.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(
                index.index.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
        } else {
            panic!("Expected index expression");
        }
    }

    #[test]
    fn test_slice_expression() {
        // Test case 1: a[1..3]
        let input = r#"a[1..3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Slice(slice) = expression.node {
            assert_eq!(
                slice.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            let range = &slice.range.node;
            assert_eq!(range.op, BinOp::Range);
            assert_eq!(
                range.begin.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                range.end.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected slice expression");
        }

        // Test case 2: a[1..=3]
        let input = r#"a[1..=3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Slice(slice) = expression.node {
            assert_eq!(
                slice.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            let range = &slice.range.node;
            assert_eq!(range.op, BinOp::RangeInclusive);
            assert_eq!(
                range.begin.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                range.end.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected slice expression");
        }

        // Test case 3: a[..]
        let input = r#"a[..]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Slice(slice) = expression.node {
            assert_eq!(
                slice.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            let range = &slice.range.node;
            assert_eq!(range.op, BinOp::Range);
            assert!(range.begin.is_none());
            assert!(range.end.is_none());
        } else {
            panic!("Expected slice expression");
        }

        // Test case 4: a[1..]
        let input = r#"a[1..]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Slice(slice) = expression.node {
            assert_eq!(
                slice.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            let range = &slice.range.node;
            assert_eq!(range.op, BinOp::Range);
            assert_eq!(
                range.begin.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert!(range.end.is_none());
        } else {
            panic!("Expected slice expression");
        }

        // Test case 5: a[..=3]
        let input = r#"a[..=3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Slice(slice) = expression.node {
            assert_eq!(
                slice.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            let range = &slice.range.node;
            assert_eq!(range.op, BinOp::RangeInclusive);
            assert!(range.begin.is_none());
            assert_eq!(
                range.end.as_ref().unwrap().node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected slice expression");
        }
    }

    #[test]
    fn test_call_expression() {
        let input = r#"a(1, 2, 3)"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Call(call) = expression.node {
            assert_eq!(
                call.func.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(call.args.len(), 3);
            assert_eq!(
                call.args[0].node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                call.args[1].node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
            assert_eq!(
                call.args[2].node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected call expression");
        }

        let input = r#"a()"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Call(call) = expression.node {
            assert_eq!(
                call.func.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(call.args.len(), 0);
        } else {
            panic!("Expected call expression");
        }
    }

    #[test]
    fn test_try_expression() {
        let input = r#"a?"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Try(expr) = expression.node {
            assert_eq!(
                expr.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
        } else {
            panic!("Expected try expression");
        }
    }

    #[test]
    fn test_await_expression() {
        let input = r#"a.await"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Await(expr) = expression.node {
            assert_eq!(
                expr.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
        } else {
            panic!("Expected await expression");
        }
    }

    #[test]
    fn test_prefix_expression() {
        let input = r#"-1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Prefix(prefix) = expression.node {
            assert_eq!(prefix.op, PrefixOp::Neg);
            assert_eq!(
                prefix.rhs.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
        } else {
            panic!("Expected prefix expression");
        }

        let input = r#"!a"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Prefix(prefix) = expression.node {
            assert_eq!(prefix.op, PrefixOp::Not);
            assert_eq!(
                prefix.rhs.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
        } else {
            panic!("Expected prefix expression");
        }
    }

    #[test]
    fn test_binary_expression() {
        // 测试加法
        let input = r#"1 + 2"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Add);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试减法
        let input = r#"5 - 3"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Sub);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(5))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试乘法
        let input = r#"4 * 6"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Mul);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(4))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(6))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试除法
        let input = r#"8 / 2"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Div);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(8))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试取模
        let input = r#"7 % 3"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Rem);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(7))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(3))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试等于比较
        let input = r#"1 == 1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::Equal);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试不等于比较
        let input = r#"1 != 2"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::NotEqual);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试逻辑与
        let input = r#"true && false"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::LogicAnd);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Boolean(true))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Boolean(false))
            );
        } else {
            panic!("Expected binary expression");
        }

        // 测试逻辑或
        let input = r#"true || false"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Binary(binary) = expression.node {
            assert_eq!(binary.op, BinOp::LogicOr);
            assert_eq!(
                binary.lhs.node,
                Expression::Literal(LiteralExpression::Boolean(true))
            );
            assert_eq!(
                binary.rhs.node,
                Expression::Literal(LiteralExpression::Boolean(false))
            );
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_empty_statement() {
        let input = r#";"#;
        let statement = parse_statement_input(input).unwrap();
        assert_eq!(statement.node, Statement::Empty);
    }

    #[test]
    fn test_break_statement() {
        let input = r#"break;"#;
        let statement = parse_statement_input(input).unwrap();
        assert_eq!(statement.node, Statement::Break);
    }

    #[test]
    fn test_continue_statement() {
        let input = r#"continue;"#;
        let statement = parse_statement_input(input).unwrap();
        assert_eq!(statement.node, Statement::Continue);
    }

    #[test]
    fn test_enum_item() {
        let input = r#"enum A { AA, BB(int, float) }"#;
        let item_statement = parse_statement_input(input).unwrap();
        if let Statement::Item(ItemStatement::Enum(enum_item)) = item_statement.node {
            // 检查枚举名称
            assert_eq!(enum_item.name, "A");

            // 检查变体数量
            assert_eq!(enum_item.variants.len(), 2);

            // 检查第一个变体（简单变体）
            assert_eq!(enum_item.variants[0], EnumVariant::Simple("AA".to_string()));

            // 检查第二个变体（元组变体）
            if let EnumVariant::Tuple(name, types) = &enum_item.variants[1] {
                assert_eq!(name, "BB");
                assert_eq!(types.len(), 2);
                assert_eq!(types[0], TypeExpression::Integer);
                assert_eq!(types[1], TypeExpression::Float);
            } else {
                panic!("Expected tuple variant");
            }
        } else {
            panic!("Expected enum item statement");
        }
    }

    #[test]
    fn test_struct_item() {
        let input = r#"struct A { a: int, b: float }"#;
        let item_statement = parse_statement_input(input).unwrap();
        if let Statement::Item(ItemStatement::Struct(struct_item)) = item_statement.node {
            // 检查结构体名称
            assert_eq!(struct_item.name, "A");

            // 检查字段数量
            assert_eq!(struct_item.fields.len(), 2);

            // 检查第一个字段
            assert_eq!(struct_item.fields[0].name, "a");
            assert_eq!(struct_item.fields[0].ty, TypeExpression::Integer);

            // 检查第二个字段
            assert_eq!(struct_item.fields[1].name, "b");
            assert_eq!(struct_item.fields[1].ty, TypeExpression::Float);
        } else {
            panic!("Expected struct item statement");
        }
    }

    #[test]
    fn test_function_item() {
        let input = r#"fn A(a: int, b: float) -> int { return a + b; }"#;
        let item_statement = parse_statement_input(input).unwrap();
        if let Statement::Item(ItemStatement::Fn(function)) = item_statement.node {
            // 检查函数名和参数
            assert_eq!(function.name, "A");
            assert_eq!(function.params.len(), 2);
            assert_eq!(function.params[0].name, "a");
            assert_eq!(function.params[0].ty, Some(TypeExpression::Integer));
            assert_eq!(function.params[1].name, "b");
            assert_eq!(function.params[1].ty, Some(TypeExpression::Float));

            // 检查返回类型
            assert_eq!(function.return_ty, Some(TypeExpression::Integer));

            // 检查函数体
            assert_eq!(function.body.0.len(), 1);
            if let Statement::Return(return_stmt) = &function.body.0[0].node {
                if let Some(value) = &return_stmt.value {
                    if let Expression::Binary(binary) = &value.node {
                        assert_eq!(binary.op, BinOp::Add);
                        assert_eq!(
                            binary.lhs.node,
                            Expression::Identifier(IdentifierExpression("a".to_string()))
                        );
                        assert_eq!(
                            binary.rhs.node,
                            Expression::Identifier(IdentifierExpression("b".to_string()))
                        );
                    } else {
                        panic!("Expected binary expression in return value");
                    }
                } else {
                    panic!("Expected value in return statement");
                }
            } else {
                panic!("Expected return statement in function body");
            }
        } else {
            panic!("Expected function item statement");
        }
    }

    #[test]
    fn test_let_statement() {
        let input = r#"let a = 1;"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Let(let_stmt) = statement.node {
            assert_eq!(let_stmt.name, "a");
            assert!(let_stmt.ty.is_none());
            if let Some(value) = let_stmt.value {
                assert_eq!(
                    value.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected value in let statement");
            }
        } else {
            panic!("Expected let statement");
        }
    }

    #[test]
    fn test_let_statement_with_type() {
        let input = r#"let a: int = 1;"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Let(let_stmt) = statement.node {
            assert_eq!(let_stmt.name, "a");
            assert_eq!(let_stmt.ty, Some(TypeExpression::Integer));
            if let Some(value) = let_stmt.value {
                assert_eq!(
                    value.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected value in let statement");
            }
        } else {
            panic!("Expected let statement");
        }
    }

    #[test]
    fn test_loop_statement() {
        let input = r#"loop { break; }"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Loop(loop_stmt) = statement.node {
            assert_eq!(loop_stmt.body.0.len(), 1);
            assert_eq!(loop_stmt.body.0[0].node, Statement::Break);
        } else {
            panic!("Expected loop statement");
        }
    }

    #[test]
    fn test_while_statement() {
        let input = r#"while a == 1 { break; }"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::While(while_stmt) = statement.node {
            // 检查条件
            if let Expression::Binary(binary) = while_stmt.condition.node {
                assert_eq!(binary.op, BinOp::Equal);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Identifier(IdentifierExpression("a".to_string()))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected binary expression in condition");
            }

            // 检查循环体
            assert_eq!(while_stmt.body.0.len(), 1);
            assert_eq!(while_stmt.body.0[0].node, Statement::Break);
        } else {
            panic!("Expected while statement");
        }
    }

    #[test]
    fn test_for_statement() {
        let input = r#"for i in 0..10 {}"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::For(for_stmt) = statement.node {
            // 检查模式
            assert_eq!(for_stmt.pat, Pattern::Identifier("i".to_string()));

            // 检查迭代器表达式
            if let Expression::Binary(binary) = for_stmt.iterable.node {
                assert_eq!(binary.op, BinOp::Range);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Literal(LiteralExpression::Integer(0))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(10))
                );
            } else {
                panic!("Expected binary expression in iterable");
            }

            // 检查循环体
            assert_eq!(for_stmt.body.0.len(), 0);
        } else {
            panic!("Expected for statement");
        }
    }

    #[test]
    fn test_if_statement() {
        // 测试基本的if语句
        let input = r#"if a == 1 { return 1; }"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::If(if_stmt) = statement.node {
            // 检查条件
            if let Expression::Binary(binary) = if_stmt.condition.node {
                assert_eq!(binary.op, BinOp::Equal);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Identifier(IdentifierExpression("a".to_string()))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected binary expression in condition");
            }

            // 检查then分支
            assert_eq!(if_stmt.then_branch.0.len(), 1);
            if let Statement::Return(return_stmt) = &if_stmt.then_branch.0[0].node {
                if let Some(value) = &return_stmt.value {
                    assert_eq!(
                        value.node,
                        Expression::Literal(LiteralExpression::Integer(1))
                    );
                } else {
                    panic!("Expected value in return statement");
                }
            } else {
                panic!("Expected return statement in then branch");
            }

            // 检查else分支
            assert!(if_stmt.else_branch.is_none());
        } else {
            panic!("Expected if statement");
        }

        // 测试带else分支的if语句
        let input = r#"if a == 2 { return 2; } else { return 3; }"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::If(if_stmt) = statement.node {
            // 检查条件
            if let Expression::Binary(binary) = if_stmt.condition.node {
                assert_eq!(binary.op, BinOp::Equal);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Identifier(IdentifierExpression("a".to_string()))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(2))
                );
            } else {
                panic!("Expected binary expression in condition");
            }

            // 检查then分支
            assert_eq!(if_stmt.then_branch.0.len(), 1);
            if let Statement::Return(return_stmt) = &if_stmt.then_branch.0[0].node {
                if let Some(value) = &return_stmt.value {
                    assert_eq!(
                        value.node,
                        Expression::Literal(LiteralExpression::Integer(2))
                    );
                } else {
                    panic!("Expected value in return statement");
                }
            } else {
                panic!("Expected return statement in then branch");
            }

            // 检查else分支
            if let Some(else_branch) = if_stmt.else_branch {
                assert_eq!(else_branch.0.len(), 1);
                if let Statement::Return(return_stmt) = &else_branch.0[0].node {
                    if let Some(value) = &return_stmt.value {
                        assert_eq!(
                            value.node,
                            Expression::Literal(LiteralExpression::Integer(3))
                        );
                    } else {
                        panic!("Expected value in return statement");
                    }
                } else {
                    panic!("Expected return statement in else branch");
                }
            } else {
                panic!("Expected else branch");
            }
        } else {
            panic!("Expected if statement");
        }

        // 测试复杂条件的if语句
        let input = r#"if a > 0 && b < 10 { return 4; }"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::If(if_stmt) = statement.node {
            // 检查条件
            if let Expression::Binary(binary) = if_stmt.condition.node {
                assert_eq!(binary.op, BinOp::LogicAnd);

                // 检查左侧条件 (a > 0)
                if let Expression::Binary(left_binary) = binary.lhs.node {
                    assert_eq!(left_binary.op, BinOp::Greater);
                    assert_eq!(
                        left_binary.lhs.node,
                        Expression::Identifier(IdentifierExpression("a".to_string()))
                    );
                    assert_eq!(
                        left_binary.rhs.node,
                        Expression::Literal(LiteralExpression::Integer(0))
                    );
                } else {
                    panic!("Expected binary expression in left condition");
                }

                // 检查右侧条件 (b < 10)
                if let Expression::Binary(right_binary) = binary.rhs.node {
                    assert_eq!(right_binary.op, BinOp::Less);
                    assert_eq!(
                        right_binary.lhs.node,
                        Expression::Identifier(IdentifierExpression("b".to_string()))
                    );
                    assert_eq!(
                        right_binary.rhs.node,
                        Expression::Literal(LiteralExpression::Integer(10))
                    );
                } else {
                    panic!("Expected binary expression in right condition");
                }
            } else {
                panic!("Expected binary expression in condition");
            }

            // 检查then分支
            assert_eq!(if_stmt.then_branch.0.len(), 1);
            if let Statement::Return(return_stmt) = &if_stmt.then_branch.0[0].node {
                if let Some(value) = &return_stmt.value {
                    assert_eq!(
                        value.node,
                        Expression::Literal(LiteralExpression::Integer(4))
                    );
                } else {
                    panic!("Expected value in return statement");
                }
            } else {
                panic!("Expected return statement in then branch");
            }

            // 检查else分支
            assert!(if_stmt.else_branch.is_none());
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r#"return 1;"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Return(return_stmt) = statement.node {
            if let Some(value) = return_stmt.value {
                assert_eq!(
                    value.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected value in return statement");
            }
        } else {
            panic!("Expected return statement");
        }

        let input = r#"return;"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Return(return_stmt) = statement.node {
            assert!(return_stmt.value.is_none());
        } else {
            panic!("Expected return statement");
        }
    }

    #[test]
    fn test_expression_statement() {
        let input = r#"1 + 2;"#;
        let statement = parse_statement_input(input).unwrap();
        if let Statement::Expression(expr) = statement.node {
            if let Expression::Binary(binary) = expr.node {
                assert_eq!(binary.op, BinOp::Add);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(2))
                );
            } else {
                panic!("Expected binary expression");
            }
        } else {
            panic!("Expected expression statement");
        }
    }

    #[test]
    fn test_property_get_expression() {
        let input = r#"a.b"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::PropertyGet(property_get) = expression.node {
            assert_eq!(property_get.property, "b");
            assert_eq!(
                property_get.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
        } else {
            panic!("Expected property get expression");
        }

        let input = r#"a.b.c"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::PropertyGet(property_get) = expression.node {
            assert_eq!(property_get.property, "c");
            if let Expression::PropertyGet(inner_property_get) = property_get.object.node {
                assert_eq!(inner_property_get.property, "b");
                assert_eq!(
                    inner_property_get.object.node,
                    Expression::Identifier(IdentifierExpression("a".to_string()))
                );
            } else {
                panic!("Expected inner member expression");
            }
        } else {
            panic!("Expected member expression");
        }
    }

    #[test]
    fn test_property_set_expression() {
        let input = r#"a.b = 1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::PropertySet(property_set) = expression.node {
            assert_eq!(property_set.property, "b");
            assert_eq!(
                property_set.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(
                property_set.value.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
        } else {
            panic!("Expected property set expression");
        }
    }

    #[test]
    fn test_index_set_expression() {
        let input = r#"a[1] = 42"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();

        if let Expression::IndexSet(IndexSetExpression {
            object,
            index,
            value,
        }) = expression.node
        {
            assert_eq!(
                object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(
                index.node,
                Expression::Literal(LiteralExpression::Integer(1))
            );

            assert_eq!(
                value.node,
                Expression::Literal(LiteralExpression::Integer(42))
            );
        } else {
            panic!("Expected index set expression");
        }
    }

    #[test]
    fn test_method_call_expression() {
        let input = r#"a.b(1, 2)"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::MethodCall(method_call) = expression.node {
            assert_eq!(method_call.method, "b");
            assert_eq!(
                method_call.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            assert_eq!(method_call.args.len(), 2);
            assert_eq!(
                method_call.args[0].node,
                Expression::Literal(LiteralExpression::Integer(1))
            );
            assert_eq!(
                method_call.args[1].node,
                Expression::Literal(LiteralExpression::Integer(2))
            );
        } else {
            panic!("Expected method call expression");
        }
    }

    #[test]
    fn test_create_assign_binary_expression() {
        // 测试 PropertyGet + Assign 操作
        let input = r#"a.b = a.b + 1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::PropertySet(property_set) = expression.node {
            assert_eq!(property_set.property, "b");
            assert_eq!(
                property_set.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            if let Expression::Binary(binary) = property_set.value.node {
                assert_eq!(binary.op, BinOp::Add);
                if let Expression::PropertyGet(property_get) = binary.lhs.node {
                    assert_eq!(property_get.property, "b");
                    assert_eq!(
                        property_get.object.node,
                        Expression::Identifier(IdentifierExpression("a".to_string()))
                    );
                } else {
                    panic!("Expected PropertyGet on lhs");
                }
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected binary expression in value");
            }
        } else {
            panic!("Expected PropertySet expression");
        }

        // 测试普通变量的 Assign + Binary 操作
        let input = r#"a = a + 1"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        if let Expression::Assign(assign) = expression.node {
            assert_eq!(
                assign.object.node,
                Expression::Identifier(IdentifierExpression("a".to_string()))
            );
            if let Expression::Binary(binary) = assign.value.node {
                assert_eq!(binary.op, BinOp::Add);
                assert_eq!(
                    binary.lhs.node,
                    Expression::Identifier(IdentifierExpression("a".to_string()))
                );
                assert_eq!(
                    binary.rhs.node,
                    Expression::Literal(LiteralExpression::Integer(1))
                );
            } else {
                panic!("Expected binary expression in value");
            }
        } else {
            panic!("Expected Assign expression");
        }
    }
}
