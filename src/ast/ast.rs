use std::{
    fmt, io,
    io::{Error, ErrorKind},
    str::FromStr,
};

use pest::{RuleType, iterators::Pair};

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode<T> {
    pub node: T,
    pub span: Span,
    pub ty: Type,
}

impl<T> AstNode<T> {
    pub fn new(node: T, span: Span, ty: Type) -> AstNode<T> {
        AstNode { span, node, ty }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn node(&self) -> &T {
        &self.node
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl<T> AsRef<T> for AstNode<T> {
    fn as_ref(&self) -> &T {
        &self.node
    }
}

impl<T> AsMut<T> for AstNode<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn from_pair<R: RuleType>(pair: &Pair<R>) -> Span {
        Span {
            start: pair.as_span().start(),
            end: pair.as_span().end(),
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Null,
    Boolean,
    Byte,
    Integer,
    Float,
    Char,
    String,
    Function {
        params: Vec<Option<Type>>,
        return_ty: Option<Box<Type>>,
    },
    DynObject,
    Range,
    EnvObject,
    Unknown,
}

impl Type {
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean | Type::Void)
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Byte | Type::Integer | Type::Float | Type::Void)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Type::DynObject)
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown)
    }
}

pub type StatementNode = AstNode<Statement>;
pub type ExpressionNode = AstNode<Expression>;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub stmts: Vec<StatementNode>,
}

impl Program {
    pub fn new() -> Program {
        Program { stmts: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Empty,
    Break,
    Continue,
    Block(BlockStatement),
    Item(ItemStatement),
    Let(LetStatement),
    For(ForStatement),
    While(WhileStatement),
    Loop(LoopStatement),
    If(IfStatement),
    Return(ReturnStatement),
    Expression(ExpressionNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement(pub Vec<StatementNode>);

#[derive(Debug, Clone, PartialEq)]
pub enum ItemStatement {
    Enum(EnumItem),
    Struct(StructItem),
    Fn(FunctionItem),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumItem {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariant {
    Simple(String),
    Tuple(String, Vec<TypeExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructItem {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_ty: Option<TypeExpression>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: String,
    pub ty: Option<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub ty: Option<TypeExpression>,
    pub value: Option<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStatement {
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: ExpressionNode,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub pat: Pattern,
    pub iterable: ExpressionNode,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: ExpressionNode,
    pub then_branch: BlockStatement,
    pub else_branch: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub value: Option<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Boolean,
    Byte,
    Integer,
    Float,
    Char,
    String,
    Array(Box<TypeExpression>),
    Tuple(Vec<TypeExpression>),
    Generic(String, Vec<TypeExpression>),
    UserDefined(String),
    Impl(Box<TypeExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpression),
    Identifier(IdentifierExpression),
    Environment(EnvironmentExpression),
    Path(PathExpression),
    Tuple(TupleExpression),
    Array(ArrayExpression),
    Map(MapExpression),
    Closure(ClosureExpression),
    Member(MemberExpression),
    Index(IndexExpression),
    Range(RangeExpression),
    Slice(SliceExpression),
    Assign(AssignExpression),
    Call(CallExpression),
    Try(Box<ExpressionNode>),
    Await(Box<ExpressionNode>),
    Prefix(PrefixExpression),
    Binary(BinaryExpression),
}

impl Expression {
    pub(crate) fn is_literal(&self) -> bool {
        matches!(self, Expression::Literal(_))
    }

    pub(crate) fn as_literal(&self) -> Option<LiteralExpression> {
        match self {
            Expression::Literal(lit) => Some(lit.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpression {
    pub params: Vec<FunctionParam>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpression {
    pub object: Box<ExpressionNode>,
    pub value: Box<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub object: Box<ExpressionNode>,
    pub property: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub func: Box<ExpressionNode>,
    pub args: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub object: Box<ExpressionNode>,
    pub index: Box<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpression {
    pub op: BinOp,
    pub begin: Option<Box<ExpressionNode>>,
    pub end: Option<Box<ExpressionNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SliceExpression {
    pub object: Box<ExpressionNode>,
    pub range: AstNode<RangeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Literal(LiteralExpression),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub op: BinOp,
    pub lhs: Box<ExpressionNode>,
    pub rhs: Box<ExpressionNode>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogicAnd,
    LogicOr,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    As,
    Range,
    RangeInclusive,
    Path,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::LogicAnd => write!(f, "&&"),
            BinOp::LogicOr => write!(f, "||"),
            BinOp::Less => write!(f, "<"),
            BinOp::LessEqual => write!(f, "<="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEqual => write!(f, ">="),
            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::As => write!(f, "as"),
            BinOp::Range => write!(f, ".."),
            BinOp::RangeInclusive => write!(f, "..="),
            BinOp::Path => write!(f, "::"),
        }
    }
}

impl FromStr for BinOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinOp::Add),
            "-" => Ok(BinOp::Sub),
            "*" => Ok(BinOp::Mul),
            "/" => Ok(BinOp::Div),
            "%" => Ok(BinOp::Mod),
            "&&" => Ok(BinOp::LogicAnd),
            "||" => Ok(BinOp::LogicOr),
            "<" => Ok(BinOp::Less),
            "<=" => Ok(BinOp::LessEqual),
            ">" => Ok(BinOp::Greater),
            ">=" => Ok(BinOp::GreaterEqual),
            "==" => Ok(BinOp::Equal),
            "!=" => Ok(BinOp::NotEqual),
            "as" => Ok(BinOp::As),
            ".." => Ok(BinOp::Range),
            "..=" => Ok(BinOp::RangeInclusive),
            "::" => Ok(BinOp::Path),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid binanry op: {s}"),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    /// -
    Neg,
    /// !
    Not,
}

impl FromStr for PrefixOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(PrefixOp::Neg),
            "!" => Ok(PrefixOp::Not),
            _ => Err(Error::new(
                ErrorKind::InvalidInput,
                format!("Invalid unary op: {s}"),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub op: PrefixOp,
    pub rhs: Box<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    Await,
    Try,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PostfixExpression {
    pub op: PostfixOp,
    pub lhs: Box<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierExpression(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpression {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpression(pub Vec<ExpressionNode>);

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression(pub Vec<ExpressionNode>);

#[derive(Debug, Clone, PartialEq)]
pub struct MapExpression(pub Vec<(ExpressionNode, ExpressionNode)>);

#[derive(Debug, Clone, PartialEq)]
pub struct EnvironmentExpression(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct PathExpression(pub Vec<PathSeg>);

#[derive(Debug, Clone, PartialEq)]
pub enum PathSeg {
    Name(String),
    Super,
    Self_,
    Crate,
}
