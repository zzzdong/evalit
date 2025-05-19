use crate::{RuntimeError, ValueRef};

use super::{Object, Value};

/// Range
#[derive(Debug)]
pub enum Range {
    Normal { begin: i64, end: i64 },
    Inclusive { begin: i64, end: i64 },
    From { begin: i64 },
    To { end: i64 },
    ToInclusive { end: i64 },
    Full,
}

impl Range {
    pub fn new(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().value().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        let end = match end.clone().value().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };

        Ok(Range::Normal { begin, end })
    }

    pub fn inclusive(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().value().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        let end = match end.clone().value().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };

        Ok(Range::Inclusive { begin, end })
    }

    pub fn range_full() -> Self {
        Range::Full
    }

    pub fn range_from(begin: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().value().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        Ok(Range::From { begin })
    }

    pub fn range_to(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = match end.clone().value().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };
        Ok(Range::To { end })
    }

    pub fn range_to_inclusive(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = match end.clone().value().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };
        Ok(Range::ToInclusive { end })
    }

    pub fn get_range(&self, len: usize) -> Result<(usize, usize), RuntimeError> {
        let len = len as i64;
        // 根据Range类型计算实际的开始和结束索引
        let (start, end) = match *self {
            Range::Normal { begin, end } => {
                check_index(begin, len)?;
                check_index(end, len)?;
                (begin, end)
            }
            Range::Inclusive { begin, end } => {
                check_index(begin, len)?;
                check_index(end, len)?;
                (begin, end + 1)
            }
            Range::From { begin } => {
                check_index(begin, len)?;
                (begin, len)
            }
            Range::To { end } => {
                check_index(end, len)?;
                (0, end)
            }
            Range::ToInclusive { end } => {
                check_index(end, len)?;
                (0, end + 1)
            }
            Range::Full => (0, len),
        };

        // 检查范围有效性
        if start > end {
            return Err(RuntimeError::invalid_operation(
                super::OperateKind::IndexGet,
                format!("invalid slice range: {start}..{end}"),
            ));
        }

        Ok((start as usize, end as usize))
    }
}

impl Object for Range {
    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
        match self {
            Range::Normal { begin, end } => {
                Ok(Box::new((*begin..*end).map(|i| Value::new(i).into())))
            }
            Range::Inclusive { begin, end } => {
                Ok(Box::new((*begin..=*end).map(|i| Value::new(i).into())))
            }
            Range::From { begin } => Ok(Box::new((*begin..).map(|i| Value::new(i).into()))),
            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::MakeIterator,
                format!("range {self:?} is not iterable"),
            )),
        }
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "len" => {
                if args.is_empty() {
                    let len = match self {
                        Range::Normal { begin, end } => *end - *begin,
                        Range::Inclusive { begin, end } => *end - *begin + 1,
                        Range::From { begin } => i64::MAX - *begin,
                        Range::To { end } => *end,
                        Range::ToInclusive { end } => *end + 1,
                        Range::Full => i64::MAX,
                    };

                    return Ok(Some(Value::new(len).into()));
                }

                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            _ => Err(RuntimeError::missing_method::<Self>(method)),
        }
    }
}

// 辅助函数：检查索引是否有效
fn check_index(index: i64, len: i64) -> Result<(), RuntimeError> {
    if index < 0 || index > len {
        return Err(RuntimeError::IndexOutOfBounds { index, length: len });
    }
    Ok(())
}
