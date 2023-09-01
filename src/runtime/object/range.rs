use crate::{RuntimeError, ValueRef};

use super::{Object, Value};

/// Range
#[derive(Debug)]
pub enum Range {
    Range { begin: i64, end: i64 },
    RangeInclusive { begin: i64, end: i64 },
    RangeFrom { begin: i64 },
    RangeTo { end: i64 },
    RangeToInclusive { end: i64 },
    RangeFull,
}

impl Range {
    pub fn new(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        let end = match end.clone().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };

        Ok(Range::Range { begin, end })
    }

    pub fn inclusive(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        let end = match end.clone().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };

        Ok(Range::RangeInclusive { begin, end })
    }

    pub fn range_full() -> Self {
        Range::RangeFull
    }

    pub fn range_from(begin: ValueRef) -> Result<Self, RuntimeError> {
        let begin = match begin.clone().downcast_ref::<i64>() {
            Some(begin) => *begin,
            None => return Err(RuntimeError::invalid_type::<i64>(begin)),
        };

        Ok(Range::RangeFrom { begin })
    }

    pub fn range_to(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = match end.clone().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };
        Ok(Range::RangeTo { end })
    }

    pub fn range_to_inclusive(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = match end.clone().downcast_ref::<i64>() {
            Some(end) => *end,
            None => return Err(RuntimeError::invalid_type::<i64>(end)),
        };
        Ok(Range::RangeToInclusive { end })
    }

    pub fn get_range(&self, len: usize) -> Result<(usize, usize), RuntimeError> {
        let len = len as i64;
        // 根据Range类型计算实际的开始和结束索引
        let (start, end) = match *self {
            Range::Range { begin, end } => {
                check_index(begin, len)?;
                check_index(end, len)?;
                (begin, end)
            }
            Range::RangeInclusive { begin, end } => {
                check_index(begin, len)?;
                check_index(end, len)?;
                (begin, end + 1)
            }
            Range::RangeFrom { begin } => {
                check_index(begin, len)?;
                (begin, len)
            }
            Range::RangeTo { end } => {
                check_index(end, len)?;
                (0, end)
            }
            Range::RangeToInclusive { end } => {
                check_index(end, len)?;
                (0, end + 1)
            }
            Range::RangeFull => (0, len),
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
    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        match self {
            Range::Range { begin, end } => {
                Ok(Box::new((*begin..*end).map(|i| Value::new(i).into())))
            }
            Range::RangeInclusive { begin, end } => {
                Ok(Box::new((*begin..=*end).map(|i| Value::new(i).into())))
            }
            Range::RangeFrom { begin } => Ok(Box::new((*begin..).map(|i| Value::new(i).into()))),
            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::MakeIterator,
                format!("range {self:?} is not iterable"),
            )),
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
