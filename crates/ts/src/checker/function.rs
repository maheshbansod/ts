use super::TsType;

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TsFunction<'a> {
    pub args: Vec<TsType<'a>>,
    pub return_type: Box<TsType<'a>>,
}

impl<'a> TsFunction<'a> {
    pub fn new(args: Vec<TsType<'a>>, return_type: Box<TsType<'a>>) -> Self {
        Self { args, return_type }
    }
}
