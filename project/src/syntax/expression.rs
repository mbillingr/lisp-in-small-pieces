use super::alternative::Alternative;
use super::assignment::Assignment;
use super::constant::Constant;
use super::keyword::MagicKeyword;
use super::reference::Reference;
use super::sequence::Sequence;
use super::function::Function;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Expression = MagicKeyword
                        | Reference
                        | Assignment
                        | Constant
                        | Sequence
                        | Alternative
                        | Function;
}
