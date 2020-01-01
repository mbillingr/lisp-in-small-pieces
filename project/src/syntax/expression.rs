use super::alternative::Alternative;
use super::application::Application;
use super::assignment::Assignment;
use super::constant::Constant;
use super::fixlet::FixLet;
use super::function::Function;
use super::keyword::MagicKeyword;
use super::reference::Reference;
use super::sequence::Sequence;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Expression = MagicKeyword
                        | Reference
                        | Assignment
                        | Constant
                        | Sequence
                        | Alternative
                        | Function
                        | Application
                        | FixLet;
}
