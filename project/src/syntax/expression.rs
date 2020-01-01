use super::assignment::Assignment;
use super::keyword::MagicKeyword;
use super::reference::Reference;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Expression = MagicKeyword
                        | Reference
                        | Assignment;
}
