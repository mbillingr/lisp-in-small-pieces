pub trait Named {
    type Name: PartialEq;
    fn name(&self) -> Self::Name;
}
