#[derive(Debug, Copy, Clone)]
pub struct Escape {
    stack_index: usize,
}

impl Escape {
    pub fn new(stack_index: usize) -> Self {
        Escape { stack_index }
    }

    pub fn allocate(stack_index: usize) -> &'static Self {
        Box::leak(Box::new(Self::new(stack_index)))
    }
}
