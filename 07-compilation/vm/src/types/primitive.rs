use crate::vm::VirtualMachine;

#[derive(Copy, Clone)]
pub struct Primitive {
    name: &'static str,
    behavior: fn(&mut VirtualMachine),
}

impl Primitive {
    pub fn new(name: &'static str, behavior: fn(&mut VirtualMachine)) -> Self {
        Primitive { name, behavior }
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn invoke(&self, vm: &mut VirtualMachine) {
        (self.behavior)(vm)
    }
}
