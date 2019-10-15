use crate::types::{Primitive, Scm};

macro_rules! defprimitive {
    ($name:ident() $body:block) => {
        defprimitive!(stringify!($name), $body)
    };
    ($name:ident($a:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $body)
    };
    ($name:ident($a:ident, $b:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $b, $body)
    };
    ($name:ident($a:ident, $b:ident, $c:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $b, $c, $body)
    };

    ($name:expr, $body:block) => {{
        Scm::primitive(Primitive::new($name, |vm| {
            let frame = vm.val.as_frame().unwrap();
            if frame.len() == 0 + 1 {
                vm.val = $body;
                unsafe {
                    vm.pc = vm.stack_pop_into();
                }
            } else {
                vm.signal_exception_str(false, concat!("incorrect arity: ", $name))
            }
        }))
    }};

    ($name:expr, $a:ident, $body:block) => {{
        Scm::primitive(Primitive::new($name, |vm| {
            let frame = vm.val.as_frame().unwrap();
            if frame.len() == 1 + 1 {
                let $a = *frame.argument(0);
                vm.val = $body;
                unsafe {
                    vm.pc = vm.stack_pop_into();
                }
            } else {
                vm.signal_exception_str(false, concat!("incorrect arity: ", $name))
            }
        }))
    }};

    ($name:expr, $a:ident, $b:ident, $body:block) => {{
        Scm::primitive(Primitive::new($name, |vm| {
            let frame = vm.val.as_frame().unwrap();
            if frame.len() == 2 + 1 {
                let $a = *frame.argument(0);
                let $b = *frame.argument(1);
                vm.val = $body;
                unsafe {
                    vm.pc = vm.stack_pop_into();
                }
            } else {
                vm.signal_exception_str(false, concat!("incorrect arity: ", $name))
            }
        }))
    }};

    ($name:expr, $a:ident, $b:ident, $c:ident, $body:block) => {{
        Scm::primitive(Primitive::new($name, |vm| {
            let frame = vm.val.as_frame().unwrap();
            if frame.len() == 2 + 1 {
                let $a = *frame.argument(0);
                let $b = *frame.argument(1);
                let $c = *frame.argument(1);
                vm.val = $body;
                unsafe {
                    vm.pc = vm.stack_pop();
                }
            } else {
                vm.signal_exception(concat!("incorrect arity: ", $name))
            }
        }))
    }};
}

pub fn init_predefined() -> Vec<Scm> {
    vec![
        Scm::bool(true),
        Scm::bool(false),
        Scm::null(),
        defprimitive!(cons(car, cdr) { Scm::cons(car, cdr) }),
        defprimitive!(car(x) { x.as_pair().unwrap().0 }),
        defprimitive!(cdr(x) { x.as_pair().unwrap().1 }),
        defprimitive!("pair?", x, { Scm::bool(x.is_pair()) }),
        defprimitive!("symbol?", x, { Scm::bool(x.is_symbol()) }),
        defprimitive!("eq?", a, b, { Scm::bool(a.eq(&b)) }),
        defprimitive!("null?", x, { Scm::bool(x.is_null()) }),
        defprimitive!("set-car!", x, a, {
            unsafe {
                x.set_car_unchecked(a);
            }
            Scm::uninitialized()
        }),
        defprimitive!("set-cdr!", x, d, {
            unsafe {
                x.set_cdr_unchecked(d);
            }
            Scm::uninitialized()
        }),
    ]
}
