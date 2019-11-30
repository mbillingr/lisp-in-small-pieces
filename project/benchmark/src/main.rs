mod bench;
mod scm_boxed;
mod scm_enum;

use bench::run_benchmark;

trait DType: Sized {
    fn int(i: i64) -> Self;
    fn flt(f: f64) -> Self;
    fn vec(v: Vec<Self>) -> Self;

    fn as_int(&self) -> Option<i64>;
    fn as_flt(&self) -> Option<f64>;
    fn as_vec(&self) -> Option<&Vec<Self>>;

    fn is_int(&self) -> bool {
        self.as_int().is_some()
    }
    fn is_flt(&self) -> bool {
        self.as_flt().is_some()
    }
    fn is_vec(&self) -> bool {
        self.as_vec().is_some()
    }

    fn eq(&self, other: &Self) -> bool;
    fn lt(&self, other: &Self) -> bool;

    fn add(&self, rhs: &Self) -> Self {
        if let (Some(a), Some(b)) = (self.as_int(), rhs.as_int()) {
            return Self::int(a + b);
        }

        if let (Some(a), Some(b)) = (self.as_flt(), rhs.as_flt()) {
            return Self::flt(a + b);
        }

        panic!("Invalid add operands")
    }

    fn sub(&self, rhs: &Self) -> Self {
        if let (Some(a), Some(b)) = (self.as_int(), rhs.as_int()) {
            return Self::int(a - b);
        }

        if let (Some(a), Some(b)) = (self.as_flt(), rhs.as_flt()) {
            return Self::flt(a - b);
        }

        panic!("Invalid sub operands")
    }

    fn mul(&self, rhs: &Self) -> Self {
        if let (Some(a), Some(b)) = (self.as_int(), rhs.as_int()) {
            return Self::int(a * b);
        }

        if let (Some(a), Some(b)) = (self.as_flt(), rhs.as_flt()) {
            return Self::flt(a * b);
        }

        panic!("Invalid mul operands")
    }

    fn div(&self, rhs: &Self) -> Self {
        if let (Some(a), Some(b)) = (self.as_int(), rhs.as_int()) {
            return Self::int(a / b);
        }

        if let (Some(a), Some(b)) = (self.as_flt(), rhs.as_flt()) {
            return Self::flt(a / b);
        }

        panic!("Invalid div operands")
    }
}

fn factorial<T: DType + std::fmt::Debug>(mut n: T) -> T {
    let mut acc = T::int(1);
    let zero = T::int(0);
    let one = T::int(1);
    while zero.lt(&n) {
        acc = acc.mul(&n);
        n = n.sub(&one);
    }
    acc
}

fn fib1(n: i64) -> i64 {
    if n < 2 {
        1
    } else {
        fib1(n - 1) + fib1(n - 2)
    }
}

fn fib2(n: i64) -> i64 {
    let mut a = 1;
    let mut b = 1;
    for _ in 1..n {
        let c = a + b;
        a = b;
        b = c;
    }
    b
}

fn fib3(n: i64) -> i64 {
    if n < 2 {
        1
    } else {
        fib3(n - 1) + fib2(n - 2)
    }
}

fn f1(n: i64) -> i64 {
    factorial(n)
}

fn f2(n: i64) -> i64 {
    factorial(scm_enum::Scm::int(n)).as_int().unwrap()
}

fn f3(n: i64) -> i64 {
    factorial(scm_boxed::Scm::int(n)).as_int().unwrap()
}

impl DType for i64 {
    fn int(i: i64) -> Self {
        i
    }
    fn flt(f: f64) -> Self {
        unimplemented!()
    }
    fn vec(v: Vec<Self>) -> Self {
        unimplemented!()
    }

    fn as_int(&self) -> Option<i64> {
        Some(*self)
    }

    fn as_flt(&self) -> Option<f64> {
        None
    }

    fn as_vec(&self) -> Option<&Vec<Self>> {
        None
    }

    fn eq(&self, other: &Self) -> bool {
        self == other
    }

    fn lt(&self, other: &Self) -> bool {
        self < other
    }
}

fn main() {
    //run_benchmark(10000, 0..20, &[("fib1", &fib1), ("fib2", &fib2), ("fib3", &fib3)]).plot().unwrap();
    run_benchmark(
        100,
        0..21,
        &[
            (
                format!("enum ({} bytes)", std::mem::size_of::<scm_enum::Scm>()),
                &f2,
            ),
            (
                format!("boxed ({} bytes)", std::mem::size_of::<scm_boxed::Scm>()),
                &f3,
            ),
            (
                format!("raw int ({} bytes)", std::mem::size_of::<i64>()),
                &f1,
            ),
        ],
    )
    .plot()
    .unwrap();
}
