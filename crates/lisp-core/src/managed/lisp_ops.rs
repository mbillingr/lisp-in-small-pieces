use super::{LispData, LispResult};

pub trait LispOps: Sized {
    fn caar(&self) -> LispResult<Self>;
    fn cadr(&self) -> LispResult<Self>;
    fn cdar(&self) -> LispResult<Self>;
    fn cddr(&self) -> LispResult<Self>;

    fn caaar(&self) -> LispResult<Self>;
    fn caadr(&self) -> LispResult<Self>;
    fn cadar(&self) -> LispResult<Self>;
    fn caddr(&self) -> LispResult<Self>;
    fn cdaar(&self) -> LispResult<Self>;
    fn cdadr(&self) -> LispResult<Self>;
    fn cddar(&self) -> LispResult<Self>;
    fn cdddr(&self) -> LispResult<Self>;

    fn cadddr(&self) -> LispResult<Self>;
}

impl<T: LispData> LispOps for T {
    fn caar(&self) -> LispResult<Self> {
        self.car()?.car()
    }

    fn cadr(&self) -> LispResult<Self> {
        self.cdr()?.car()
    }

    fn cdar(&self) -> LispResult<Self> {
        self.car()?.cdr()
    }

    fn cddr(&self) -> LispResult<Self> {
        self.cdr()?.cdr()
    }

    fn caaar(&self) -> LispResult<Self> {
        self.caar()?.car()
    }

    fn caadr(&self) -> LispResult<Self> {
        self.cdar()?.car()
    }

    fn cadar(&self) -> LispResult<Self> {
        self.cadr()?.car()
    }

    fn caddr(&self) -> LispResult<Self> {
        self.cddr()?.car()
    }

    fn cdaar(&self) -> LispResult<Self> {
        self.caar()?.cdr()
    }

    fn cdadr(&self) -> LispResult<Self> {
        self.cdar()?.cdr()
    }

    fn cddar(&self) -> LispResult<Self> {
        self.cadr()?.cdr()
    }

    fn cdddr(&self) -> LispResult<Self> {
        self.cddr()?.cdr()
    }

    fn cadddr(&self) -> LispResult<Self> {
        self.cddr()?.cadr()
    }
}
