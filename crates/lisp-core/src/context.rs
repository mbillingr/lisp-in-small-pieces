use crate::storage::ManagedStorage;
use crate::{LispData, LispResult, Managable};
use rustyline::Editor;
use std::str::FromStr;

pub struct Context<T>
where
    T: Managable + LispData,
{
    storage: ManagedStorage<T>,
    roots: Vec<T>,
    symbols: Vec<T>,
}

impl<T: std::fmt::Debug> Context<T>
where
    T: Managable + LispData + From<i64>,
{
    pub fn new() -> Self {
        Context {
            storage: ManagedStorage::new(10000),
            roots: vec![],
            symbols: vec![],
        }
    }

    pub fn push(&mut self, value: T) {
        self.roots.push(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.roots.pop()
    }

    pub fn read(&mut self) -> LispResult<T> {
        let mut rl = Editor::<()>::new();
        match rl.readline(">> ") {
            Ok(line) => self.convert_lexpr(lexpr::Value::from_str(&line)?),
            Err(e) => Err(e.into()),
        }
    }

    pub fn convert_lexpr(&mut self, val: lexpr::Value) -> LispResult<T> {
        use lexpr::Value::*;
        Ok(match val {
            Nil | Null => T::nil(),
            Bool(b) => T::bool(b),
            Number(num) => {
                if let Some(i) = num.as_i64() {
                    i.into()
                } else {
                    unimplemented!("{:?}", num)
                }
            }
            Char(ch) => T::char(ch),
            String(s) => self.array(s.as_bytes()).string_from_array().unwrap(),
            Symbol(s) => self.symbol(&s),
            Keyword(s) => T::keyword(&s),
            Bytes(b) => self.array(&b),
            Cons(cons) => {
                let (car, cdr) = cons.into_pair();
                let car = self.convert_lexpr(car)?;
                self.push(car);
                let cdr = self.convert_lexpr(cdr)?;
                self.push(cdr);
                self.record(2)
            }
            Vector(values) => {
                for v in values.into_iter() {
                    let item = self.convert_lexpr(v.clone())?;
                    self.push(item);
                }
                self.record(values.len())
            }
        })
    }

    pub fn symbol(&mut self, s: &str) -> T {
        if let Some(sym) = self.symbols.iter().find(|p| p.equals_str(s)) {
            return *sym;
        }

        let sym = T::symbol(s);
        self.symbols.push(sym);
        sym
    }

    pub fn register_symbol(&mut self, sym: T) {
        assert!(sym.is_symbol());
        self.symbols.push(sym);
    }

    pub fn list(&mut self, items: &[T]) -> T {
        for x in items {
            self.push(*x);
        }
        self.push(T::nil());
        for _ in 0..items.len() {
            let y = self.record(2);
            self.push(y);
        }
        self.pop().unwrap()
    }

    pub fn cons(&mut self, car: T, cdr: T) -> T {
        self.push(car);
        self.push(cdr);
        self.record(2)
    }

    pub fn record(&mut self, size: usize) -> T {
        assert!(self.roots.len() >= size);
        self.ensure_heap_space(size);
        let rec = self.storage.make_record(size).expect("unable to allocate");
        for i in (0..size).rev() {
            let x = self.roots.pop().unwrap();
            rec.set_array_item(i, x).unwrap();
        }
        rec
    }

    pub fn array(&mut self, items: &[u8]) -> T {
        self.ensure_heap_space(items.len());
        self.storage.new_array(items).expect("unable to allocate")
    }

    pub fn ensure_heap_space(&mut self, size: usize) {
        if self.storage.free() < self.roots.len() + size
            && self.storage.capacity() > self.roots.len() + size
        {
            eprintln!("collecting...");
            let new_roots = unsafe { self.storage.collect_garbage(&self.roots, 0) };
            self.roots.clear();
            self.roots.extend(new_roots);
        }

        while self.storage.free() < self.roots.len() + size {
            eprintln!("extending...");
            let storage_extension = self.storage.capacity().max(size); // double the heap size
            let new_roots = unsafe { self.storage.collect_garbage(&self.roots, storage_extension) };
            self.roots.clear();
            self.roots.extend(new_roots);
        }
    }
}
