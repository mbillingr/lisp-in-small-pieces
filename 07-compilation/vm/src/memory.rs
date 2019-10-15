use crate::types::{Scm, ScmBoxedValue};

thread_local! {
    pub static PAIR_ALLOCATOR: allocator::Allocator<(Scm, Scm)> = allocator::Allocator::new();
    pub static VALUE_ALLOCATOR: allocator::Allocator<ScmBoxedValue> = allocator::Allocator::new();
}

#[cfg(not(feature = "batch-alloc"))]
mod allocator {
    use std::marker::PhantomData;

    pub type Allocator<T> = BoxAllocator<T>;

    pub struct BoxAllocator<T: Copy>(PhantomData<T>);

    impl<T: Copy> BoxAllocator<T> {
        pub fn new() -> Self {
            BoxAllocator(PhantomData)
        }

        pub fn alloc(&self, value: T) -> &'static mut T {
            Box::leak(Box::new(value))
        }
    }
}

#[cfg(feature = "batch-alloc")]
mod allocator {
    use std::cell::UnsafeCell;
    use std::os::raw::c_void;

    #[link(name = "gc", kind = "static")]
    extern "C" {
        fn GC_malloc_many(n_bytes: usize) -> *mut c_void;
    }

    pub type Allocator<T> = BatchAllocator<T>;

    pub struct BatchAllocator<T: Copy> {
        free_list: UnsafeCell<*mut T>,
    }

    impl<T: Copy> BatchAllocator<T> {
        pub fn new() -> Self {
            assert!(std::mem::size_of::<T>() >= std::mem::size_of::<usize>());
            BatchAllocator {
                free_list: UnsafeCell::new(0 as *mut _),
            }
        }

        pub fn alloc(&self, value: T) -> &'static mut T {
            unsafe {
                let x = self.alloc_uninit();
                std::ptr::write(x, value);
                &mut *x
            }
        }

        unsafe fn alloc_uninit(&self) -> *mut T {
            self.ensure_list();
            self.next_item()
        }

        unsafe fn ensure_list(&self) {
            if *self.free_list.get() == std::mem::transmute(0_usize) {
                let memory = GC_malloc_many(std::mem::size_of::<T>());
                *self.free_list.get() = std::mem::transmute(memory);
                assert_ne!(*self.free_list.get(), std::mem::transmute(0_usize));
            }
        }

        unsafe fn next_item(&self) -> *mut T {
            // C macro: #define GC_NEXT(p) (*(void * *)(p))
            let head = self.free_list.get();
            let item = *head;
            let next_item = *(item as *mut *mut T);
            *head = next_item;
            item
        }
    }
}
