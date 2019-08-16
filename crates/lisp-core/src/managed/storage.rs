use super::Managable;

pub struct ManagedStorage<T>
where
    T: Managable,
{
    heap: Vec<T>,
    last_root_size: usize,
}

impl<T> ManagedStorage<T>
where
    T: Managable,
{
    pub fn new(size: usize) -> Self {
        ManagedStorage {
            heap: Vec::with_capacity(size),
            last_root_size: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.heap.capacity()
    }

    pub fn free(&self) -> usize {
        self.heap.capacity() - self.heap.len()
    }

    pub fn new_record(&mut self, items: &[T]) -> Option<T> {
        let mut data = self.alloc(items.len())?;
        let rec = Some(T::record(data, items.len()));
        for i in items {
            unsafe {
                *data = *i;
                data = data.offset(1);
            }
        }
        rec
    }

    pub fn make_record(&mut self, n: usize) -> Option<T> {
        self.alloc(n).map(|ptr| T::record(ptr, n))
    }

    pub fn new_array(&mut self, items: &[u8]) -> Option<T> {
        let k = std::mem::size_of::<T>();
        let n = (items.len() + k - 1) / k;
        let mut data = self.alloc(n)? as *mut u8;
        let arr = Some(T::array(data, items.len()));
        for i in items {
            unsafe {
                *data = *i;
                data = data.offset(1);
            }
        }
        arr
    }

    fn alloc(&mut self, n: usize) -> Option<*mut T> {
        let i = self.heap.len();

        // If the backing storage reallocates we're doomed.
        if self.heap.capacity() < i + n {
            return None;
        }

        self.heap.resize_with(i + n, Default::default);
        Some(&mut self.heap[i])
    }

    /// Collect garbage.
    /// This function is unsafe because it will invalidate all pointers to the heap.
    /// Objects reachable by the roots are preserved and their pointers updated. It
    /// is the responsibility of the caller to pass the roots required to reach all
    /// live objects. Otherwise there will be dangling pointers!
    pub unsafe fn collect_garbage(&mut self, roots: &[T], mut extend: usize) -> &[T] {
        let mem_used_before = self.heap.len();
        let capacity_before = self.heap.capacity();
        let mem_free_before = capacity_before - mem_used_before;

        if mem_free_before + self.last_root_size + extend < roots.len() {
            extend += roots.len();
        }
        self.last_root_size = roots.len();

        let mut to_space = vec![T::default(); self.heap.capacity() + extend];

        let mut scan_ptr = &mut to_space[0] as *mut _;
        let mut insert_ptr = &mut to_space[0] as *mut _;

        for root in roots {
            *insert_ptr = *root;
            insert_ptr = insert_ptr.offset(1);
        }

        while scan_ptr < insert_ptr {
            match *scan_ptr {
                slot if slot.is_record() => {
                    let r = slot.as_record().unwrap();
                    if !r.is_empty() {
                        if let Some(dst) = r[0].as_relocated() {
                            *scan_ptr = T::record(dst as *mut _, r.len());
                        } else {
                            *insert_ptr = r[0];
                            *scan_ptr = T::record(insert_ptr, r.len());
                            r[0] = T::relocated(insert_ptr);
                            insert_ptr = insert_ptr.offset(1);
                            for item in &r[1..] {
                                *insert_ptr = *item;
                                insert_ptr = insert_ptr.offset(1);
                            }
                        }
                    }
                }

                _ => {}
            }
            scan_ptr = scan_ptr.offset(1);
        }

        let start = &to_space[0] as *const _ as usize;
        let len = (scan_ptr as usize - start) / std::mem::size_of::<T>();
        to_space.truncate(len);

        std::mem::replace(&mut self.heap, to_space);

        let mem_used_after = self.heap.len();

        eprintln!(
            "{} collected, {} live, {} free, {} total",
            mem_used_before.saturating_sub(mem_used_after),
            mem_used_after,
            self.heap.capacity() - mem_used_after,
            self.heap.capacity()
        );

        &self.heap[..roots.len()]
    }
}
