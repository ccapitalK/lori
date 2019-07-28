use std::marker::PhantomData;

//TODO: Write and use this, instead of reference counting

#[derive(Debug)]
pub struct GarbageCollector {}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {}
    }
}

pub struct Gc<T> {
    _phantom_data: PhantomData<T>,
}
