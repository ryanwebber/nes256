use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Register<T = u8>(pub T);

impl<T> Register<T> {
    pub fn load(&mut self, value: T) {
        self.0 = value;
    }

    pub fn load_with(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.0);
    }

    pub fn value(&self) -> T
    where
        T: Copy,
    {
        self.0
    }
}

impl<T> Deref for Register<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Register<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
