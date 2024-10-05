#[macro_export]
macro_rules! unique {
    ($v:vis struct $name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $v struct $name(usize);

        impl $name {
            $v fn unique() -> Self {
                static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
                Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
            }
        }
    };
}
