#[macro_export]
macro_rules! as_prim {
    ($ref:expr, $variant:ident) => {
        if let $crate::dinobj::DinObject::$variant(v) = $ref.as_ref() {
            Some(v)
        } else {
            None
        }
    };
}

#[macro_export]
macro_rules! as_ext {
    ($ref:expr, $ty:ident) => {{
        let raw = $crate::dinobj_utils::as_prim!($ref, Extended);
        if let Some(v) = raw {
            let ptr = (*v);
            let tn = (unsafe { &*ptr }).type_name();
            if tn != $ty::EXPECTED_TYPE_NAME {
                None
            } else {
                let ptr = ptr as *const $ty;
                Some(unsafe { &*ptr })
            }
        } else {
            None
        }
    }};
} // todo this should return a malformedbytecode in all usages

#[macro_export]
macro_rules! catch{
    ($e:expr) => {
        match $e{
            Ok(v) => v,
            Err(e) => return Ok(Err(e))
        }
    };
}

pub use as_ext;
pub use as_prim;
