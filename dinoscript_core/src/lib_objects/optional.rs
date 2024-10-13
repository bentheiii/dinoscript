pub mod tag {
    use crate::dinobj::VariantTag;

    pub const NONE: VariantTag = VariantTag::new(1);
    pub const SOME: VariantTag = VariantTag::new(0);
}
