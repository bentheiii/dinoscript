pub mod tag {
    use crate::dinobj::VariantTag;

    pub const EMPTY: VariantTag = VariantTag::new(0);
    pub const NODE: VariantTag = VariantTag::new(1);
}