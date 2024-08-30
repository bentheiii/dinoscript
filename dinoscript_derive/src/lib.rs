use proc_macro::TokenStream as TokenStream1;

#[proc_macro_attribute]
pub fn dinopack_items(attr: TokenStream1, item: TokenStream1) -> TokenStream1 {
    println!("attr: {:?}", attr);
    println!("item: {:?}", item);
    item
}
