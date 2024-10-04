use std::{rc::Rc, sync::Arc};

use crate::compilation_scope::ty::{Generic, GenericSetId, Specialized, Ty};

pub struct BindingResolution<'s> {
    gen_id: Option<GenericSetId>,
    bound_generics: Vec<Option<Arc<Ty<'s>>>>
}

impl<'s> BindingResolution<'s> {
    pub fn new(gen_id: Option<GenericSetId>, n_generics: usize) -> Self {
        BindingResolution {
            gen_id,
            bound_generics: vec![None; n_generics],
        }
    }

    pub fn assign(&mut self, assign_type: &Arc<Ty<'s>>, input_type: &Arc<Ty<'s>>) -> Result<(), ()> {
        match assign_type.as_ref(){
            Ty::Specialized(Specialized{template: ass_template, args: ass_args}) => {
                if let Ty::Specialized(Specialized{template: inp_template, args: inp_args}) = input_type.as_ref(){
                    // TODO: we should give each template an id to make sure they are equal, for now, we just compare the pointers
                    if !Arc::ptr_eq(ass_template, inp_template){
                        return  Err(());
                    }

                    assert_eq!(ass_args.len(), inp_args.len());
                    ass_args.iter().zip(inp_args.iter()).map(|(a,i)| self.assign(a, i)).collect()
                } else {
                    Err(())
                }
            },
            Ty::Tuple(ass_tup) => {
                if let Ty::Tuple(inp_tup) = input_type.as_ref(){
                    ass_tup.iter().zip(inp_tup.iter()).map(|(a,i)| self.assign(a,i)).collect()
                } else {
                    Err(())
                }
            }
            Ty::Fn(ass_fn) => todo!(),
            Ty::Generic(Generic {idx, gen_id}) if self.gen_id.is_some_and(|gid| gen_id == gen_id)  => {
                if let Some(bound_type) = &self.bound_generics[*idx]{
                    todo!() // assert that the two types are equal
                } else {
                    self.bound_generics[*idx] = Some(input_type.clone());
                    Ok(())
                }
            }
            Ty::Generic(_) =>{
                todo!()
            }
            Ty::Tail => unreachable!()
        }
    }
}