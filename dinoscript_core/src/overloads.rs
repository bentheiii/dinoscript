use std::{rc::Rc, sync::Arc};

use crate::compilation_scope::ty::{Generic, GenericSetId, Specialized, Ty};

pub struct BindingResolution<'s> {
    gen_id: Option<GenericSetId>,
    pub bound_generics: Vec<Option<Arc<Ty<'s>>>>
}

impl<'s> BindingResolution<'s> {
    pub fn new(gen_id: Option<GenericSetId>, n_generics: usize) -> Self {
        BindingResolution {
            gen_id,
            bound_generics: vec![None; n_generics],
        }
    }

    pub fn primitive() -> Self {
        BindingResolution {
            gen_id: None,
            bound_generics: Vec::new(),
        }
    }

    pub fn assign(&mut self, assign_type: &Arc<Ty<'s>>, input_type: &Arc<Ty<'s>>) -> Result<(), ()> {
        match assign_type.as_ref(){
            Ty::Specialized(Specialized{template: assign_template, args: assign_args}) => {
                if let Ty::Specialized(Specialized{template: inp_template, args: inp_args}) = input_type.as_ref(){
                    // TODO: we should give each template an id to make sure they are equal, for now, we just compare the pointers
                    if !Arc::ptr_eq(assign_template, inp_template){
                        return  Err(());
                    }

                    assert_eq!(assign_args.len(), inp_args.len());
                    assign_args.iter().zip(inp_args.iter()).map(|(a,i)| self.assign(a, i)).collect()
                } else {
                    Err(())
                }
            },
            Ty::Tuple(assign_tup) => {
                if let Ty::Tuple(inp_tup) = input_type.as_ref(){
                    assign_tup.iter().zip(inp_tup.iter()).map(|(a,i)| self.assign(a,i)).collect()
                } else {
                    Err(())
                }
            }
            Ty::Fn(assign_fn) => todo!(),
            Ty::Generic(Generic {idx, gen_id}) if self.gen_id.is_some_and(|gid| gen_id == gen_id)  => {
                if let Some(bound_type) = &self.bound_generics[*idx]{
                    self.bound_generics[*idx] = Some(combine_types(bound_type, input_type)?);
                    Ok(())
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

fn combine_types<'s>(a: &Arc<Ty<'s>>, b: &Arc<Ty<'s>>) -> Result<Arc<Ty<'s>>, ()>{
    match (a.as_ref(),b.as_ref()) {
        (Ty::Specialized(a_spec), Ty::Specialized(b_spec)) => {
            let a_templ = &a_spec.template;
            let b_templ = &b_spec.template;
            if Arc::ptr_eq(a_templ, b_templ){
                if a_spec.args.len() != b_spec.args.len(){
                    unreachable!()
                }
                if a_spec.args.len() == 0{
                    return Ok(a.clone())
                }
                let args = a_spec.args.iter().zip(b_spec.args.iter()).map(|(a,b)| combine_types(a,b)).collect::<Result<Vec<_>,_>>()?;
                Ok(Arc::new(Ty::Specialized(Specialized{template: a_templ.clone(), args})))
            } else {
                Err(())
            }
        }
        (Ty::Tuple(a_tup), Ty::Tuple(b_tup)) => {
            if a_tup.len() != b_tup.len(){
                return Err(())
            }
            if a_tup.len() == 0{
                return Ok(a.clone())
            }
            let tup = a_tup.iter().zip(b_tup.iter()).map(|(a,b)| combine_types(a,b)).collect::<Result<Vec<_>,_>>()?;
            Ok(Arc::new(Ty::Tuple(tup)))
        }
        (Ty::Fn(a_fn), Ty::Fn(b_fn)) => {
            todo!()
        }
        (Ty::Generic(a_gen), Ty::Generic(b_gen)) => {
            dbg!(a_gen, b_gen);
            if a_gen.idx == b_gen.idx && a_gen.gen_id == b_gen.gen_id{
                Ok(a.clone())
            } else {
                Err(())
            }
        }
        (Ty::Tail, Ty::Tail) => Ok(a.clone()),
        _ => Err(())
    }
}