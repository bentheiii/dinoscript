use std::sync::Arc;

use crate::compilation_scope::ty::{Fn, Generic, GenericSetId, Specialized, Ty};

pub struct BindingResolution<'s> {
    gen_id: Option<GenericSetId>,
    pub bound_generics: Vec<Arc<Ty<'s>>>
}

impl<'s> BindingResolution<'s> {
    pub fn new(gen_id: Option<GenericSetId>, n_generics: usize) -> Self {
        BindingResolution {
            gen_id,
            bound_generics: vec![Ty::unknown(); n_generics],
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
                match input_type.as_ref(){
                    Ty::Specialized(Specialized{template: inp_template, args: inp_args}) => {
                        if !Arc::ptr_eq(assign_template, inp_template){
                            return  Err(());
                        }
        
                        assert_eq!(assign_args.len(), inp_args.len());
                        assign_args.iter().zip(inp_args.iter()).try_for_each(|(a,i)| self.assign(a, i))
                    }
                    Ty::Unknown => {
                        Ok(())
                    }
                    _ => Err(())
                }
            },
            Ty::Tuple(assign_tup) => {
                if let Ty::Tuple(inp_tup) = input_type.as_ref(){
                    assign_tup.iter().zip(inp_tup.iter()).try_for_each(|(a,i)| self.assign(a,i))
                } else {
                    Err(())
                }
            }
            Ty::Fn(assign_fn) => {
                if let Ty::Fn(inp_fn) = input_type.as_ref(){
                    if assign_fn.args.len() != inp_fn.args.len(){
                        return Err(())
                    }
                    assign_fn.args.iter().zip(inp_fn.args.iter()).map(|(a,i)| self.assign(a,i)).collect::<Result<Vec<_>,_>>()?;
                    self.assign(&assign_fn.return_ty, &inp_fn.return_ty)
                } else {
                    Err(())
                }
            }
            Ty::Generic(Generic {idx, gen_id}) if self.gen_id.is_some_and(|gid| gen_id == &gid)  => {
                let bound_type = &self.bound_generics[*idx];
                self.bound_generics[*idx] = combine_types(bound_type, input_type)?;
                Ok(())
            }
            Ty::Generic(_) =>{
                todo!()
            }
            Ty::Unknown => unreachable!(),
            Ty::Tail => unreachable!(),
        }
    }
}

pub fn combine_types<'s>(a: &Arc<Ty<'s>>, b: &Arc<Ty<'s>>) -> Result<Arc<Ty<'s>>, ()>{
    match (a.as_ref(),b.as_ref()) {
        (Ty::Specialized(a_spec), Ty::Specialized(b_spec)) => {
            let a_templ = &a_spec.template;
            let b_templ = &b_spec.template;
            if Arc::ptr_eq(a_templ, b_templ){
                if a_spec.args.len() != b_spec.args.len(){
                    unreachable!()
                }
                if a_spec.args.is_empty(){
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
            if a_tup.is_empty(){
                return Ok(a.clone())
            }
            let tup = a_tup.iter().zip(b_tup.iter()).map(|(a,b)| combine_types(a,b)).collect::<Result<Vec<_>,_>>()?;
            Ok(Arc::new(Ty::Tuple(tup)))
        }
        (Ty::Fn(a_fn), Ty::Fn(b_fn)) => {
            if a_fn.args.len() != b_fn.args.len(){
                return Err(())
            }
            let args = a_fn.args.iter().zip(b_fn.args.iter()).map(|(a,b)| combine_types(a,b)).collect::<Result<Vec<_>,_>>()?;
            let ret = combine_types(&a_fn.return_ty, &b_fn.return_ty)?;
            Ok(Arc::new(Ty::Fn(Fn::new(args, ret))))
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
        (Ty::Unknown, _) => Ok(b.clone()),
        (_, Ty::Unknown) => Ok(a.clone()),
        _ => Err(())
    }
}