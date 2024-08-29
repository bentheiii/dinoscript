use crate::{compilation_scope::CompilationScope, runtime::RuntimeFrame};

pub trait DinoPack {
    fn setup_compiler_with_id(&self, scope: &mut CompilationScope, source_id: usize);
    fn setup_runtime_with_id(&self, frame: &mut RuntimeFrame){
        todo!()
    }

    fn setup_compiler<'p,'s>(&self, scope: &mut CompilationScope<'p,'s>){
        let source_id = scope.get_source();
        self.setup_compiler_with_id(scope, source_id);
    }
}

pub(crate) mod utils{
    use std::{borrow::Cow, sync::Arc};

    use crate::{compilation_scope::{ty::Ty, CompilationScope, Overload, OverloadArg, OverloadLoc, SystemLoc}, dinobj::{DinObject, SourceFn, UserFn}};

    pub(crate) enum SetupItem<'s, C>{
        Function(SetupFunction<'s, C>),
    }

    impl <'s, C> SetupItem<'s, C>{
        pub(crate) fn push_to_compilation(&self, scope: &mut CompilationScope<'_, 's>, c: C, source: usize, mut id_generator: impl FnMut()->usize){
            match self{
                SetupItem::Function(f) => {
                    let (name, overload) = f.to_overload(c, SystemLoc{source, id: id_generator()});
                    scope.add_overload(name, overload);
                }
            }
        }
    }
            
    
    pub(crate) struct SetupFunction<'s, C>{
        pub(crate) sig: fn(C) -> Signature<'s>,
        pub(crate) body: SetupFunctionBody<'s>,
    }

    impl <'s, C> SetupFunction<'s, C>{
        pub(crate) fn new(sig: fn(C) -> Signature<'s>, body: SetupFunctionBody<'s>)->Self{
            Self{
                sig,
                body,
            }
        }

        pub(crate) fn to_overload(&self, c: C, loc: SystemLoc)->(Cow<'s,str>,Overload<'s>){
            let sig = (self.sig)(c);
            (sig.name.clone(), sig.to_overload(loc))
        }

        pub(crate) fn to_dinobject(self)->Arc<DinObject<'s>>{
            match self.body{
                SetupFunctionBody::System(f) => Arc::new(DinObject::SourceFn(f)),
                SetupFunctionBody::User(f) => Arc::new(DinObject::UserFn(f)),
            }
        }
    }
    
    pub(crate) struct Signature<'s>{
        name: Cow<'s, str>,
        // todo generics
        args: Vec<Arg<'s>>,
        ret: Arc<Ty<'s>>,
    }
    
    impl<'s> Signature<'s>{
        pub(crate) fn new(name: Cow<'s, str>, args: Vec<Arg<'s>>, ret: Arc<Ty<'s>>)->Self{
            Self{
                name,
                args,
                ret,
            }
        }
    
        pub(crate) fn to_overload(&self, loc: SystemLoc)->Overload<'s>{
            Overload{
                generic_params: Vec::new(),
                args: self.args.iter().map(Arg::to_overload_arg).collect(),
                return_ty: self.ret.clone(),
                loc: OverloadLoc::System(loc)
            }
        }
    }
    
    pub(crate) struct Arg<'s>{
        name: Cow<'s, str>,
        ty: Arc<Ty<'s>>,
        // todo default
    }
    
    impl<'s> Arg<'s>{
        pub(crate) fn new(name: Cow<'s, str>, ty: Arc<Ty<'s>>)->Self{
            Self{
                name,
                ty,
            }
        }
    
        pub(crate) fn to_overload_arg(&self)->OverloadArg<'s>{
            OverloadArg{
                ty: self.ty.clone(),
                default: None,
            }
        }
    }
    
    pub(crate) enum SetupFunctionBody<'s>{
        System(SourceFn),
        User(UserFn<'s>)
    }
}