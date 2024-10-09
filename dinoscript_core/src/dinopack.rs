use crate::{bytecode::SourceId, compilation_scope::CompilationScope, runtime::RuntimeFrame};

pub trait DinoPack {
    type Builtins<'s>;

    fn setup_compiler<'p, 's>(&self, scope: &mut CompilationScope<'p, 's, Self::Builtins<'s>>);
    fn setup_runtime(&self, frame: &mut RuntimeFrame);
}

pub mod utils {
    use std::{borrow::Cow, collections::HashMap, sync::Arc};

    use crate::{
        ast::statement::Stmt, bytecode::SourceId, compilation_scope::{ty::{Generic, GenericSetId, Ty}, CompilationScope, Overload, OverloadArg, OverloadGenericParams, OverloadLoc, OverloadResolve, SystemLoc}, dinobj::{AllocatedRef, DinObject, SourceFnFunc, UserFn}, errors::RuntimeViolation, runtime::Runtime
    };

    pub enum SetupItem<'s, C> {
        Function(SetupFunction<'s, C>),
    }

    impl<'s, C> SetupItem<'s, C> {
        pub fn push_to_compilation<'p>(
            self,
            scope: &mut CompilationScope<'p, 's, C>,
            source: SourceId,
            mut id_generator: impl FnMut() -> usize,
        ) {
            match self {
                SetupItem::Function(f) => {
                    let (name, overload) = f.to_overload(scope.builtins.as_ref().unwrap().as_ref(), SystemLoc::new(source, id_generator()));
                    scope.add_overload(name, overload);
                }
            }
        }

        pub fn to_dinobject(self, runtime: &Runtime<'s>) -> Result<AllocatedRef<'s>, RuntimeViolation> {
            match self {
                SetupItem::Function(f) => f.to_dinobject(runtime),
            }
        }
    }

    pub struct SetupFunction<'s, C> {
        pub sig: fn(&C)->Signature<'s>,
        pub body: SetupFunctionBody<'s>,
    }

    impl<'s, C> SetupFunction<'s, C> {
        pub fn new(sig: fn(&C)->Signature<'s>, body: SetupFunctionBody<'s>) -> Self {
            Self { sig, body }
        }

        pub fn to_overload(self, c: &C, loc: SystemLoc) -> (Cow<'s, str>, Overload<'s>) {
            let sig = (self.sig)(c);
            (sig.name.clone(), sig.to_overload(loc))
        }

        pub fn to_dinobject(self, runtime: &Runtime<'s>) -> Result<AllocatedRef<'s>, RuntimeViolation> {
            Ok(runtime.allocate(Ok(
                match self.body {
                    SetupFunctionBody::System(f) => DinObject::SourceFn(f),
                    SetupFunctionBody::User(f) => DinObject::UserFn(f),
                }
            ))?.unwrap())
        }
    }

    pub struct SignatureGen<'s> {
        id: GenericSetId,
        generic_params: Vec<Cow<'s, str>>,
        gen_tys: HashMap<Cow<'s, str>, Arc<Ty<'s>>>,
    }

    impl<'s> SignatureGen<'s> {
        pub fn new(generic_params: Vec<impl Into<Cow<'s, str>>>) -> Self {
            let id = GenericSetId::unique();
            let generic_params: Vec<_> = generic_params.into_iter().map(|s| s.into()).collect();
            let gen_tys = generic_params.iter().enumerate().map(|(i, k)| (k.clone(), Arc::new(Ty::Generic(Generic::new(i, id))))).collect();
            Self { id, generic_params,  gen_tys}
        }

        pub fn get_gen(&self, name: &str) -> Arc<Ty<'s>> {
            match self.gen_tys.get(name) {
                None => panic!("generic type {} not found (found: {:?})", name, self.generic_params),
                Some(ty) => ty.clone(),
            }
        }
    }

    pub struct Signature<'s> {
        name: Cow<'s, str>,
        generic: Option<SignatureGen<'s>>,
        args: Vec<Arg<'s>>,
        ret: Arc<Ty<'s>>,
    }

    impl<'s> Signature<'s> {
        pub fn new(name: impl Into<Cow<'s, str>>, args: Vec<Arg<'s>>, ret: Arc<Ty<'s>>) -> Self {
            Self { name: name.into(), generic:None, args, ret }
        }

        pub fn new_generic(name: impl Into<Cow<'s, str>>, args: Vec<Arg<'s>>, ret: Arc<Ty<'s>>, generic: SignatureGen<'s>) -> Self {
            Self { name: name.into(), generic:Some(generic), args, ret }
        }

        pub fn to_overload(&self, loc: SystemLoc) -> Overload<'s> {
            let gen_params = self.generic.as_ref().map(|gen| OverloadGenericParams::new(gen.id, gen.generic_params.clone()));
            Overload::new(gen_params, self.args.iter().map(Arg::to_overload_arg).collect(), self.ret.clone(), OverloadLoc::System(loc))
        }

        pub fn from_statement(stmt: &Stmt<'s>)->Self{
            let Stmt::Fn(func) = stmt else {
                panic!("expected function statement, got {:?}", stmt);
            };
            todo!()
        }
    }

    pub struct Arg<'s> {
        name: Cow<'s, str>,
        ty: Arc<Ty<'s>>,
        default: Option<ArgDefault<'s>>,
    }

    pub enum ArgDefault<'s> {
        Resolve(Cow<'s, str>),
    }

    impl<'s> Arg<'s> {
        pub fn new(name: impl Into<Cow<'s, str>>, ty: Arc<Ty<'s>>) -> Self {
            Self { name: name.into(), ty, default: None }
        }

        pub fn with_default_resolve(name: impl Into<Cow<'s, str>>, ty: Arc<Ty<'s>>, default: impl Into<Cow<'s, str>>) -> Self {
            Self { name: name.into(), ty, default: Some(ArgDefault::Resolve(default.into())) }
        }

        pub fn name(&self) -> &Cow<'s, str> {
            &self.name
        }

        pub fn to_overload_arg(&self) -> OverloadArg<'s> {
            OverloadArg {
                ty: self.ty.clone(),
                default: match &self.default {
                    Some(ArgDefault::Resolve(name)) => Some(crate::compilation_scope::OverloadArgDefault::OverloadResolve(OverloadResolve::new(name.clone()))),
                    None => None,
                },
            }
        }
    }

    pub enum SetupFunctionBody<'s> {
        System(SourceFnFunc),
        User(UserFn<'s>),
    }
}
