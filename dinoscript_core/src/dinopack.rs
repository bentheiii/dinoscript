use crate::{bytecode::SourceId, compilation_scope::CompilationScope, runtime::RuntimeFrame};

pub trait DinoPack {
    type Builtins<'s>;

    fn setup_compiler<'p, 's>(&self, scope: &mut CompilationScope<'p, 's, Self::Builtins<'s>>);
    fn setup_runtime(&self, frame: &mut RuntimeFrame);
}

pub mod utils {
    use std::{borrow::Cow, sync::Arc};

    use crate::{
        bytecode::SourceId, compilation_scope::{ty::{GenericSetId, Ty}, CompilationScope, Overload, OverloadArg, OverloadGenericParams, OverloadLoc, SystemLoc}, dinobj::{AllocatedRef, DinObject, SourceFnFunc, UserFn}, errors::RuntimeViolation, runtime::Runtime
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
    }

    impl<'s> SignatureGen<'s> {
        pub fn new(id: GenericSetId, generic_params: Vec<Cow<'s, str>>) -> Self {
            Self { id, generic_params }
        }
    }

    pub struct Signature<'s> {
        name: Cow<'s, str>,
        generic: Option<SignatureGen<'s>>,
        args: Vec<Arg<'s>>,
        ret: Arc<Ty<'s>>,
    }

    impl<'s> Signature<'s> {
        pub fn new(name: Cow<'s, str>, generic: Option<SignatureGen<'s>>, args: Vec<Arg<'s>>, ret: Arc<Ty<'s>>) -> Self {
            Self { name, generic, args, ret }
        }

        pub fn to_overload(&self, loc: SystemLoc) -> Overload<'s> {
            let gen_params = self.generic.as_ref().map(|gen| OverloadGenericParams::new(gen.id, gen.generic_params.clone()));
            Overload::new(gen_params, self.args.iter().map(Arg::to_overload_arg).collect(), self.ret.clone(), OverloadLoc::System(loc))
        }
    }

    pub struct Arg<'s> {
        name: Cow<'s, str>,
        ty: Arc<Ty<'s>>,
        // todo default
    }

    impl<'s> Arg<'s> {
        pub fn new(name: Cow<'s, str>, ty: Arc<Ty<'s>>) -> Self {
            Self { name, ty }
        }

        pub fn name(&self) -> &Cow<'s, str> {
            &self.name
        }

        pub fn to_overload_arg(&self) -> OverloadArg<'s> {
            OverloadArg {
                ty: self.ty.clone(),
                default: None,
            }
        }
    }

    pub enum SetupFunctionBody<'s> {
        System(SourceFnFunc),
        User(UserFn<'s>),
    }
}
