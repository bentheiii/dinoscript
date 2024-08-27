use std::borrow::Cow;

#[derive(Debug)]
pub(crate) enum Command<'s>{
    PopToCell(usize), // POP

    PushInt(i64), // PIC
    PushFloat(f64), // PFC
    PushBool(bool), // PBC
    PushString(Cow<'s, str>), // PSC
    PushFromCell(usize), // PCL
    PushFromSource(PushFromSource), // PSR
    PushFromCapture(usize), // PCP
    PushGlobal(usize), // PGL
    PushTail(usize), // PT-

    MakeFunction(MakeFunction<'s>), // MFN
    MakePending(usize), // MPE

    Attr(usize), // ATR
    Struct(usize), // STR
    Variant(usize), // VAR
    VariantOpt(usize), // VOP
}

#[derive(Debug)]
pub(crate) struct PushFromSource{
    pub(crate) source: usize,
    pub(crate) id: usize,
}

#[derive(Debug)]
pub(crate) struct MakeFunction<'s>{
    pub(crate) n_captures: usize,
    pub(crate) n_cells: usize,
    pub(crate) commands: Vec<Command<'s>>,
}