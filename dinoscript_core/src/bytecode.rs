use std::borrow::Cow;

#[derive(Debug)]
pub enum Command<'s> {
    PopToCell(usize), // POP
    EvalTop,              // EVL

    PushInt(i64),                   // PIC
    PushFloat(f64),                 // PFC
    PushBool(bool),                 // PBC
    PushString(Cow<'s, str>),       // PSC
    PushFromCell(usize),            // PCL
    PushFromSource(PushFromSource), // PSR
    PushFromCapture(usize),         // PCP
    PushGlobal(usize),              // PGL
    PushTail(usize),                // PT-

    MakeFunction(MakeFunction<'s>), // MFN
    MakePending(usize),             // MPE

    Attr(usize),       // ATR
    Struct(usize),     // STR
    Variant(usize),    // VAR
    VariantOpt(usize), // VOP
}

pub type SourceId = &'static str;

#[derive(Debug)]
pub struct PushFromSource {
    pub source: SourceId,
    pub id: usize,
}

#[derive(Debug)]
pub struct MakeFunction<'s> {
    pub n_captures: usize,
    pub n_cells: usize,
    pub commands: Vec<Command<'s>>,
}
