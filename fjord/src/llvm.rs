use std::ffi::CStr;
use std::marker::PhantomData;
use std::os::raw::c_char;
use std::os::raw::c_int;
use std::os::raw::c_uint;

#[link(name = "LLVM")]
extern
{
    fn LLVMAddFunction(M: *mut u8, Name: *const c_char, FunctionTy: *mut u8) -> *mut u8;
    fn LLVMAddGlobal(M: *mut u8, Ty: *mut u8, Name: *const c_char) -> *mut u8;
    fn LLVMConstNull(Ty: *mut u8) -> *mut u8;
    fn LLVMContextCreate() -> *mut u8;
    fn LLVMContextDispose(C: *mut u8);
    fn LLVMDisposeModule(M: *mut u8);
    fn LLVMDumpModule(M: *mut u8);
    fn LLVMFunctionType(ReturnType: *mut u8, ParamTypes: *mut *mut u8, ParamCount: c_uint, IsVarArg: c_int) -> *mut u8;
    fn LLVMIntTypeInContext(C: *mut u8, NumBits: c_uint) -> *mut u8;
    fn LLVMModuleCreateWithNameInContext(ModuleID: *const c_char, C: *mut u8) -> *mut u8;
    fn LLVMPointerType(ElementType: *mut u8, AddressSpace: c_uint) -> *mut u8;
    fn LLVMSetInitializer(GlobalVar: *mut u8, ConstantVal: *mut u8);
    fn LLVMStructCreateNamed(C: *mut u8, Name: *const c_char) -> *mut u8;
}

////////////////////////////////////////////////////////////////////////////////

pub struct context(*mut u8);

impl context
{
    pub fn new() -> Self
    {
        unsafe
        {
            let raw = LLVMContextCreate();
            context(raw)
        }
    }

    pub fn inttype(&self, numbits: u32) -> tipe
    {
        unsafe
        {
            let raw = LLVMIntTypeInContext(self.0, numbits);
            tipe(raw, PhantomData)
        }
    }

    pub fn namedstructtype(&self, name: &CStr) -> tipe
    {
        unsafe
        {
            let raw = LLVMStructCreateNamed(self.0, name.as_ptr());
            tipe(raw, PhantomData)
        }
    }
}

impl Drop for context
{
    fn drop(&mut self)
    {
        unsafe
        {
            LLVMContextDispose(self.0);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct module<'ctx>(*mut u8, PhantomData<&'ctx ()>);

impl<'ctx> module<'ctx>
{
    pub fn new(ctx: &'ctx context, name: &CStr) -> Self
    {
        unsafe
        {
            let raw = LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx.0);
            module(raw, PhantomData)
        }
    }

    pub fn addglobal(&self, name: &CStr, t1pe: tipe<'ctx>) -> value<'ctx>
    {
        unsafe
        {
            let raw = LLVMAddGlobal(self.0, t1pe.0, name.as_ptr());
            value(raw, PhantomData)
        }
    }

    pub fn addfunction(&self, name: &CStr, t1pe: tipe<'ctx>) -> value<'ctx>
    {
        unsafe
        {
            let raw = LLVMAddFunction(self.0, name.as_ptr(), t1pe.0);
            value(raw, PhantomData)
        }
    }

    pub fn dump(&self)
    {
        unsafe
        {
            LLVMDumpModule(self.0);
        }
    }
}

impl<'ctx> Drop for module<'ctx>
{
    fn drop(&mut self)
    {
        unsafe
        {
            LLVMDisposeModule(self.0);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct value<'ctx>(*mut u8, PhantomData<&'ctx ()>);

impl<'ctx> value<'ctx>
{
    pub fn setinitializer(self, v4lue: value<'ctx>)
    {
        unsafe
        {
            LLVMSetInitializer(self.0, v4lue.0);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct tipe<'ctx>(*mut u8, PhantomData<&'ctx ()>);

impl<'ctx> tipe<'ctx>
{
    pub fn nullconst(self) -> value<'ctx>
    {
        unsafe
        {
            let raw = LLVMConstNull(self.0);
            value(raw, PhantomData)
        }
    }

    pub fn pointertype(self) -> Self
    {
        unsafe
        {
            let raw = LLVMPointerType(self.0, 0);
            tipe(raw, PhantomData)
        }
    }

    pub fn functiontype(returntype: tipe<'ctx>, paramtypes: &[tipe<'ctx>]) -> tipe<'ctx>
    {
        unsafe
        {
            let raw = LLVMFunctionType(
                returntype.0,
                paramtypes.as_ptr() as *mut *mut u8,
                paramtypes.len() as c_uint,
                0,
            );
            tipe(raw, PhantomData)
        }
    }
}
