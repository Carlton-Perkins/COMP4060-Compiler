pub trait Interp {
    type Env;
    type Output;

    fn interp(&self, env: &Self::Env) -> Self::Output;
}

pub trait InterpMut {
    type Env;
    type Output;

    fn interp(&self) -> Self::Output;
    fn interp_(&self, env: &mut Self::Env) -> Self::Output;
}

pub trait Opt {
    type Env;

    fn opt(&self) -> Self;
    fn opt_(&self, env: &Self::Env) -> Self;
}

pub trait IsPure {
    fn is_pure(&self) -> bool;
}

pub trait Emit {
    fn emit(&self) -> String;
}
