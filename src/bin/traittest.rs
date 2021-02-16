struct ExprA {}

struct ExprB {}

struct EnvA {
    foo: isize,
}

struct EnvB {
    bar: usize,
}

trait FooBar {
    type Env;
    type Output;

    fn act(a: &Self::Env) -> Self::Output;
}

impl FooBar for ExprA {
    type Env = EnvA;
    type Output = i64;

    fn act(env: &Self::Env) -> Self::Output {
        env.foo as Self::Output
    }
}

impl FooBar for ExprB {
    type Env = EnvB;
    type Output = i64;

    fn act(env: &Self::Env) -> Self::Output {
        env.bar as Self::Output
    }
}

fn main() {}
