use std::{
    fmt::Display,
    ops::{Add, Mul, Not},
    str::FromStr,
};

use itertools::Itertools;

pub type Variable = String;
pub type Number = i64;
pub type Label = String;
pub type Address = usize;
pub type AddressOffset = isize;

#[derive(Debug, Eq, PartialEq, Copy, Clone, PartialOrd, Ord)]
pub enum Answer {
    S64(Number),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum CMP {
    EQ,
    LT,
    LEQ,
    GEQ,
    GT,
}

impl Add for Answer {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Answer::S64(l), Answer::S64(r)) => Answer::S64(l + r),
            _ => panic!("Tried to add a {:?} and a {:?}", self, rhs),
        }
    }
}

impl Mul for Answer {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Answer::S64(l), Answer::S64(r)) => Answer::S64(l * r),
            _ => panic!("Tried to mult a {:?} and a {:?}", self, rhs),
        }
    }
}

impl Not for Answer {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Answer::Bool(b) => Answer::Bool(!b),
            _ => panic!("Tried to not a {:?}", self),
        }
    }
}
impl Display for Answer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Answer::S64(n) => {
                write!(f, "{:?}", n)
            }
            Answer::Bool(b) => {
                write!(f, "{:?}", b)
            }
        }
    }
}

impl From<Number> for Answer {
    fn from(n: Number) -> Self {
        Answer::S64(n)
    }
}

impl From<i32> for Answer {
    fn from(n: i32) -> Self {
        Answer::S64(n as Number)
    }
}

impl From<usize> for Answer {
    fn from(n: usize) -> Self {
        Answer::S64(n as Number)
    }
}

impl From<bool> for Answer {
    fn from(b: bool) -> Self {
        Answer::Bool(b)
    }
}

impl FromStr for Answer {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(ps) = s.parse::<Number>() {
            Ok(Answer::S64(ps))
        } else {
            match s.chars().exactly_one() {
                Ok(n) => match n {
                    'T' => Ok(Answer::Bool(true)),
                    'F' => Ok(Answer::Bool(false)),
                    _ => Err(String::from("Cant parse")),
                },
                Err(_) => Err(String::from("Cant parse")),
            }
        }
    }
}
