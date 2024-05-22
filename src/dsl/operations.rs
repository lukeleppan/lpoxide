use std::ops::{Add, Mul, Sub};

use super::{
    expression::{
        LpExprOp::{Addition, Subtraction},
        LpExpression,
    },
    variable::{LpBinary, LpContinuous, LpInteger},
};

macro_rules! impl_ops_for_expr {
    ($trait_name: ident, $func_name: ident, $expr_type: ident) => {
        impl<T> $trait_name<T> for LpExpression
        where
            T: Into<LpExpression> + Clone,
        {
            type Output = LpExpression;
            fn $func_name(self, expr: T) -> LpExpression {
                LpExpression::binary_op($expr_type, &self.into(), &expr.into())
            }
        }

        impl<'a, T> $trait_name<T> for &'a LpExpression
        where
            T: Into<LpExpression> + Clone,
        {
            type Output = LpExpression;
            fn $func_name(self, expr: T) -> LpExpression {
                LpExpression::binary_op($expr_type, &(*self).clone().into(), &expr.into())
            }
        }

        impl $trait_name<i64> for LpExpression {
            type Output = LpExpression;
            fn $func_name(self, rhs: i64) -> Self::Output {
                LpExpression::binary_op($expr_type, &self, &LpExpression::constant(rhs as f64))
            }
        }

        impl<'a> $trait_name<i64> for &'a LpExpression {
            type Output = LpExpression;
            fn $func_name(self, rhs: i64) -> Self::Output {
                LpExpression::binary_op(
                    $expr_type,
                    &(*self).clone(),
                    &LpExpression::constant(rhs as f64),
                )
            }
        }

        impl $trait_name<f64> for LpExpression {
            type Output = LpExpression;
            fn $func_name(self, rhs: f64) -> Self::Output {
                LpExpression::binary_op($expr_type, &self, &LpExpression::constant(rhs))
            }
        }

        impl<'a> $trait_name<f64> for &'a LpExpression {
            type Output = LpExpression;
            fn $func_name(self, rhs: f64) -> Self::Output {
                LpExpression::binary_op($expr_type, &(*self).clone(), &LpExpression::constant(rhs))
            }
        }
    };
}

impl_ops_for_expr!(Add, add, Addition);
impl_ops_for_expr!(Sub, sub, Subtraction);

macro_rules! impl_var_from_expr {
    ($var_type: ty, $func_name: ident) => {
        impl From<$var_type> for LpExpression {
            fn from(value: $var_type) -> Self {
                Self::$func_name(&value)
            }
        }

        impl<'a> From<&'a $var_type> for LpExpression {
            fn from(value: &'a $var_type) -> Self {
                Self::$func_name(value)
            }
        }
    };
}

impl_var_from_expr!(LpBinary, binary_variable);
impl_var_from_expr!(LpInteger, integer_variable);
impl_var_from_expr!(LpContinuous, continuous_variable);

macro_rules! impl_var_ops {
    ($trait_name: ident, $func_name: ident, $var_type: ty, $op_name: ident) => {
        impl<T> $trait_name<T> for $var_type
        where
            T: Into<LpExpression> + Clone,
        {
            type Output = LpExpression;
            fn $func_name(self, rhs: T) -> Self::Output {
                LpExpression::binary_op($op_name, &self.into(), &rhs.into())
            }
        }

        impl<'a, T> $trait_name<T> for &'a $var_type
        where
            T: Into<LpExpression> + Clone,
        {
            type Output = LpExpression;
            fn $func_name(self, rhs: T) -> Self::Output {
                LpExpression::binary_op($op_name, &(*self).clone().into(), &rhs.into())
            }
        }

        impl $trait_name<$var_type> for i64 {
            type Output = LpExpression;
            fn $func_name(self, rhs: $var_type) -> Self::Output {
                LpExpression::binary_op($op_name, &LpExpression::constant(self as f64), &rhs.into())
            }
        }

        impl $trait_name<$var_type> for f64 {
            type Output = LpExpression;
            fn $func_name(self, rhs: $var_type) -> Self::Output {
                LpExpression::binary_op($op_name, &LpExpression::constant(self), &rhs.into())
            }
        }

        impl<'a> $trait_name<&'a $var_type> for i64 {
            type Output = LpExpression;
            fn $func_name(self, rhs: &'a $var_type) -> Self::Output {
                LpExpression::binary_op(
                    $op_name,
                    &LpExpression::constant(self as f64),
                    &rhs.clone().into(),
                )
            }
        }

        impl<'a> $trait_name<&'a $var_type> for f64 {
            type Output = LpExpression;
            fn $func_name(self, rhs: &'a $var_type) -> Self::Output {
                LpExpression::binary_op(
                    $op_name,
                    &LpExpression::constant(self),
                    &rhs.clone().into(),
                )
            }
        }
    };
}

impl_var_ops!(Add, add, LpBinary, Addition);
impl_var_ops!(Add, add, LpInteger, Addition);
impl_var_ops!(Add, add, LpContinuous, Addition);
impl_var_ops!(Sub, sub, LpBinary, Subtraction);
impl_var_ops!(Sub, sub, LpInteger, Subtraction);
impl_var_ops!(Sub, sub, LpContinuous, Subtraction);

macro_rules! impl_mul_ops_for_expr {
    ($type_name: ty) => {
        impl Mul<$type_name> for LpExpression {
            type Output = LpExpression;
            fn mul(self, rhs: $type_name) -> Self::Output {
                LpExpression::mul_op(rhs as f64, &self)
            }
        }

        impl Mul<LpExpression> for $type_name {
            type Output = LpExpression;
            fn mul(self, rhs: LpExpression) -> Self::Output {
                LpExpression::mul_op(self as f64, &rhs)
            }
        }

        impl<'a> Mul<&'a LpExpression> for $type_name {
            type Output = LpExpression;
            fn mul(self, rhs: &'a LpExpression) -> Self::Output {
                LpExpression::mul_op(self as f64, rhs)
            }
        }
    };
}

impl_mul_ops_for_expr!(i64);
impl_mul_ops_for_expr!(f64);

macro_rules! impl_mul_ops_for_var {
    ($type_name: ty, $var_name: ty, $var_func: ident) => {
        impl Mul<$type_name> for $var_name {
            type Output = LpExpression;
            fn mul(self, rhs: $type_name) -> Self::Output {
                LpExpression::mul_op(rhs as f64, &LpExpression::$var_func(&self))
            }
        }

        impl Mul<$var_name> for $type_name {
            type Output = LpExpression;
            fn mul(self, rhs: $var_name) -> Self::Output {
                LpExpression::mul_op(self as f64, &LpExpression::$var_func(&rhs))
            }
        }

        impl<'a> Mul<&'a $var_name> for $type_name {
            type Output = LpExpression;
            fn mul(self, rhs: &'a $var_name) -> Self::Output {
                LpExpression::mul_op(self as f64, &LpExpression::$var_func(rhs))
            }
        }
    };
}

impl_mul_ops_for_var!(i64, LpBinary, binary_variable);
impl_mul_ops_for_var!(i64, LpInteger, integer_variable);
impl_mul_ops_for_var!(i64, LpContinuous, continuous_variable);
impl_mul_ops_for_var!(f64, LpBinary, binary_variable);
impl_mul_ops_for_var!(f64, LpInteger, integer_variable);
impl_mul_ops_for_var!(f64, LpContinuous, continuous_variable);

#[cfg(test)]
mod test {
    use super::*;
    use crate::dsl::variable::LpContinuous;

    #[test]
    fn test_complex_expr() {
        let a = LpBinary::new("a");
        let b = LpInteger::new("b");
        let c = LpContinuous::new("c");

        let expr = 5.0 * &a + 5 * &b - &c + 5.0;
        let expr_expected = LpExpression::binary_op(
            Addition,
            &LpExpression::binary_op(
                Subtraction,
                &LpExpression::binary_op(
                    Addition,
                    &LpExpression::mul_op(5.0, &LpExpression::binary_variable(&a)),
                    &LpExpression::mul_op(5.0, &LpExpression::integer_variable(&b)),
                ),
                &LpExpression::continuous_variable(&c),
            ),
            &LpExpression::constant(5.0),
        );

        assert_eq!(expr, expr_expected);
    }

    #[test]
    fn test_add_expr() {
        let var = LpContinuous::new("var");
        let expr1 = LpExpression::continuous_variable(&var);
        let cloned_expr1 = expr1.clone();
        let expr2 = LpExpression::constant(6.0);
        let cloned_expr2 = expr2.clone();

        let expr3 = expr1 + expr2;

        let expr_expected = LpExpression::binary_op(Addition, &cloned_expr1, &cloned_expr2);

        assert_eq!(expr3, expr_expected);
    }
}
