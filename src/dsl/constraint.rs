use super::expression::LpExpression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    GreaterOrEqual,
    LessOrEqual,
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LpConstraint(LpExpression, Constraint, LpExpression);

impl LpConstraint {
    #[must_use]
    pub const fn new(lhs: LpExpression, constraint: Constraint, rhs: LpExpression) -> Self {
        Self(lhs, constraint, rhs)
    }
}

#[cfg(test)]
mod tests {
    use crate::dsl::{
        expression::LpExprOp,
        variable::{LpBinary, LpInteger},
    };

    use super::*;

    #[test]
    fn test_greater_or_equal_constraint_creation() {
        let lhs = LpExpression::constant(2.0);
        let rhs = LpExpression::constant(3.0);
        let constraint = LpConstraint::new(lhs.clone(), Constraint::GreaterOrEqual, rhs.clone());
        assert_eq!(
            constraint,
            LpConstraint(lhs, Constraint::GreaterOrEqual, rhs)
        );
    }

    #[test]
    fn test_less_or_equal_constraint_creation() {
        let lhs = LpExpression::constant(2.0);
        let rhs = LpExpression::constant(3.0);
        let constraint = LpConstraint::new(lhs.clone(), Constraint::LessOrEqual, rhs.clone());
        assert_eq!(constraint, LpConstraint(lhs, Constraint::LessOrEqual, rhs));
    }

    #[test]
    fn test_equal_constraint_creation() {
        let lhs = LpExpression::constant(2.0);
        let rhs = LpExpression::constant(3.0);
        let constraint = LpConstraint::new(lhs.clone(), Constraint::Equal, rhs.clone());
        assert_eq!(constraint, LpConstraint(lhs, Constraint::Equal, rhs));
    }

    #[test]
    fn test_constraint_with_variables() {
        let bin_var = LpBinary::new("x");
        let int_var = LpInteger::new("y");
        let lhs = LpExpression::binary_variable(&bin_var);
        let rhs = LpExpression::integer_variable(&int_var);
        let constraint = LpConstraint::new(lhs.clone(), Constraint::LessOrEqual, rhs.clone());
        assert_eq!(constraint, LpConstraint(lhs, Constraint::LessOrEqual, rhs));
    }

    #[test]
    fn test_constraint_with_complex_expression() {
        let lhs = LpExpression::binary_op(
            LpExprOp::Addition,
            &LpExpression::constant(2.0),
            &LpExpression::constant(3.0),
        );
        let rhs = LpExpression::mul_op(2.0, &LpExpression::constant(3.0));
        let constraint = LpConstraint::new(lhs.clone(), Constraint::GreaterOrEqual, rhs.clone());
        assert_eq!(
            constraint,
            LpConstraint(lhs, Constraint::GreaterOrEqual, rhs)
        );
    }
}
