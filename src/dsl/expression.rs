use super::variable::{LpBinary, LpContinuous, LpInteger};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LpExprOp {
    Addition,
    Subtraction,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LpExpression {
    Constant(f64),
    BinVariable(LpBinary),
    IntVariable(LpInteger),
    ContVariable(LpContinuous),
    BinaryOp(LpExprOp, Box<LpExpression>, Box<LpExpression>),
    MulOp(f64, Box<LpExpression>),
}

impl LpExpression {
    #[must_use]
    pub const fn constant(value: f64) -> Self {
        Self::Constant(value)
    }

    #[must_use]
    pub fn binary_variable(var: &LpBinary) -> Self {
        Self::BinVariable(var.clone())
    }

    #[must_use]
    pub fn integer_variable(var: &LpInteger) -> Self {
        Self::IntVariable(var.clone())
    }

    #[must_use]
    pub fn continuous_variable(var: &LpContinuous) -> Self {
        Self::ContVariable(var.clone())
    }

    #[must_use]
    pub fn binary_op(op: LpExprOp, lhs: &Self, rhs: &Self) -> Self {
        Self::BinaryOp(
            op,
            Box::<Self>::new(lhs.clone()),
            Box::<Self>::new(rhs.clone()),
        )
    }

    #[must_use]
    pub fn mul_op(constant: f64, expr: &Self) -> Self {
        Self::MulOp(constant, Box::<Self>::new(expr.clone()))
    }

    #[must_use]
    pub fn simplify(&self) -> Self {
        match self {
            Self::Constant(_)
            | Self::BinVariable(_)
            | Self::IntVariable(_)
            | Self::ContVariable(_) => self.clone(),
            Self::BinaryOp(op, lhs, rhs) => {
                let lhs_simplified = lhs.simplify();
                let rhs_simplified = rhs.simplify();
                match (lhs_simplified, rhs_simplified) {
                    (Self::Constant(left_val), Self::Constant(right_val)) => match op {
                        LpExprOp::Addition => Self::Constant(left_val + right_val),
                        LpExprOp::Subtraction => Self::Constant(left_val - right_val),
                    },
                    (lhs, rhs) => Self::BinaryOp(op.clone(), Box::new(lhs), Box::new(rhs)),
                }
            }
            Self::MulOp(coef, expr) => {
                let simplified_expr = expr.simplify();
                match simplified_expr {
                    Self::Constant(val) => Self::Constant(coef * val),
                    _ => Self::MulOp(*coef, Box::new(simplified_expr)),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_creation() {
        let constant = LpExpression::constant(5.0);
        assert_eq!(constant, LpExpression::Constant(5.0));
    }

    #[test]
    fn test_binary_variable_creation() {
        let bin_var = LpBinary::new("bin_var");
        let expression = LpExpression::binary_variable(&bin_var);
        assert_eq!(expression, LpExpression::BinVariable(bin_var));
    }

    #[test]
    fn test_integer_variable_creation() {
        let int_var = LpInteger::new("int_var");
        let expression = LpExpression::integer_variable(&int_var);
        assert_eq!(expression, LpExpression::IntVariable(int_var));
    }

    #[test]
    fn test_continuous_variable_creation() {
        let cont_var = LpContinuous::new("cont_var");
        let expression = LpExpression::continuous_variable(&cont_var);
        assert_eq!(expression, LpExpression::ContVariable(cont_var));
    }

    #[test]
    fn test_binary_op_creation() {
        let lhs = LpExpression::constant(3.0);
        let rhs = LpExpression::constant(4.0);
        let expression = LpExpression::binary_op(LpExprOp::Addition, &lhs, &rhs);
        assert_eq!(
            expression,
            LpExpression::BinaryOp(LpExprOp::Addition, Box::new(lhs), Box::new(rhs))
        );
    }

    #[test]
    fn test_mul_op_creation() {
        let expr = LpExpression::constant(3.0);
        let mul_expression = LpExpression::mul_op(2.0, &expr);
        assert_eq!(mul_expression, LpExpression::MulOp(2.0, Box::new(expr)));
    }

    #[test]
    fn test_simplify_constant() {
        let constant = LpExpression::constant(5.0);
        assert_eq!(constant.simplify(), LpExpression::Constant(5.0));
    }

    #[test]
    fn test_simplify_binary_op() {
        let lhs = LpExpression::constant(3.0);
        let rhs = LpExpression::constant(4.0);
        let add_expr = LpExpression::binary_op(LpExprOp::Addition, &lhs, &rhs);
        assert_eq!(add_expr.simplify(), LpExpression::Constant(7.0));

        let sub_expr = LpExpression::binary_op(LpExprOp::Subtraction, &lhs, &rhs);
        assert_eq!(sub_expr.simplify(), LpExpression::Constant(-1.0));
    }

    #[test]
    fn test_simplify_mul_op() {
        let expr = LpExpression::constant(3.0);
        let mul_expr = LpExpression::mul_op(2.0, &expr);
        assert_eq!(mul_expr.simplify(), LpExpression::Constant(6.0));
    }

    #[test]
    fn test_simplify_nested_expression() {
        let inner_expr = LpExpression::binary_op(
            LpExprOp::Addition,
            &LpExpression::constant(2.0),
            &LpExpression::constant(3.0),
        );
        let outer_expr = LpExpression::mul_op(2.0, &inner_expr);
        assert_eq!(outer_expr.simplify(), LpExpression::Constant(10.0));
    }

    #[test]
    fn test_simplify_non_simplifiable_expression() {
        let bin_var = LpBinary {
            name: "x".to_string(),
        };
        let expr = LpExpression::binary_variable(&bin_var);
        assert_eq!(expr.simplify(), LpExpression::BinVariable(bin_var));
    }
}
