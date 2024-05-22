use std::ops::AddAssign;

use super::{constraint::LpConstraint, expression::LpExpression};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum LpObjective {
    #[default]
    Maximize,
    Minimize,
}

pub trait Problem {
    fn add_objective_expr(&mut self, expr: &LpExpression);
    fn add_constraint(&mut self, constraint: &LpConstraint);
}

#[derive(Debug, Clone, PartialEq)]
pub struct LpProblem {
    pub name: String,
    pub objective_type: LpObjective,
    pub objective_expr: Option<LpExpression>,
    pub constraints: Vec<LpConstraint>,
}

impl LpProblem {
    #[must_use]
    pub fn new(name: &str, objective: LpObjective) -> Self {
        Self {
            name: name.to_string(),
            objective_type: objective,
            objective_expr: None,
            constraints: Vec::new(),
        }
    }
}

impl Problem for LpProblem {
    fn add_objective_expr(&mut self, expr: &LpExpression) {
        self.objective_expr = Some(expr.clone());
    }

    fn add_constraint(&mut self, constraint: &LpConstraint) {
        self.constraints.push(constraint.clone());
    }
}

impl AddAssign<LpExpression> for LpProblem {
    fn add_assign(&mut self, rhs: LpExpression) {
        self.add_objective_expr(&rhs);
    }
}

impl AddAssign<LpConstraint> for LpProblem {
    fn add_assign(&mut self, rhs: LpConstraint) {
        self.add_constraint(&rhs);
    }
}

#[cfg(test)]
mod test {
    use crate::dsl::{constraint::Constraint, variable::LpContinuous};

    use super::*;

    #[test]
    fn test_create_lp_problem() {
        let problem = LpProblem::new("Problem", LpObjective::Maximize);

        assert_eq!(problem.name, "Problem");
        assert_eq!(problem.objective_type, LpObjective::Maximize);
        assert_eq!(problem.objective_expr, None);
    }

    #[test]
    fn test_add_objective_expr() {
        let mut problem = LpProblem::new("Problem", LpObjective::Maximize);
        let variable = LpContinuous::new("variable");
        let expr = LpExpression::continuous_variable(&variable);
        problem.add_objective_expr(&expr);

        assert_eq!(problem.objective_expr, Some(expr));
    }

    #[test]
    fn test_add_constraint() {
        let mut problem = LpProblem::new("Problem", LpObjective::Maximize);

        // Add objective expr
        let variable = LpContinuous::new("variable");
        let expr = LpExpression::continuous_variable(&variable);
        problem.add_objective_expr(&expr);

        // Add constraint
        let constraint = LpConstraint::new(expr.clone(), Constraint::Equal, expr);
        problem.add_constraint(&constraint);

        assert_eq!(problem.constraints.last(), Some(&constraint));
    }

    #[test]
    fn test_add_assign_objective_expr() {
        let mut problem = LpProblem::new("Problem", LpObjective::Maximize);

        let variable = LpContinuous::new("variable");
        let expr = LpExpression::continuous_variable(&variable);
        let expr_cloned = expr.clone();

        problem += expr;

        assert_eq!(problem.objective_expr, Some(expr_cloned));
    }

    #[test]
    fn test_add_assign_constraint() {
        let mut problem = LpProblem::new("Problem", LpObjective::Maximize);

        let variable = LpContinuous::new("variable");
        let expr = LpExpression::continuous_variable(&variable);
        let constraint = LpConstraint::new(expr.clone(), Constraint::Equal, expr);
        let constraint_cloned = constraint.clone();

        problem += constraint;

        assert_eq!(problem.constraints.first(), Some(&constraint_cloned));
    }
}
