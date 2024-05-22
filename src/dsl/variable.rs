pub trait BoundableVar: PartialEq + Clone {
    #[must_use]
    fn lower_bound(self, lb: f32) -> Self;
    #[must_use]
    fn upper_bound(self, ub: f32) -> Self;
    #[must_use]
    fn bounds(self, lb: f32, ub: f32) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LpBinary {
    pub name: String,
}

impl LpBinary {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LpInteger {
    pub name: String,
    pub lower_bound: Option<f32>,
    pub upper_bound: Option<f32>,
}

impl LpInteger {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            lower_bound: None,
            upper_bound: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LpContinuous {
    pub name: String,
    pub lower_bound: Option<f32>,
    pub upper_bound: Option<f32>,
}

impl LpContinuous {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            lower_bound: None,
            upper_bound: None,
        }
    }
}

macro_rules! implement_boundable {
    ($lp_var: ident) => {
        impl BoundableVar for $lp_var {
            fn lower_bound(mut self, lb: f32) -> Self {
                self.lower_bound = Some(lb);
                self
            }

            fn upper_bound(mut self, ub: f32) -> Self {
                self.upper_bound = Some(ub);
                self
            }

            fn bounds(mut self, lb: f32, ub: f32) -> Self {
                self.upper_bound = Some(ub);
                self.lower_bound = Some(lb);
                self
            }
        }
    };
}

implement_boundable!(LpInteger);
implement_boundable!(LpContinuous);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_lp_binary() {
        let var = LpBinary::new("binary_var");
        assert_eq!(var.name, "binary_var");
    }

    #[test]
    fn test_create_lp_integer() {
        let var = LpInteger::new("integer_var");
        assert_eq!(var.name, "integer_var");
        assert_eq!(var.lower_bound, None);
        assert_eq!(var.upper_bound, None);
    }

    #[test]
    fn test_create_lp_continuous() {
        let var = LpInteger::new("continuous_var");
        assert_eq!(var.name, "continuous_var");
        assert_eq!(var.lower_bound, None);
        assert_eq!(var.upper_bound, None);
    }

    #[test]
    fn test_set_lower_bound_lp_integer() {
        let var = LpInteger::new("integer_var").lower_bound(0.0);
        assert_eq!(var.lower_bound, Some(0.0));
    }

    #[test]
    fn test_set_upper_bound_lp_integer() {
        let var = LpInteger::new("integer_var").upper_bound(10.0);
        assert_eq!(var.upper_bound, Some(10.0));
    }

    #[test]
    fn test_set_bounds_lp_integer() {
        let var = LpInteger::new("integer_var").bounds(0.0, 10.0);
        assert_eq!(var.lower_bound, Some(0.0));
        assert_eq!(var.upper_bound, Some(10.0));
    }

    #[test]
    fn test_set_lower_bound_lp_continuous() {
        let var = LpContinuous::new("continuous_var").lower_bound(0.0);
        assert_eq!(var.lower_bound, Some(0.0));
    }

    #[test]
    fn test_set_upper_bound_lp_continuous() {
        let var = LpContinuous::new("continuous_var").upper_bound(10.0);
        assert_eq!(var.upper_bound, Some(10.0));
    }

    #[test]
    fn test_set_bounds_lp_continuous() {
        let var = LpContinuous::new("continuous_var").bounds(0.0, 10.0);
        assert_eq!(var.lower_bound, Some(0.0));
        assert_eq!(var.upper_bound, Some(10.0));
    }
}
