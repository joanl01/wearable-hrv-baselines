compute_icc <- function(data, outcome) {
  formula <- as.formula(paste0(outcome, " ~ 1 + (1 | participant)"))
  model <- lmer(formula, data = data, REML = TRUE)
  
  var_comp <- as.data.frame(VarCorr(model))
  
  between_var <- var_comp$vcov[var_comp$grp == "participant"]
  within_var  <- attr(VarCorr(model), "sc")^2
  
  icc <- between_var / (between_var + within_var)
  return(icc)
}
