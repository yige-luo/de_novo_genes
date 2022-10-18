my_lm <- function(formula, data, alternative = "two.sided", ...){
  mod <- lm(formula = formula, data = data, ...)
  mod_sum <- summary(mod)
  
  t_star <- (mod_sum$coefficients[2,1] - 1)/(mod_sum$coefficients[2,2])
  alternative = match.arg(alternative, c("greater", "two.sided", "less"))[1]
  if(alternative == "two.sided"){
    2 * pt(-abs(t_star ), df = df.residual(mod))
  }
  else if(alternative == "less"){
    pt(t_star , df = df.residual(mod))
  }
  else{
    # one sided p val (b1 > 1)
    pt(t_star, df = df.residual(mod), lower.tail = FALSE)
  }
}

