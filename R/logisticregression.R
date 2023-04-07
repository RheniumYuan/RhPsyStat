#' Logistic Regression
#'
#' Logistic regression is for fitting a regression curve to data in which the dependent variable is dichotomous.
#'
#' @param formula The formula of the model. Usually y ~ x1 + x2 + ...
#' @param data The dataset for your regression.
#'
#' @return A list of 12 contents.
#' @return 1) The summary of the model, including Coefficients and their significance, and How is the model compared to the intercept-only (null) model
#' @return 2) Test of each predictor of the logistic model
#' @return 3) -2*LL difference:(null - model)
#' @return 4) df of this chi-square test, the difference in parameter number
#' @return 5) The chi-square of The overall test of the model
#' @return 6) Hosmer and Lemeshow goodness of fit (GOF) test
#' @return 7~9) Examining each predictor
#' @return 10) Odds ratio with its confidence interval for each predictor
#' @return 11) Log likelihood
#' @return 12) Model fitting quality; r2CU is the Nagelkerke R2; r2ML is the Cox&Snell R2
#'
#' @export
#'

logisticregression <- function(formula, data){
  fit <- glm(formula, data = data, family = binomial('logit'), maxit = 200)
  out1 <- summary(fit)
  out2 <- anova(fit, test = 'Chisq')
  out3 <- with(fit, null.deviance - deviance)
  out4 <- with(fit, df.null - df.residual)
  out5 <-with(fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  out6 <- ResourceSelection::hoslem.test(x = fit$y, y = fitted(fit), g = 10)
  out7 <- confint(fit)
  out8 <- aod::wald.test(b=coef(fit),Sigma=vcov(fit),Terms=2:2)
  out9 <- aod::wald.test(b=coef(fit),Sigma=vcov(fit),Terms=3:3)
  out10 <- exp(cbind(OR = coef(fit), confint(fit)))
  out11 <- logLik(fit)
  out12 <- pscl::pR2(fit)
  return(list(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12))
}
