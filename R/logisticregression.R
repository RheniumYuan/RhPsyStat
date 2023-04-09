#' Logistic Regression
#'
#' Logistic regression is for fitting a regression curve to data in which the dependent variable is dichotomous.
#'
#' @param formula The formula of the model. Usually y ~ x1 + x2 + ...
#' @param data The dataset for your regression.
#' @param hoslemG g in hoslem test
#'
#' @return The summary of the model, including Coefficients and their significance, and How is the model compared to the intercept-only (null) model
#' @return Test of each predictor of the logistic model
#' @return Wald Test for intercept and each coefficient
#' @return Hosmer and Lemeshow goodness of fit (GOF) test
#' @return Odds ratio with its confidence interval for each predictor
#' @return Log likelihood
#' @return Model fitting quality: r2CU is the Nagelkerke R2; r2ML is the Cox&Snell R2
#' @return Classification Table
#'
#' @export
#'

logisticregression <- function(formula, data, hoslemG = 10){
  fit <- glm(formula, data = data, family = binomial('logit'))
  print(summary(fit))
  print(anova(fit, test = 'Chisq'))
  print('-2*LL difference:(null - model):')
  print(with(fit, null.deviance - deviance))
  print('df of this chi-square test, the difference in parameter number:')
  print(with(fit, df.null - df.residual))
  print('the chi-square test:')
  print(with(fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
  print('Wald Test for intercept and each coefficient')
  for (i in 1:length(fit$coefficients)){
    print(wald.test(b=coef(fit),Sigma=vcov(fit),Terms=i:i))
  }
  print(hoslem.test(x = fit$y, y = fitted(fit), g = hoslemG))
  print(confint(fit))
  print(exp(cbind(OR = coef(fit), confint(fit))))
  print(logLik(fit))
  print(pR2(fit))
  pred_y = predict(fit,type="response")
  classification_df = data.frame(observed_y = fit$y,
                                 predicted_y = round(pred_y,0))
  print(xtabs(~observed_y+predicted_y,data=classification_df))
}
