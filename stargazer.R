library(stargazer)
names <- mlr[["lab"]]
stargazer(
  mlr,
  type = "latex",
  title = "Multinomial Logistic Regression Coefficients",
  font.size = "tiny",
  covariate.labels = names,
  header = F,
  report = "vc*p",
  dep.var.labels = names,
  notes.align = "l",
  omit.stat = c("aic"),
  out = "mlr.html"
)
