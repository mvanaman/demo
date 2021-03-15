# Inspect Data/Descriptives ---
skim(data)

# linear regression
mod <- lm(DV ~ IV, data)
## homoscedasticity
lmtest::bptest(mod)
plot(mod, which = 1)
## normality of residuals
plot(mod, which = 2)
mod$residuals %>% density() %>% plot
round(mean(mod$residuals), 4)
## autocorrelation
lmtest::dwtest(mod)
