library(ggplot2)
library(dplyr)

# import data
data.df <- read.csv('o:/m248/data/csvfiles/unaffiliated.csv', fileEncoding = "UTF-8-BOM") %>% 
  rename(
    var.rsp = "Satisfaction", 
    var1.exp = "Ratio", 
    var2.exp = "Academic.spend", 
    var3.exp = "Facilities.spend"
  )

# create linear regression model:
data.lm <- lm(var.rsp ~ var1.exp, data = data.df)

data.conf <- data.lm %>% 
  predict(interval = 'confidence', level = 0.95) %>% 
  as.data.frame() %>% 
  rename(
    conf.lo = lwr, 
    conf.hi = upr
  )

data.pred <- data.lm %>% 
  predict(interval = 'prediction', level = 0.95) %>% 
  as.data.frame() %>% 
  rename(
    pred.lo = lwr, 
    pred.hi = upr
  )

data.df <- cbind(data.df, data.conf[!names(data.conf) %in% names(data.df)])
data.df <- cbind(data.df, data.pred[!names(data.pred) %in% names(data.df)])

# alternative scatterplot (prefer this)
ggplot(data = data.df, aes(x = var1.exp)) +
  geom_point(aes(y = var.rsp), color = "darkred") + 
  geom_ribbon(aes(ymin = conf.lo, ymax = conf.hi), fill = "bisque", linetype = "dashed", color = "chocolate", alpha = 0.5) +
  geom_ribbon(aes(ymin = pred.lo, ymax = pred.hi), fill = "beige", linetype = "dashed", color = "chocolate", alpha = 0.4) +
  geom_line(aes(y = fit), color = "chocolate") +
  annotate(
    "text", x = 18.5, y = 4.38, 
    label = paste("var.rsp = ", round(coef(data.lm)[1], 3), " - ", abs(round(coef(data.lm)[2], 3)), " var1.exp")
  ) + 
  theme_bw()

# plot residuals vs fitted values
ggplot(data.lm, aes(x = .fitted, y = .resid)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, color = "chocolate") +
  # geom_smooth(se = FALSE) + 
  labs(
    title="Residuals vs Fitted values", 
    y="Residuals",
    x="Fitted values" 
  ) + 
  theme_bw()

# standardised residuals vs fitted values
ggplot(fortify(data.lm, data.df), aes(.fitted, .stdresid)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, color = "chocolate") +
  labs(
    title="Standardised residuals vs Fitted values", 
    y="Standardised residuals",
    x="Fitted values" 
  ) + 
  theme_bw()

# standardised residuals vs variables values
ggplot(fortify(data.lm, data.df), aes(var1.exp, .stdresid)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, color = "chocolate") +
  labs(
    title="Standardised residuals vs Variable values", 
    y="Standardised residuals",
    x="var1.exp" 
  ) + 
  theme_bw()

# normal probability plot (QQ plot)
ggplot(data.lm) +
  stat_qq(aes(sample = .stdresid), color = "red") +
  geom_abline(color = "chocolate") +
  labs(
    title="Normal probability plot (QQ plot)", 
    y="Standardised residuals",
    x="Theoretical quantiles" 
  ) + 
  theme_bw()

#
# more diagnostic plots: Cook's distance vs Leverage; 
#

# scale-location plot (or spread-location plot):

# the assumption of homogeneity of variance can be checked by examining the scale-location plot, also known as 
# the spread-location plot
# shows residuals are spread equally along the ranges of predictors. This is how you can check the assumption 
# of equal variance (homoscedasticity). you should see a horizontal line with equally (randomly) spread points.
ggplot(data.lm, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(color = "darkred") + 
  geom_smooth(se = FALSE, color = "chocolate") +
  labs(
    title="Scale-Location plot", 
    y="Square root of standardised residuals",
    x="Fitted values" 
  ) + 
  theme_bw()


# cook's distance vs leverage
# This plot helps us to find influential cases (i.e., subjects) if any. Not all outliers are influential in 
# linear regression analysis (whatever outliers mean). Even though data have extreme values, they might not be 
# influential to determine a regression line. That means the results wouldn’t be much different if we either 
# include or exclude them from analysis. They follow the trend in the majority of cases and they don’t really 
# matter; they are not influential. On the other hand, some cases could be very influential even if they look 
# to be within a reasonable range of the values. They could be extreme cases against a regression line and can 
# alter the results if we exclude them from analysis. Another way to put it is that they don’t get along with 
# the trend in the majority of the cases.

# Unlike the other plots, this time patterns are not relevant. We watch out for outlying values at the upper 
# right corner or at the lower right corner. Those spots are the places where cases can be influential against 
# a regression line. Look for cases outside of a dashed line, Cook’s distance. When cases are outside of the 
# Cook’s distance (meaning they have high Cook’s distance scores), the cases are influential to the regression 
# results. The regression results will be altered if we exclude those cases.

# .hat is used to detect high-leverage points (or extreme values in the predictors x variables)

ggplot(data.lm, aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, color = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), linetype = "dashed", colour = "gray60") +
  geom_smooth(se = FALSE, color = "gray80") +
  geom_point(color = "darkred") +
  scale_x_continuous(
    expand = c(0, 0, 0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0, 0, 0.0)
  ) + 
  theme_bw()


# predicting new values:
fitted(data.lm)
predict(data.lm)
new.df <- data.frame(var1.exp = c(1200, 1300, 1400, 1500, 1600))
predict(data.lm, newdata = new.df)

predict(data.lm, interval = "confidence", level = 0.95)
predict(data.lm, interval = "predict", level = 0.95)

