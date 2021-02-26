library(dplyr)

a.df <- data.frame(
  Var <- c("o", "v", "f", "vf"), 
  Obs <- c(187, 37, 35, 31)
)

a.df <- a.df %>% 
  mutate(
    #Probs = c(9/16, 3/16, 3/16, 1/16), 
    Probs =c(0.6209, 0.1291, 0.1291, 0.1209), 
    Exp = sum(Obs) * Probs, 
    OE.diff = Obs - Exp, 
    OE.scaled = OE.diff^2 / Exp 
  ) %>% 
  rename(
    Var = names(a.df[1]), 
    Obs = names(a.df[2])
  )

chi.stat.test <- chisq.test(x = a.df$Obs, p=a.df$Probs)$statistic
chi.parm <- chisq.test(x = a.df$Obs, p=a.df$Probs)$parameters
chi.p.raw <- chisq.test(x = a.df$Obs, p=a.df$Probs)$p.value