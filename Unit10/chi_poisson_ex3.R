library(dplyr)

a.df <- data.frame(
  #Num <- seq(0, 6, 1), 
  Num <- seq(0, 3, 1), 
  #Obs <- c(213, 128, 37, 18, 3, 1, 0)
  Obs <- c(213, 128, 37, 22)
)

lambda <- 0.6825             #sum(a.df$Num * a.df$Obs)/sum(a.df$Obs)

a.df <- a.df %>% 
  mutate(
    Exp = sum(Obs) * (exp(-lambda)*(lambda^Num)/factorial(Num)), 
    OE.diff = Obs - Exp, 
    OE.scaled = (Obs - Exp)^2/Exp
  ) %>% 
  rename(
    Num = names(a.df[1]), 
    Obs = names(a.df[2])
  )

p.Obs = dpois(a.df$Num, lambda)
p.Extra = 1-sum(p.Obs)
p.All <- c(p.Obs, p.Extra)

chi.stat.calc <- sum(a.df$OE.scaled)
chi.stat.test <- chisq.test(x = c(a.df$Obs, 0), p=p.All)$statistic
chi.parm <- chisq.test(x = c(a.df$Obs, 0), p=p.All)$parameter
chi.p.raw <- chisq.test(x = c(a.df$Obs, 0), p=p.All)$p.value
chi.p.mod <- chisq.test(x = c(a.df$Obs, 0), p=p.All, simulate.p.value = TRUE)$p.value