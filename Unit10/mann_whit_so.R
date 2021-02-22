library(tidyverse)
# Activity 9: aboriginal village size in Alaska and California
a.df <- data.frame(
  Alaska  = c(23, 26, 30, 33, 42, 45, 45, 50, 50.5, 96, 113, 557, NA), 
  Calif   = c(39, 48, 53.5, 55, 57, 66, 77, 79, 108, 121, 162, 197, 309)
)%>% 
  pivot_longer(
    cols = c("Alaska", "Calif"),
    names_to = "State",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>% 
  mutate(StateRank = rank(Value, ties.method = "average")) %>%
  group_by(State) %>%  
  mutate(mx = max(Value)) %>%
  arrange(desc(mx), desc(Value)) %>%
  select(-mx)


nA <- as.numeric(min(count(a.df, State, sort = TRUE)$n))
nB <- as.numeric(max(count(a.df, State, sort = TRUE)$n))
a.U <- sum(a.df$StateRank[1:nA])
a.E <- (nA*(nA+nB+1))/2
a.V <- (nA*nB*(nA+nB+1))/12
a.Z <- (a.U - a.E)/sqrt(a.V)
a.P <- round((1 - round(pnorm(round(abs(a.Z), 2), 
                                mean = 0, sd = 1) ,4)) * 2, 3)
# all the rounding is to mimic statistical tables (so that 
# the answer is the same as the textbook answer)
