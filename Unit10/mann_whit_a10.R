library(tidyverse)

mem.df <- data.frame(
  Pleasant     = c(1.07, 1.17, 1.22, 1.42, 1.63, 1.98, 2.12, 2.32, 2.56, 2.70, 
                   2.93, 2.97, 3.03, 3.15, 3.22, 3.42, 4.63, 4.70, 5.55, 6.17), 
  Unpleasant   = c(1.45, 1.67, 1.90, 2.02, 2.32, 2.35, 2.43, 2.47, 2.57, 3.33, 
                   3.87, 4.33, 5.35, 5.72, 6.48, 6.90, 8.68, 9.47, 10.00, 10.93)
  )%>% 
  pivot_longer(
    cols = c("Pleasant", "Unpleasant"),
    names_to = "Memory",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>% 
  mutate(MemoryRank = rank(Value, ties.method = "average")) %>%
  group_by(Memory) %>%  
  mutate(mx = max(Value)) %>%
  arrange(desc(mx), desc(Value)) %>%
  select(-mx)


nA <- as.numeric(min(count(mem.df, Memory, sort = TRUE)$n))
nB <- as.numeric(max(count(mem.df, Memory, sort = TRUE)$n))
mem.U <- sum(mem.df$MemoryRank[1:nA])
mem.E <- (nA*(nA+nB+1))/2
mem.V <- (nA*nB*(nA+nB+1))/12
mem.Z <- (mem.U - mem.E)/sqrt(mem.V)
mem.P <- pnorm(round(mem.Z, 2), mean = 0, sd = 1), 3)) * 2
  
#wilcox.test(mem.df$Pleasant, mem.df$Unpleasant, exact = FALSE)


