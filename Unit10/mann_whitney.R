library(tidyverse)

# if data source is a file (use 'UTF-8-BOM' if 1st colname is prefixed by 'i..':
# a.df.df <- read.csv("./path/to/file.csv", fileEncoding = "UTF-8-BOM") %>%

# input = a.df;  change columns to 'Asample' and 'Bsample'
a.df <- data.frame(
  Asample  = c(5.9, 6.2, 6.4, 6.6, 6.8, 6.9, 7.0, 7.2, 7.7), 
  Bsample  = c(6.6, 6.9, 8.1, 8.6, 9.0, 9.2, 9.3, NA, NA)
)%>% 
  pivot_longer(
    cols = c("Asample", "Bsample"),
    names_to = "Group",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>% 
  mutate(GroupRank = rank(Value, ties.method = "average")) %>%
  group_by(Group) %>%  
  mutate(mx = max(Value)) %>%
  arrange(desc(mx), desc(Value)) %>%
  select(-mx)


nA <- as.numeric(min(count(a.df, Group, sort = TRUE)$n))
nB <- as.numeric(max(count(a.df, Group, sort = TRUE)$n))
a.U <- sum(a.df$GroupRank[1:nA])
a.E <- (nA*(nA+nB+1))/2
a.V <- (nA*nB*(nA+nB+1))/12
a.Z <- round((a.U - a.E)/sqrt(a.V), 2)
a.P <- (1 - round(pnorm(abs(a.Z), mean = 0, sd = 1) ,4)) * 2

# rounding mimics statistical tables (so that 
# the answer is the same as the textbook answer)


#-----------------------------------------------------------------
# Mann-Whitney using R function, wilcox.test()
#-----------------------------------------------------------------
# a.df <- data.frame(
#   Asample  = c(5.9, 6.2, 6.4, 6.6, 6.8, 6.9, 7.0, 7.2, 7.7), 
#   Bsample  = c(6.6, 6.9, 8.1, 8.6, 9.0, 9.2, 9.3, NA, NA))
#
# wilcox.test(a.df$Asample, a.df$Bsample, exact = FALSE)
#
# (only works on a.df before piped transformations)
#-----------------------------------------------------------------