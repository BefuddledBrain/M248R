library(tidyverse)

# if data source is a file (use 'UTF-8-BOM' if 1st colname is prefixed by 'i..':
# a.df.df <- read.csv("./path/to/file.csv", fileEncoding = "UTF-8-BOM") %>%

# input = a.df;  change columns to 'Asample' and 'Bsample'
a.df <- data.frame(
  Asample  = c(23, 26, 30, 33, 42, 45, 45, 50, 50.5, 96, 113, 557, NA), 
  Bsample  = c(39, 48, 53.5, 55, 57, 66, 77, 79, 108, 121, 162, 197, 309)
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
a.Z <- (a.U - a.E)/sqrt(a.V)
a.P <- 
  if (a.Z < 0) {
    (1-round(pnorm(round(abs(a.Z), 2), mean = 0, sd = 1), 3)) * 2
  } else {
    (round(pnorm(round(a.Z, 2), mean = 0, sd = 1), 3)) * 2
  }