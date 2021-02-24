library(dplyr)
ruth.df <- data.frame(
  count = seq(0, 13, 1), 
  frequency = c(57, 203, 383, 525, 532, 408, 273, 139, 49, 27, 10, 4, 2, 0)
)
lambda <- sum(ruth.df$count * ruth.df$frequency)/sum(ruth.df$frequency)
ruth.df <- ruth.df %>% 
  mutate(
    expected = sum(frequency) * 
                       (exp(-lambda)*(lambda^count)/factorial(count)), 
    o_e.diff = frequency - expected, 
    o_e.diff.scaled = (o_e.diff)^2/expected
  )
chi.sq <- round(sum(ruth.df$o_e.diff.scale), 3)
chisq.test(ruth.df$expected, ruth.df$frequency, correct = TRUE)
