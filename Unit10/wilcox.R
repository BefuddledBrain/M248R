library(tidyverse)

a4.dat <- c(7, -3, 3, -5, 3, -1, -3, -2, 1, 8)
wilcox.test(a4.dat, exact = FALSE, correct = TRUE)

#coins <- read.csv("o:/m248/data/csvfiles/coins.csv")
coins <- c(-0.6, 1.5, -0.9, 0.6, 1.8, 1.7, 1.1)
wilcox.test(coins, mu = 0, exact = FALSE)

sh <- read.csv("./Unit10/shoshoni.csv", fileEncoding = "UTF-8-BOM") %>% 
  mutate(
    Diff = Ratio - 0.618, 
    Sign = sign(Diff), 
    AbsDiff = abs(Diff), 
    Rank = rank(AbsDiff), 
    RankSign = Rank * Sign
  ) 
w.plus <-  as.numeric(sh %>% summarise(w.plus = sum(RankSign[RankSign > 0])))

wilcox.test(sh$Ratio, mu = (sqrt(5)-1)/2, exact = FALSE)
