library(tidyverse)

a4.dat <- c(7, -3, 3, -5, 3, -1, -3, -2, 1, 8)
wilcox.test(a4.dat, exact = FALSE, correct = TRUE)

#coins <- read.csv("o:/m248/data/csvfiles/coins.csv")
coins <- c(-0.6, 1.5, -0.9, 0.6, 1.8, 1.7, 1.1)
wilcox.test(coins, mu = 0, exact = FALSE)

# shoshoni tapestry activities
sh <- read.csv("./Unit10/shoshoni.csv", fileEncoding = "UTF-8-BOM") %>% 
  mutate(
    Diff = Ratio - 0.618, 
    Sign = sign(Diff), 
    AbsDiff = abs(Diff), 
    Rank = rank(AbsDiff), 
    RankSign = Rank * Sign
  ) 
w.plus <-  as.numeric(sh %>% summarise(w.plus = sum(RankSign[RankSign > 0])))

n <- length(sh$Ratio)
w.plus.E <- (n*(n+1))/4
w.plus.V <- (n*(n+1)*(2*n+1))/24
w.plus.Z <- (w.plus - w.plus.E)/sqrt(w.plus.V)
w.plus.p <- 1 - round(pnorm(round(w.plus.Z, 2), mean = 0, sd = 1), 3)
# we are considering not= so this is a two-tailed test
w.plus.p <- round(2 * w.plus.p, 3)

wilcox.test(sh$Ratio, mu = (sqrt(5)-1)/2, exact = FALSE)
