library(tidyverse)

a4.dat <- c(7, -3, 3, -5, 3, -1, -3, -2, 1, 8)
wilcox.test(a4.dat, exact = FALSE, correct = TRUE)

#coins <- read.csv("o:/m248/data/csvfiles/coins.csv")
coins <- c(-0.6, 1.5, -0.9, 0.6, 1.8, 1.7, 1.1)
wilcox.test(coins, mu = 0, exact = FALSE)

sh <- read.csv("./Unit10/shoshoni.csv")
wilcox.test(sh$rects, mu = (sqrt(5)-1)/2, exact = FALSE)

# These are here for potential use:
# rank(sh[,1], ties.method = "average")
# abs(sh[,1])
# sign(sh[,1])
