a4.dat <- c(7, -3, 3, -5, 3, -1, -3, -2, 1, 8)
wilcox.test(a4.dat, exact = FALSE, correct = TRUE)

coins <- read.csv("o:/m248/data/csvfiles/coins.csv")
wilcox.test(coins$Coin2, mu = 7.5, exact = FALSE)

sh <- read.csv("o:/m248/data/csvfiles/shoshoni.csv")
wilcox.test(sh[,1], mu = (sqrt(5)-1)/2, exact = FALSE)

# These are here for potential use:
# rank(sh[,1], ties.method = "average")
# abs(sh[,1])
# sign(sh[,1])