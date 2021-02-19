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

# Example 6: dopamine levels in psychotic and normal subjects
dop.df <- read.csv("./Unit10/dopamine.csv", fileEncoding = "UTF-8-BOM") %>% 
  pivot_longer(
    cols = c("Psychotic", "Non.psychotic"), 
    names_to = "Diag", 
    values_to = "Dopamine", 
    values_drop_na = TRUE
  ) %>% 
  mutate(DiagRank = rank(Dopamine, ties.method = "average")) %>% 
  arrange(desc(Diag))

dop.n.lo <- as.numeric(min(count(dop.df, Diag, sort = TRUE)$n))

dop.mann_whitney.u <- sum(dop.df$DiagRank[1:dop.lo])

# Activity 9: aboriginal village size in Alaska and California
a.df <- data.frame(
  Col_1  = c(23, 26, 30, 33, 42, 45, 45, 50, 50.5, 96, 113, 557, NA), 
  Col_2      = c(39, 48, 53.5, 55, 57, 66, 77, 79, 108, 121, 162, 197, 309)
)%>% 
  pivot_longer(
    cols = c("Col_1", "Col_2"),
    names_to = "Nm_To",
    values_to = "Value",
    values_drop_na = TRUE
  ) %>% 
  mutate(Nm_ToRank = rank(Value, ties.method = "average"))

up.down <- as.matrix(count(a.df, Nm_To))
if (as.numeric(up.down[1,2])>as.numeric(up.down[2,2])) {
  a.df <- arrange(a.df, desc(Nm_To))
} else {
  a.df <- arrange(a.df, Nm_To)
}

nA <- as.numeric(min(count(a.df, Nm_To, sort = TRUE)$n))
nB <- as.numeric(max(count(a.df, Nm_To, sort = TRUE)$n))
a.U <- sum(a.df$Nm_ToRank[1:nA])
a.E <- (nA*(nA+nB+1))/2
a.V <- (nA*nB*(nA+nB+1))/12
a.Z <- (a.U - a.E)/sqrt(a.V)
a.P <- 
  if (a.Z < 0) {
    (1-round(pnorm(round(abs(a.Z), 2), mean = 0, sd = 1), 3)) * 2
  } else {
    (round(pnorm(round(a.Z, 2), mean = 0, sd = 1), 3)) * 2
  }
