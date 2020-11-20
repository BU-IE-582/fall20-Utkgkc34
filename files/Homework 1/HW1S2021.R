
dataset <- read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv', header = TRUE)

home_goals <- dataset$FTHG

away_goals <- dataset$FTAG

hist(home_goals, main = 'Histogram of Home Teams Goals', xlab = 'Home Goals', ylab = 'Number of Games', ylim = c(0 ,50))

yfith <- dpois(0:7, mean(home_goals))*100

lines(0:7, yfith)

hist(away_goals, main = 'Histogram of Away Team Goals', xlab = 'Away Goals', ylab = 'Number of Games', ylim = c(0, 50))

yfita <- dpois(0:6, mean(home_goals))*100

lines(0:6, yfita)

goal_difference <- home_goals- away_goals

hist(goal_difference, main = 'Histogram of Goal Differences', xlab = 'Goal Difference', ylab = 'Number of Games', xlim = c(-5, 5))

home_win1 <- dataset$B365H

home_lose1 <- dataset$B365A

tie1 <- dataset$B365D

P_homewin1 <- (1/home_win1)

P_homelose1 <- (1/home_lose1)

P_tie1 <- (1/tie1)

P_homewin1 <- (P_homewin1) * (P_homewin1 + P_homelose1 + P_tie1)

P_homelose1 <- (P_homelose1) * (P_homewin1 + P_homelose1 + P_tie1)

P_tie1 <- (P_tie1)* (P_homewin1 + P_homelose1 + P_tie1)

P_draw1 <- (P_homewin1 - P_homelose1)

plot(P_draw1, P_tie1, main = '365 BET', xlim = c(-1,1))

cut(P_draw1, seq(-1,1,0.2))

home_win2 <- dataset$BWH

home_lose2 <- dataset$BWA

tie2 <- dataset$BWD

P_homewin2 <- (1/home_win2)

P_homelose2 <- (1/home_lose2)

P_tie2 <- (1/tie2)

P_homewin2 <- (P_homewin2) * (P_homewin2 + P_homelose2 + P_tie2)

P_homelose2 <- (P_homelose2) * (P_homewin2 + P_homelose2 + P_tie2)

P_tie2 <- (P_tie2)* (P_homewin2 + P_homelose2 + P_tie2)

P_draw2 <- (P_homewin2 - P_homelose2)

plot(P_draw2, P_tie2, main = 'BET AND WIN', xlim = c(-1,1))

cut(P_draw2, seq(-1,1,0.2))

home_win3 <- dataset$PSH

home_lose3 <- dataset$PSA

tie3 <- dataset$PSD

P_homewin3 <- (1/home_win3)

P_homelose3 <- (1/home_lose3)

P_tie3 <- (1/tie3)

P_homewin3 <- (P_homewin3) * (P_homewin3 + P_homelose3 + P_tie3)

P_homelose3 <- (P_homelose3) * (P_homewin3 + P_homelose3 + P_tie3)

P_tie3 <- (P_tie3)* (P_homewin3 + P_homelose3 + P_tie3)

P_draw3 <- (P_homewin3 - P_homelose3)

plot(P_draw3, P_tie3, main = 'PINNACLE', xlim = c(-1,1))

cut(P_draw3, seq(-1,1,0.2))

home_win4 <- dataset$IWH

home_lose4 <- dataset$IWA

tie4 <- dataset$IWD

P_homewin4 <- (1/home_win4)

P_homelose4 <- (1/home_lose4)

P_tie4 <- (1/tie4)

P_homewin4 <- (P_homewin4) * (P_homewin4 + P_homelose4 + P_tie4)

P_homelose4 <- (P_homelose4) * (P_homewin4 + P_homelose4 + P_tie4)

P_tie4 <- (P_tie4)* (P_homewin4 + P_homelose4 + P_tie4)

P_draw4 <- (P_homewin4 - P_homelose4)

plot(P_draw4, P_tie4, main = 'SOME BOOKMAKER', xlim = c(-1,1))

cut(P_draw4, seq(-1,1,0.2))