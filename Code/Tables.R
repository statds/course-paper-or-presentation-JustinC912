library(xtable)

data1 <- read.csv("all_seasons.csv")
par(mfrow = c(2,2))
hist(data1$player_height, data = data1)
hist(log(data1$player_height), data = data1)
hist(data1$player_weight, data = data1)

set.seed(1)
ind <- sample(1:dim(data1)[1], dim(data1)[1] * 0.7)
data1train <- data1[ind, ]
data1test <- data1[-ind, ]

# Points Models
linPts <- lm(pts ~ player_height + player_weight, data = data1train)
summary(linPts)

linPts1 <- lm(pts ~ player_height, data = data1train)
summary(linPts1)

linPts2 <- lm(pts ~ player_weight, data = data1train)
summary(linPts2)

# Rebounds Models
linReb <- lm(reb ~ player_height + player_weight, data = data1train)
summary(linReb)

linReb1 <- lm(reb ~ player_height, data = data1train)
summary(linReb1)

linReb2 <- lm(reb ~ player_weight, data = data1train)
summary(linReb2)

# Assists Models
linAst <- lm(ast ~ player_height + player_weight, data = data1train)
summary(linAst)

linAst1 <- lm(ast ~ player_height, data = data1train)
summary(linAst1)

linAst2 <- lm(ast ~ player_weight, data = data1train)
summary(linAst2)

# True Shooting Percentage Models
linTS <- lm(ts_pct ~ player_height + player_weight, data = data1train)
summary(linTS)

linTS1 <- lm(ts_pct ~ player_height, data = data1train)
summary(linTS1)

linTS2 <- lm(ts_pct ~ player_weight, data = data1train)
summary(linTS2)

extractModelDetails <- function(model) {
  c(RSquared = summary(model)$r.squared,
    PValue = summary(model)$coefficients[2, 4])
}

# Points Models
ptsDetails <- data.frame(
  HeightWeight = extractModelDetails(linPts),
  Height = extractModelDetails(linPts1),
  Weight = extractModelDetails(linPts2)
)

# Rebounds Models
rebDetails <- data.frame(
  HeightWeight = extractModelDetails(linReb),
  Height = extractModelDetails(linReb1),
  Weight = extractModelDetails(linReb2)
)

# Assists Models
astDetails <- data.frame(
  HeightWeight = extractModelDetails(linAst),
  Height = extractModelDetails(linAst1),
  Weight = extractModelDetails(linAst2)
)

# True Shooting Percentage Models
tsDetails <- data.frame(
  HeightWeight = extractModelDetails(linTS),
  Height = extractModelDetails(linTS1),
  Weight = extractModelDetails(linTS2)
)


# Printing the tables
print(xtable(ptsDetails, digits = c(0, 4, 4, 4)), include.rownames = TRUE)
print(xtable(rebDetails, digits = c(0, 4, 4, 4)), include.rownames = TRUE)
print(xtable(astDetails, digits = c(0, 4, 4, 4)), include.rownames = TRUE)
print(xtable(tsDetails, digits = c(0, 4, 4, 4)), include.rownames = TRUE)
#print(xtable(rebDetails), include.rownames = TRUE)
#print(xtable(astDetails), include.rownames = TRUE)
#print(xtable(tsDetails), include.rownames = TRUE)




