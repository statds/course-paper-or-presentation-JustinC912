
data1 <- read.csv("all_seasons.csv")
set.seed(1)
ind <- sample(1:dim(data1)[1], dim(data1)[1] * 0.7)
data1train <- data1[ind, ]
data1test <- data1[-ind, ]
linPts <- lm(pts ~ player_height + player_weight, data = data1train)
linReb <- lm(reb ~ player_height + player_weight, data = data1train)
linAst1 <- lm(ast ~ player_height, data = data1train)
linTS1 <- lm(ts_pct ~ player_height, data = data1train)

pdf("actuals_vs_predictedgraphs")
par(mfrow = c(2,2))
# Points Graph
predictions <- predict(linPts, data1train)
plot(data1train$pts, predictions, xlab = "Actual", ylab = "Predicted", 
     main = "Actual vs Predicted Points", col = "black", pch = 16)
abline(lm(predictions ~ data1train$pts), col = "red")
abline(0, 1, col = "green", lty = 2)

# Rebounds Graph
predictions <- predict(linReb, data1train)
plot(data1train$reb, predictions, xlab = "Actual", ylab = "Predicted", 
     main = "Actual vs Predicted Rebounds", col = "black", pch = 16)
abline(lm(predictions ~ data1train$reb), col = "red")
abline(0, 1, col = "green", lty = 2)

# Assists Graph
predictions <- predict(linAst1, data1train)
plot(data1train$ast, predictions, xlab = "Actual", ylab = "Predicted", 
     main = "Actual vs Predicted Assists", col = "black", pch = 16)
abline(lm(predictions ~ data1train$ast), col = "red")
abline(0, 1, col = "green", lty = 2)

# True Shooting Graph
predictions <- predict(linTS1, data1train)
plot(data1train$ts_pct, predictions, xlab = "Actual", ylab = "Predicted", 
     main = "Actual vs Predicted True Shooting", col = "black", pch = 16)
abline(lm(predictions ~ data1train$ts_pct), col = "red")
abline(0, 1, col = "green", lty = 2)
dev.off()
