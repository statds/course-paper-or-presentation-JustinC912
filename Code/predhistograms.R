pdf("predhistograms.pdf", width = 8.5, height = 5.5)

data1 <- read.csv("all_seasons.csv")

par(mfrow = c(1, 2), mar = c(5, 4, 2, 2) + 0.1)

hist(data1$player_height, main = "Player Height", xlab = "Height(cm)", ylab = "Frequency")
hist(data1$player_weight, main = "Player Weight", xlab = "Weight(kg)", ylab = "Frequency")

dev.off() 