SampleID=c("C1","C2","A003_dt0.5","A028_dt0.5","A200_dt0.5","A003_dt1.0","A010_dt1.0","A017_dt1.0","A028_dt1.0","A200_dt1.0","A010_dt1.5","A028_dt1.5","A200_dt1.5","A010_dt2.0","A017_dt2.0","A028_dt2.0","A200_dt2.0")

BulkDensity=c(125.65, 134.55, 115.98, 114.39, 127.89, 129.15, 129.46, 119.5, 117.34, 126.2, 120.65, 134.98, 138.25, 127.93, 117.03, 117.96, 123.16)
diamLess2=c(12988.7535235678, 1253.55443391625, 10299.6645950186, 12944.7823100362, 1624.25132287284, 1875.6363955196, 931.351462576687, 5811.60212468018, 3036.01593953867, 1093.99396624585, 1130.80221900048, 1167.98000437304, 8677.11707813081, 2852.14202164831, 2196.16061243017, 5175.39402637571, 2027.47676219774)
diamMore2=c(0.185693449546921, 0, 1.22724773234201, 0.83946229477785, 140.304121681251, 136.469128846693, 12.8039877300613, 145.887277947254, 31.2111315422886, 205.635383009018, 104.779913526892, 86.3046786291473, 51.7169533259012, 153.141348651728, 38.1593798789572, 49.2484034914611, 78.4160151713192)
alldiam=c(12988.9392170174, 1253.55443391625, 10300.8918427509, 12945.621772331, 1764.55544455409, 2012.10552436629, 944.155450306748, 5957.48940262744, 3067.22707108096, 1299.62934925486, 1235.58213252737, 1254.28468300219, 8728.83403145671, 3005.28337030004, 2234.31999230913, 5224.64242986717, 2105.89277736906) 


# Correlation between BulkDensity and diamLess2
cor(BulkDensity, diamLess2)

# Correlation between BulkDensity and diamMore2
cor(BulkDensity, diamMore2)

# Correlation between BulkDensity and alldiam
cor(BulkDensity, alldiam)

# Scatter plot between BulkDensity and diamLess2
plot(BulkDensity, diamLess2, 
     main = "Bulk Density vs diamLess2",
     xlab = "Bulk Density", 
     ylab = "diamLess2",
     pch = 19)

# Scatter plot between BulkDensity and diamMore2
plot(BulkDensity, diamMore2, 
     main = "Bulk Density vs diamMore2",
     xlab = "Bulk Density", 
     ylab = "diamMore2",
     pch = 19)

# Scatter plot between BulkDensity and alldiam
plot(BulkDensity, alldiam, 
     main = "Bulk Density vs alldiam",
     xlab = "Bulk Density", 
     ylab = "alldiam",
     pch = 19)

# Linear regression of BulkDensity on diamLess2
linregBDl2 <- lm(BulkDensity ~ diamLess2)
summary(linregBDl2)

# Linear regression of BulkDensity on diamMore2
linregBDm2 <- lm(BulkDensity ~ diamMore2)
summary(linregBDm2)

# Linear regression of BulkDensity on alldiam
linregBDall <- lm(BulkDensity ~ alldiam)
summary(linregBDall)

# Plot with regression line for diamLess2
plot(diamLess2, BulkDensity,
     main = "Bulk Density vs diamLess2 with Regression Line",
     xlab = "diamLess2",
     ylab = "Bulk Density",
     pch = 19)
abline(linregBDl2, col = "blue")

# Plot with regression line for diamMore2
plot(diamMore2, BulkDensity,
     main = "Bulk Density vs diamMore2 with Regression Line",
     xlab =  "diamMore2",
     ylab = "Bulk Density",
     pch = 19)
abline(linregBDm2, col = "blue")

# Plot with regression line for alldiam
plot(alldiam, BulkDensity,
     main = "Bulk Density vs alldiam with Regression Line",
     xlab = "alldiam",
     ylab = "Bulk Density",
     pch = 19)
abline(linregBDall, col = "blue")


par(mar = c(6,4,4, 2))
barplot(BulkDensity~SampleID, ylim=c(0,140), las = 2, cex.names = 1, xlab="", ylab="Bulk Density (g/100cm3)")

summary(BulkDensity)
sd(BulkDensity)

dev.new(width=2, height=5)
boxplot(BulkDensity, ylim=c(0,140),  ylab="Bulk Density (g/100cm3)")
dev.off()

mean_value <- mean(BulkDensity)
std_dev <- sd(BulkDensity)

# Define the range within one standard deviation from the mean
lower_bound <- mean_value - std_dev
upper_bound <- mean_value + std_dev

# Count how many values fall within this range
within_one_sd <- BulkDensity[BulkDensity >= lower_bound & BulkDensity <= upper_bound]
percent_within_one_sd <- (length(within_one_sd) / length(BulkDensity)) * 100

# Output the result
percent_within_one_sd




#cor BD soil moisture
cor(BulkDensity, PercentSOM)
plot(BulkDensity, PercentSOM, 
     main = "Bulk Density vs PercentSOM",
     xlab = "Bulk Density", 
     ylab = "PercentSOM",
     pch = 19)
linregBDSM <- lm(BulkDensity ~ PercentSOM)
summary(linregBDSM)

mregBDSM <- lm(BulkDensity ~ PercentSOM+diamLess2)
summary(mregBDSM)
