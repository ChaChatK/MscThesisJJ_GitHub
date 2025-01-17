HedgeAge=c("c", "c", 3, 28, 200, 3, 10, 17, 28, 200, 10, 28, 200, 10, 17, 28, 200)
Distance=c("c", "c", 0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1.5, 1.5, 1.5, 2, 2, 2, 2)
SoilMoisture=c(54.94, 47.42, 49.54, 45.98, 34.67, 48.56, 48.18, 47.05, 46.32, 42.88, 48, 44.57, 39.78, 47.42, 47.82, 44.29, 41.55)

df <- data.frame(HedgeAge,SoilMoisture)
df$HedgeAge <- as.character(df$HedgeAge)
str(df)

boxplot(SoilMoisture~HedgeAge, data=df)
df$HedgeAge <- factor(df$HedgeAge, levels=c("c", "3", "10", "17", "28", "200"))
boxplot(SoilMoisture~HedgeAge, data=df)                     

#One-way Anova test
AgeMoisture.aov <- aov(SoilMoisture~HedgeAge, data=df)
summary(AgeMoisture.aov)

#Post-hoc Test Tukey kramer
AgeMoisture.Tukey <- TukeyHSD(AgeMoisture.aov, conf.level=.90)
AgeMoisture.Tukey


#multiple regression
HedgeAgeNUM = c(0, 0, 3, 28, 200, 3, 10, 17, 28, 200, 10, 28, 200, 10, 17, 28, 200)
DistanceNUM = c(0, 0, 0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1.5, 1.5, 1.5, 2, 2, 2, 2)
SoilMoisture = c(54.94, 47.42, 49.54, 45.98, 34.67, 48.56, 48.18, 47.05, 46.32, 42.88, 48, 44.57, 39.78, 47.42, 47.82, 44.29, 41.55)
                 
df <- data.frame(HedgeAgeNUM, DistanceNUM, SoilMoisture)                  

plot(df)    

mReg <- lm(SoilMoisture~HedgeAgeNUM+DistanceNUM, data=df)
summary(mReg)

summary(lm(SoilMoisture~HedgeAgeNUM, data=df))
cor(SoilMoisture,HedgeAgeNUM)

#cor with SOM?
PercentSOM = c(6.39453046450837, 5.37365177195685, 6.67205822887185, 8.10313075506448, 6.27522935779814, 6.30562393486086, 6.07625099285149, 7.23684210526317, 6.1839830996735, 6.19999999999997, 6.05095541401275, 5.81331377223547, 5.73977118479735, 8.13771517996864, 5.58733790999236, 6.70076726342712, 6.24883764180767)

cor(SoilMoisture, PercentSOM)
summary(lm(SoilMoisture ~ PercentSOM))

#cor with RLDs
cor(SoilMoisture, diamMore2)
summary(lm(SoilMoisture ~ diamMore2+diamLess2))
cor(SoilMoisture, diamLess2)
summary(lm(SoilMoisture ~ diamLess2))

#cor with BD
cor(SoilMoisture, BulkDensity)
summary(lm(SoilMoisture ~ BulkDensity))
