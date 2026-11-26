#2026A_flowersInsects: Ana Vilaplana (vilapl-ana), Shabrin Jahan Shaili (Shabrin-Jahan) and Md.Asif Ibne Habib (asifih79-hue)
#Trying to make the first graph Flower density per plot vs. Origin of plants (native, origin)

library(ggplot2)
library(dplyr)
library(tidyr)
library(multcompView)
library(broom)

load("Data/Flowers.Rdata")

#Flower density per plot vs. Origin of plants (native, origin)
Pollen_Flower$Flower_density 
Pollen_Flower$Origin
#In the article it says: To get flower density per plot, we multiplied the estimated number of flowers per plant by the number of plants per plot (2 or 3).
#Is this the value we already have? Let's see other data sources:
Pollen_Plot$Flower_density
Pollen_Plot$Origin

Pollen_Protein$Flower_density
Pollen_Protein$Origin

#Origin <- c("Native", "Non native") #But this doesnt work because it is not attached to the data
#Origin <- c("Native", "Nonative")
#df1 <- data.frame(Pollen_Flower$Flower_density,Pollen_Flower$Origin)
#sum(df$Pollen_Flower.Flower_density) #This doesnt work because it doesnt distinguish between Origin
#table(row.name = Pollen_Flower$Origin) 



x <- Pollen_Flower$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Flower$Flower_density)
x2 <- character()
for (i in Pollen_Flower$Origin) x2 <- c(x2,i)

d02 <- data.frame (Origin = x2,Flower.density = x)
d02$Origin <- as.factor(d02$Origin)

plot(Flower.density ~ Origin, data = d02)

d03 <- d02 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d03, col = Origin)

#Now, let's do the same with the other two data sets:

x1 <- Pollen_Plot$Flower_density
for (i in 1:2) Origin <- c(x1,Pollen_Plot$Flower_density)
x12 <- character()
for (i in Pollen_Plot$Origin) x12 <- c(x12,i)

d12 <- data.frame (Origin = x12,Flower.density = x1)
d12$Origin <- as.factor(d12$Origin)

plot(Flower.density ~ Origin, data = d12) 

d13 <- d12 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d13, col = Origin)

#Still doesn't look similar to the article, let's try with the last one...

x3 <- Pollen_Protein$Flower_density
for (i in 1:2) Origin <- c(x3,Pollen_Protein$Flower_density)
x22 <- character()
for (i in Pollen_Protein$Origin) x22 <- c(x22,i)

d22 <- data.frame (Origin = x22,Flower.density = x3)
d22$Origin <- as.factor(d22$Origin)

plot(Flower.density ~ Origin, data = d22) 

d23 <- d22 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d23, col = Origin)

#I don't understand, each bar plot is different. Let's try bar graphs and SE for all 3: 

Pollen_Flower$Origin <- as.factor(Pollen_Flower$Origin)
d03 <- Pollen_Flower |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density, na.rm = TRUE)) #We couldn't figure out why this gives an error and that is why we are doing the next: 

d03 <- data.frame(Origin = levels(Pollen_Flower$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Flower$Flower_density[Pollen_Flower$Origin == d03$Origin[i]]
  d03$Flower.density[i] <- mean(ss)
  d03$SE[i] <- sd(ss)/sqrt(length(ss))
}

barplot(Flower.density ~ Origin, data = d03) #How can we add SE here? Well, let's try ggplot

ggplot(d03) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="blue", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.4, colour="black", alpha=0.9, size=1.3)

#Next one:

d13 <- data.frame(Origin = levels(Pollen_Plot$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Plot$Flower_density[Pollen_Plot$Origin == d13$Origin[i]]
  d13$Flower.density[i] <- mean(ss)
  d13$SE[i] <- sd(ss)/sqrt(length(ss))
}

ggplot(d13) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="purple", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.4, colour ="black", alpha=0.9, size=1.3)

#Next one:

d23 <- data.frame(Origin = levels(Pollen_Plot$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Protein$Flower_density[Pollen_Protein$Origin == d23$Origin[i]]
  d23$Flower.density[i] <- mean(ss)
  d23$SE[i] <- sd(ss)/sqrt(length(ss))
}

ggplot(d23) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="green",
            alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), 
                 width=0.4, colour ="black", alpha=0.9, size=1.3)
library(ggplot2)
library(ggsignif)

# Example summary data from emmeans
library(ggpubr)
df <- data.frame(
  Origin = c("Native", "Introduced", "Cultivar"),
  response = c(12.5, 18.2, 10.1),
  asymp.LCL = c(10.0, 15.1, 8.2),
  asymp.UCL = c(15.0, 21.3, 12.0)
)

ggplot(df, aes(x = Origin, y = response, fill = Origin)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_signif(
    comparisons = list(
      c("Native", "Introduced"),   # Pair 1
      c("Introduced", "Cultivar")  # Pair 2
    ),
    annotations = c("**", "***"),  # Labels from your emmeans p-values
    y_position = c(23, 26),        # Height of each bracket on the y-axis
    tip_length = 0.02,
    textsize = 5
  ) +
  ylim(0, 29) +                    # Expand y-axis so brackets don't get cut off
  theme_classic()

ggplot(d23, aes(x = Origin, y = Flower.density, fill = Origin)) +
  geom_col(width = 0.6) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, 
                     ymax=Flower.density+SE), width = 0.2) +
  annotate("text",
    x=1.5,y=600,label = "***",

    size = 5
  ) +                   # Expand y-axis so brackets don't get cut off
  theme_bw()+
  theme(legend.position = "none")



#still working on this: check the article!!!
install.packages("glmmTMB")
library (glmmTMB)
(m1 <- glmmTMB(Flower_density ~ Origin + Origin * Irrigation + (1 | Site/Plot) + (1 | Year),
               family=nbinom2, data=Pollen_Flower))
summary(m1)
anova(m1)
predict(m1)
#predict(budworm.lg, data.frame(ldose = ld,
sex = factor(rep("M", length(ld)), levels = levels(sex))),
type = "response")
#change the name of the variables and try to predict


#What we want to accomplish, a graph with the total flower sensity per origin
#plot (df$Pollen_Flower.Origin, df$Pollen_Flower.Flower_density, "Flower density", "Origin")
=======
>>>>>>> e4410b2bf9307192ea70b27de12f532a8e8ef2db


x <- Pollen_Flower$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Flower$Flower_density)
x2 <- character()
for (i in Pollen_Flower$Origin) x2 <- c(x2,i)

d02 <- data.frame (Origin = x2,Flower.density = x)
d02$Origin <- as.factor(d02$Origin)

plot(Flower.density ~ Origin, data = d02)

d03 <- d02 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d03, col = Origin)

#Now, let's do the same with the other two data sets:

x1 <- Pollen_Plot$Flower_density
for (i in 1:2) Origin <- c(x1,Pollen_Plot$Flower_density)
x12 <- character()
for (i in Pollen_Plot$Origin) x12 <- c(x12,i)

d12 <- data.frame (Origin = x12,Flower.density = x1)
d12$Origin <- as.factor(d12$Origin)

plot(Flower.density ~ Origin, data = d12) 

d13 <- d12 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d13, col = Origin)

#Still doesn't look similar to the article, let's try with the last one...

x3 <- Pollen_Protein$Flower_density
for (i in 1:2) Origin <- c(x3,Pollen_Protein$Flower_density)
x22 <- character()
for (i in Pollen_Protein$Origin) x22 <- c(x22,i)

d22 <- data.frame (Origin = x22,Flower.density = x3)
d22$Origin <- as.factor(d22$Origin)

plot(Flower.density ~ Origin, data = d22) 

d23 <- d22 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d23, col = Origin)

#I don't understand, each bar plot is different. Let's try bar graphs and SE for all 3: 

Pollen_Flower$Origin <- as.factor(Pollen_Flower$Origin)
d03 <- Pollen_Flower |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density, na.rm = TRUE)) #We couldn't figure out why this gives an error and that is why we are doing the next: 

d03 <- data.frame(Origin = levels(Pollen_Flower$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Flower$Flower_density[Pollen_Flower$Origin == d03$Origin[i]]
  d03$Flower.density[i] <- mean(ss)
  d03$SE[i] <- sd(ss)/sqrt(length(ss))
}

barplot(Flower.density ~ Origin, data = d03) #How can we add SE here? Well, let's try ggplot

ggplot(d03) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="blue", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.4, colour="black", alpha=0.9, size=1.3)

#Next one:

d13 <- data.frame(Origin = levels(Pollen_Plot$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Plot$Flower_density[Pollen_Plot$Origin == d13$Origin[i]]
  d13$Flower.density[i] <- mean(ss)
  d13$SE[i] <- sd(ss)/sqrt(length(ss))
}

ggplot(d13) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="purple", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.4, colour ="black", alpha=0.9, size=1.3)

#Next one:

d23 <- data.frame(Origin = levels(Pollen_Plot$Origin), 
                  Flower.density = NA,
                  SE = NA)
for (i in 1:2) {
  ss <- Pollen_Protein$Flower_density[Pollen_Protein$Origin == d23$Origin[i]]
  d23$Flower.density[i] <- mean(ss)
  d23$SE[i] <- sd(ss)/sqrt(length(ss))
}

ggplot(d23) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="green", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.2, colour = "darkgray", alpha=0.9, size=0.6)+
theme_bw()+
  ylim(0,700)


#still working on this: check the article!!!
install.packages("glmmTMB")
library (glmmTMB)
(m1 <- glmmTMB(Flower_density ~ Origin + Origin * Irrigation + (1 | Site/Plot) + (1 | Year),
               family=nbinom2, data=Pollen_Flower))
summary(m1)
anova(m1)
predict(m1)
#predict(budworm.lg, data.frame(ldose = ld,
sex = factor(rep("M", length(ld)), levels = levels(sex))),
type = "response")
#change the name of the variables and try to predict


#What we want to accomplish, a graph with the total flower sensity per origin
#plot (df$Pollen_Flower.Origin, df$Pollen_Flower.Flower_density, "Flower density", "Origin")
