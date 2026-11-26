#2026A_flowersInsects: Ana Vilaplana (vilapl-ana), Shabrin Jahan Shaili (Shabrin-Jahan) and Md.Asif Ibne Habib (asifih79-hue)
#Trying to make the first graph Flower density per plot vs. Origin of plants (native, origin)

library(ggplot2)
library(dplyr)
library(tidyr)
library(multcompView)
library(broom)
library(ggsignif)
library(ggpubr)
install.packages("glmmTMB")
library (glmmTMB)
install.packages("viridis")

# Create a folder for all figures
if (!dir.exists("Figures")) {
  dir.create("Figures")
}

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

#For Pollen_Flower group of data:

#First, we make the data base with what we want to work: 
x <- Pollen_Flower$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Flower$Flower_density)
x2 <- character()
for (i in Pollen_Flower$Origin) x2 <- c(x2,i)

d02 <- data.frame (Origin = x2,Flower.density = x)
d02$Origin <- as.factor(d02$Origin)

d03 <- d02 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

for (i in 1:2) {
  ss <- Pollen_Flower$Flower_density[Pollen_Flower$Origin == d03$Origin[i]]
  d03$Flower.density[i] <- mean(ss)
  d03$SE[i] <- sd(ss)/sqrt(length(ss))
}
#We need to do the stadistics and check th p-value, we will need that information for the graph!
(m1 <- glmmTMB(Flower_density ~ Origin + Origin * Irrigation + (1 | Site/Plot) + (1 | Year),
               family=nbinom2, data=Pollen_Flower))
summary(m1) #This one is the important and from where we will get the p-values
predict(m1)

#Let's make the graph:
ad03 <- ggplot(d03, aes(x = Origin, y = Flower.density, fill = Origin)) +
  geom_col(width = 0.5) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, 
                     ymax=Flower.density+SE), width = 0.1) +
  annotate("text",
           x=1.5,y=800,label = "***", #Change this according to previous results
           
           size = 6
  ) +
  ylab("Flower density per plot") +
  xlab("Origin of plants") +
  scale_fill_viridis_d(option = "E") +
  theme_bw() +
  theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5))

print(ad03)

#Let's save the graph, change the name! 
ggsave(
  "Figures/Flower_density_Origin_d03.tiff",
  plot = ad03,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 600
)

#For Pollen_Plot group of data:

#First, we make the data base with what we want to work: 
x <- Pollen_Plot$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Plot$Flower_density)
x2 <- character()
for (i in Pollen_Plot$Origin) x2 <- c(x2,i)

d12 <- data.frame (Origin = x2,Flower.density = x)
d12$Origin <- as.factor(d12$Origin)

d13 <- d12 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

for (i in 1:2) {
  ss <- Pollen_Plot$Flower_density[Pollen_Plot$Origin == d13$Origin[i]]
  d13$Flower.density[i] <- mean(ss)
  d13$SE[i] <- sd(ss)/sqrt(length(ss))
}
#We need to do the stadistics and check th p-value, we will need that information for the graph!
(m2 <- glmmTMB(Flower_density ~ Origin + Origin * Irrigation + (1 | Site/Plot) + (1 | Year),
               family=nbinom2, data=Pollen_Plot))
summary(m2) #This one is the important and from where we will get the p-values
predict(m2)

#Let's make the graph:
ad13 <- ggplot(d13, aes(x = Origin, y = Flower.density, fill = Origin)) +
  geom_col(width = 0.5) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, 
                     ymax=Flower.density+SE), width = 0.1) +
  annotate("text",
           x=1.5,y=600,label = "***", #Change this according to previous results
           
           size = 6
  ) +
  ylab("Flower density per plot") +
  xlab("Origin of plants") +
  scale_fill_viridis_d(option = "E") +
  theme_bw() +
  theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5))

print(ad13)

#Let's save the graph, change the name! 
ggsave(
  "Figures/Flower_density_Origin_d13.tiff",
  plot = ad13,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 600
)

#For Pollen_Protein group of data:
#Note! The glmm gave errors, object Year and Site was not founded in Pollen_Protein, so I assume this is not the data they used for the one in the article and i dont think we can use it to compare it

#First, we make the data base with what we want to work: 
x <- Pollen_Protein$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Protein$Flower_density)
x2 <- character()
for (i in Pollen_Protein$Origin) x2 <- c(x2,i)

d22 <- data.frame (Origin = x2,Flower.density = x)
d22$Origin <- as.factor(d12$Origin)

d23 <- d22 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

for (i in 1:2) {
  ss <- Pollen_Protein$Flower_density[Pollen_Protein$Origin == d23$Origin[i]]
  d23$Flower.density[i] <- mean(ss)
  d23$SE[i] <- sd(ss)/sqrt(length(ss))
}
#We need to do the stadistics and check th p-value, we will need that information for the graph!
(m3 <- glmmTMB(Flower_density ~ Origin + Origin * Irrigation, #here object Year and Site was not founded
               family=nbinom2, data=Pollen_Protein))
summary(m3) #This one is the important and from where we will get the p-values
predict(m3)

#Let's make the graph:
ad23 <- ggplot(d23, aes(x = Origin, y = Flower.density, fill = Origin)) +
  geom_col(width = 0.5) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, 
                     ymax=Flower.density+SE), width = 0.1) +
  annotate("text",
           x=1.5,y=600,label = "*", #Change this according to previous results
           
           size = 6
  ) +
  ylab("Flower density per plot") +
  xlab("Origin of plants") +
  scale_fill_viridis_d(option = "E") +
  theme_bw() +
  theme(legend.position = "none", plot.margin = margin(5, 5, 5, 5))

print(ad23)

#Let's save the graph, change the name! 
ggsave(
  "Figures/Flower_density_Origin_d23.tiff",
  plot = ad23,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 600
)
