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

#Origin <- c("Native", "Non native") #But this doesnt work because it is not attached to the data
Origin <- c(names(Pollen_Flower$Origin)[1 : 2])
Origin <- c("Native", "Nonative")

Flower.density <- 1

df <- data.frame(Pollen_Flower$Flower_density,Pollen_Flower$Origin)

df

#sum(df$Pollen_Flower.Flower_density) #This doesnt work because it doesnt distinguish between Origin

table(row.name = Pollen_Flower$Origin) 


x <- Pollen_Flower$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Flower$Flower_density)
x2 <- character()
for (i in Pollen_Flower$Origin) x2 <- c(x2,rep(i,4))

d2 <- data.frame (Origin = x2,Flower.density = x)

i <- 1
for (i in 1:128) {
  
  a <- df[df$Pollen_Flower.Flower_density == d2$Species[i], d2$Variables[i]]
  d2$mean[i] <- mean(a)
  d2$sd[i] <- sd(a)
  d2$median[i] <- median(a)
  d2$min[i] <-  min(a)
  d2$max[i] <-  max(a)
}

#What we want to accomplish, a graph with the total flower sensity per origin
#plot (df$Pollen_Flower.Origin, df$Pollen_Flower.Flower_density, "Flower density", "Origin")
