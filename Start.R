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

Origin <- c("Native", "Nonative")



df <- data.frame(Pollen_Flower$Flower_density,Pollen_Flower$Origin)

df

#sum(df$Pollen_Flower.Flower_density) #This doesnt work because it doesnt distinguish between Origin

table(row.name = Pollen_Flower$Origin) 


x <- Pollen_Flower$Flower_density 
for (i in 1:2) Origin <- c(x,Pollen_Flower$Flower_density)
x2 <- character()
for (i in Pollen_Flower$Origin) x2 <- c(x2,i)

d2 <- data.frame (Origin = x2,Flower.density = x)
d2$Origin <- as.factor(d2$Origin)
plot(Flower.density ~ Origin, data = d2)

d3 <- d2 |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density), 
            SE = sd(Flower.density))

barplot(Flower.density ~ Origin, data = d3, col = Origin)

Pollen_Plot$Origin <- as.factor(Pollen_Plot$Origin)
d3 <- Pollen_Plot |> 
  group_by(Origin) |> 
  summarise(Flower.density = mean(Flower.density, na.rm = TRUE))

d3 <- data.frame(Origin = levels(Pollen_Plot$Origin), 
                 Flower.density = NA,
                 SE = NA)
for (i in 1:2) {
  ss <- Pollen_Plot$Flower_density[Pollen_Plot$Origin == d3$Origin[i]]
  d3$Flower.density[i] <- mean(ss)
  d3$SE[i] <- sd(ss)/sqrt(length(ss))
}
  

barplot(Flower.density ~ Origin, data = d3)


ggplot(d3) +
  geom_bar( aes(x=Origin, y=Flower.density), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Origin, ymin=Flower.density-SE, ymax=Flower.density+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)

#What we want to accomplish, a graph with the total flower sensity per origin
#plot (df$Pollen_Flower.Origin, df$Pollen_Flower.Flower_density, "Flower density", "Origin")
