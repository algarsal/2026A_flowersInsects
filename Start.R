#2026A_flowersInsects: Ana Vilaplana (vilapl-ana), Shabrin Jahan Shaili (Shabrin-Jahan) and Md.Asif Ibne Habib (asifih79-hue)
#Explanation of the script

#libraries

load("Data/Flowers.Rdata")

#Flower density per plot vs. Origin of plants (native, origin)
Pollen_Flower$Flower_density 
Pollen_Flower$Origin

#Origin <- c("Native", "Non native") #But this doesnt work because it is not attached to the data
Origin <- c(names(Pollen_Flower$Origin)[1 : 2])

df <- data.frame(Pollen_Flower$Flower_density,Pollen_Flower$Origin)
df
sum(df$Pollen_Flower.Flower_density)

table(row.name = Pollen_Flower$Origin)

plot (df$Pollen_Flower.Origin, df$Pollen_Flower.Flower_density, "Flower density", "Origin")

