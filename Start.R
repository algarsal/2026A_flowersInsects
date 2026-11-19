#2026A_flowersInsects: Ana Vilaplana (vilapl-ana), Shabrin Jahan Shaili (Shabrin-Jahan) and Md.Asif Ibne Habib (asifih79-hue)
#Explanation of the script

#libraries

load("Data/Flowers.Rdata")

#Flower density per plot vs. Origin of plants (native, origin)
Pollen_Flower$Flower_density 
Pollen_Flower$Origin

df <- data.frame(Pollen_Flower$Flower_density,Pollen_Flower$Origin)
df
summary(df)
table(row.name = Pollen_Flower$Origin)
