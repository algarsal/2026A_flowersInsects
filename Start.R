#2026A_flowersInsects: Ana Vilaplana (vilapl-ana), Shabrin Jahan Shaili (Shabrin-Jahan) and Md.Asif Ibne Habib (asifih79-hue)
#Explanation of the script

#libraries

load("Data/Flowers.Rdata")

#nectar_volume on irrigation
Plot_Nectar$Irrigation
Plot_Nectar$nectar_per_plot
df <- data.frame(Plot_Nectar$Irrigation,Plot_Nectar$nectar_per_plot)
df



