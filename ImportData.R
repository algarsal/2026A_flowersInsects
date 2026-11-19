
#the file of nectarperflower.csv is not the best option to get our data, best use this code

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollinatorsflowersperplot.csv"
Flowers <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollinators.diversity.csv"
Pollinators <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/nectarperflower.csv"
Nectar_Flower <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/nectarperplots.csv"
Plot_Nectar <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollenperflower.csv"
Pollen_Flower <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollenperplot.csv"
Pollen_Plot <- read.table(Webcsv, header = T, sep = ",")

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollen_protein.csv"
Pollen_Protein <- read.table(Webcsv, header = T, sep = ",")




save(Flowers, Pollinators, Nectar_Flower, Plot_Nectar, Pollen_Flower, Pollen_Plot, Pollen_Protein, file = "Data/Flowers.Rdata")
