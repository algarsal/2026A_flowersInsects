
#the file of nectarperflower.csv is not the best option to get our data, best use this code

Webcsv <- "https://dfzljdn9uc3pi.cloudfront.net/2026/20906/1/pollinatorsflowersperplot.csv"
Flowers <- read.table(Webcsv, header = T, sep = ",")
