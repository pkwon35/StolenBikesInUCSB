library(data.table)
library(base)

# assign and read data set
bikesStolen <- read.csv("C:/Users/Eri/Documents/Data Science/BikesStolenUCSB/bikesStolenUCSB.csv", header = TRUE)
bikesStolen <- data.table(bikesStolen)
head(bikesStolen)

#check the structure of the dataset
str(bikesStolen)

#regular expression
bikesStolen$Location <- as.character(bikesStolen$Location)
#changes factor variables to character

find.anacapa <- agrep("anacapa", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.anacapa] <- "ANACAPA"

find.arbor <- agrep("arbor", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.arbor] <- "ARBOR"

find.library <- agrep("library", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.library] <- "LIBRARY"

find.bren <- grep("bren", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.bren] <- "BREN"

find.broida <- agrep("broida", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.broida] <- "BROIDA"

find.campbell <- agrep("campbell", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.campbell] <- "CAMPBELL"

find.carillo <- agrep("carillo", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.carillo] <- "CARILLO"

find.chemistry <- agrep("chemistry", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.chemistry] <- "CHEMISTRY"

find.harder <- agrep("harder", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.harder] <- "HARDERSTADIUM"

find.hssb <- agrep("hssb", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.hssb] <- "HSSB"

find.music <- agrep("music", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.music] <- "MUSIC"

find.rec <- agrep("rec cen", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.rec] <- "RECREATIONCENTER"
find.recreation <- agrep("recreation", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.recreation] <- "RECREATIONCENTER"

find.srb <- agrep("srb", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.srb] <- "SRB"
find.student <- agrep("student resource", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.student] <- "SRB"

find.catalina <- agrep("catalina", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.catalina] <- "SANTACATALINA"

find.cruz <- agrep("cruz", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.cruz] <- "SANTACRUZ"

find.rosa <- agrep("rosa", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.rosa] <- "SANTAROSA"

find.ynez <- agrep("ynez", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.ynez] <- "SANTAYNEZ"
x <- "sya"
find.sya <- grep(x, bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.sya] <- "SANTAYNEZ"

find.dlg <- agrep("guerra", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.dlg] <- "DLG"

find.hfh <- agrep("harold", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.hfh] <- "HFH"

find.miguel <- agrep("miguel", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.miguel] <- "SANMIGOLAS"

find.nicolas <- agrep("nicolas", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.nicolas] <- "SANMIGOLAS"

find.arts <- grep("art", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.arts] <- "ARTS"

find.ortega <- agrep("ortega", bikesStolen$Location,ignore.case = TRUE)
bikesStolen$Location[find.ortega] <- "ORTEGA"

### adding new columns to the dataframe to put latitude and longtitude 
bikesStolen[,"Latitude"] <- NA 
bikesStolen[,"Longitude"] <- NA 

colnames(bikesStolen)

#make a for loop comparing the location col in bikesStolen to LocationName 

bikeParking <- read.csv("C:/Users/Eri/Documents/Data Science/BikesStolenUCSB/bikeParking_1.csv", header = TRUE)

for (i in 1:1326) {
  for (j in 1:45) {
    if (bikesStolen$Location[i] == bikeParking$LocationName[j]) {
      bikesStolen$Latitude[i] <- bikeParking$Latitude[j]
      bikesStolen$Longitude[i] <- bikeParking$longitude[j]
    }
     
  }
  
}

#for some reason they didn't get SRB so i will separately put the values in for that
for (i in 1:1326) {
  if (bikesStolen$Location[i] == "SRB") {
    bikesStolen$Latitude[i] <- 34.412778
    bikesStolen$Longitude[i] <- -119.851935
  }
}


#Geospatial part!!
library(ggplot2)
library(ggmap)
library(plotly)

mapUCSB <- get_map(location = "UCSB", zoom = 15, source = "google", maptype = "roadmap", crop = FALSE)
ggmap(mapUCSB)

#remove emtpy locations, works!
bikesStolen <- na.omit(bikesStolen)


#ravi's way
library(plyr)
density <- ddply(bikesStolen, .(Location), "nrow")  #dividing data frame
colnames(density) <- c("Location", "count") 
DF <- merge(bikesStolen, density, by = "Location")

circle_scale_amt = .35

ggmap(mapUCSB) + 
  geom_point(data=DF, 
             mapping = aes(x = Longitude, y = Latitude, colour = Location), 
             na.rm=TRUE, alpha = 1/40, 
             size = DF$count*circle_scale_amt) + 
  theme(legend.position="none") + 
  scale_size_continuous(range=range(DF$count)) + 
  ggtitle("Frequency of Bikes Stolen in UCSB", 
          subtitle = "2010-2017")


p <- ggmap(mapUCSB) + 
  geom_point(data=DF, 
             mapping = aes(x = Longitude, y = Latitude, colour = Location), 
             na.rm=TRUE, alpha = 1/40, 
             size = DF$count*circle_scale_amt) + 
  theme(legend.position="none") + 
  scale_size_continuous(range=range(DF$count)) + 
  ggtitle("Frequency of Bikes Stolen in UCSB", 
          subtitle = "2010-2017")

ggplotly(p)

#having trouble with the interactive part?

#making a bar graph to make the spatial map easier to interpret
bargraph <- ggplot(data = density) +
  geom_bar(aes (x = Location, y = freq), fill = "#0099cc") +
  xlab("Location") + ylab("Frequency") + ggtitle("Frequency of Stolen Bikes")


ggplotly(bargraph)






  
