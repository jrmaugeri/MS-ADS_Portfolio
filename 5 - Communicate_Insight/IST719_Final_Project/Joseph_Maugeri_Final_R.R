### FILE INFO
# Author: Joseph Maugeri
# Class : IST 719
# Purpose : Final Project R Code to generate poster elements
# Version : 1.0
# 

### REQUIRING PACKAGES #########################################################
## Make sure you have installed the following packages
require(dplyr)
require(plotrix)
require(RColorBrewer)
require(ggplot2)
require(ggmap)
require(maps)
require(lattice)
require(scatterplot3d)
### Setting my dir and reading in file #########################################
"https://data.ny.gov/Energy-Environment/Spill-Incidents/u44d-k5fk/data"
my.dir <-  "c:\\Users\\Maugeri\\Desktop\\IST719\\"

### READING IN THE SPILLS DATASET ##############################################

spills <- read.csv(file=paste0(my.dir
                              , "Spill_Incidents.csv")
                  , header = TRUE
                  , stringsAsFactors = FALSE
                  , na.strings =c(""," ","NA"))

sp <- spills
### PRE-ANALYSIS TRANSFORMATIONS ###############################################

# Transforming Dates 
sp$Spill.Date <- as.Date(sp$Spill.Date, format = "%m/%d/%Y", optional = TRUE)
sp$Received.Date <- as.Date(sp$Received.Date, format = "%m/%d/%Y", optional = TRUE)
sp$Close.Date <- as.Date(sp$Close.Date, format = "%m/%d/%Y", optional = TRUE)
sp$Year.Month.Rec. <- format(sp$Received.Date, "%Y-%m")
sp$Year.Rec. <- format(sp$Received.Date, "%Y")
sp$Year.Month.Spill <- format(sp$Spill.Date, "%Y-%m")
sp$Year.Spill <- format(sp$Spill.Date, "%Y")

# Reducing size of sp dataset to last 20 years
sp <- subset.data.frame(sp, Spill.Date > "2000-01-01")

# Dropping rows with "Missing Code in Old Data = Must be Fixed"
nrow(sp)

src_list <- sp$Source
src_list <- src_list %>% unique()
print( src_list)

# removing the 14th index of the list ( which is missing code in data)
sp <- sp[sp$Source != src_list[14],]
# checking to see if there are any rows left
nrow(sp[sp$Source == src_list[14],])


# creating datediff columns for received date and closed date from spill date
sp$Diff.Days.O2C = as.numeric(difftime(sp$Close.Date, sp$Spill.Date, units = "days")) 
sp$Diff.Days.R2C = as.numeric(difftime(sp$Received.Date, sp$Spill.Date, units = "days")) 
spdaycalc1 <- sp$Diff.Days.O2C
for (i in nrow(spdaycalc1))
{
  if (spdaycalc1[i]<0) {spdaycalc1[i] = 0}
  else{spdaycalc1[i] = spdaycalc1[i]}
}
sp$Diff.Days.O2C <- spdaycalc1

# cleaning the datediff columns

spdaycalc2 <- sp$Diff.Days.R2C
for (i in nrow(spdaycalc2))
{
  if (spdaycalc2[i]<0) {spdaycalc2[i] = 0}
  else{spdaycalc2[i] = spdaycalc2[i]}
}
sp$Diff.Days.R2C <- spdaycalc2

# Moving Outliers of all unit data to a new dataset. 
sp_TopSpills <- sp[sp$Quantity > 2000000,]

sp <- sp[sp$Quantity < 2000000,]

# Dividing into separate dataframes by units 
spg <- sp[sp$Units == "Gallons",]
splb <- sp[sp$Units == "Pounds",]
sp_ <- sp[is.na(sp$Units) == TRUE,]

### Testing if my data passes the formula this must be greater than 100 (it is 200)
str(sp)
summary(sp)
(ncol(sp) * 4) * (nrow(sp)/100)
sp[sp$Quantity > 10000000000,]

### TIMELINE OF TOP 20 SPILLS ##################################################
sp_TopSpills <- sp_TopSpills[order(-sp_TopSpills$Quantity),]
topIDs <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
sp_TopSpills$IDdec <- topIDs 
#massive outliers to point out
LargestSpill <- sp_TopSpills[1,]
SpillOutliers <- sp_TopSpills[1:5,]
#removing the outlier for the visualization
sp_TopSpills <- sp_TopSpills[-1,]

#text for visualization

sp_TopSpills$Text <- sp_TopSpills %>% paste0(c(sp_TopSpills$IDdec))

#sp_TopSpills$IDdecsrc <- c(sp_TopSpills[,27],"-",sp_TopSpills[,14])
sp_out <- ggplot(SpillOutliers, aes(x = Spill.Date
                                        , y = Quantity
                                        , col = Units
                                        ,label = Material.Name)) +
  geom_point(stat = "identity") + 
  scale_color_manual(breaks = sp$Units, values = c("#6DE800","#E7004A","#FFFFFF")) +
  ggtitle("5 LargestS Spills In New York (Outliers)") +
  geom_text(hjust=-.15, vjust=.45) +
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .75, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 10)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = 1.2, size = 40))

sp_out

sp_t20point <- ggplot(sp_TopSpills, aes(x = Spill.Date
                                        , y = Quantity
                                        , color = Units
                                        ,label = Material.Name)) +
  geom_point(stat = "identity") + 
  scale_color_manual(breaks = sp$Units, values = c("#6DE800","#E7004A","#FFFFFF")) +
  ggtitle("Top 20 Spills since 2000") +
  geom_text(hjust=-.15, vjust=.9) +
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .75, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 40))

sp_t20point

### BOXPLOT TO SHOW DISTRIBUTIONS BY UNITS #####################################

sp_box <- ggplot(sp, aes(x = Quantity, y = Units, group = Units)) +
  geom_boxplot(aes(x = Quantity, y = Units, group = Units, color = Units)) +
  scale_color_manual(breaks = sp$Units, values = c("#6DE800","#E7004A","#FFFFFF")) +
  ggtitle("Distribution of Spills by Units") +
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 40))

sp_box

### MORE EXPLORATORY GRAPHS ####################################################

spplot <- ggplot(sp)+
  geom_line( aes(x = Year.Spill, y = Quantity,color = Material.Family)) +
  scale_color_manual(breaks = sp$Material.Family, values = c("#30A100","#190B6E","#95001A","#9F7E00")) +
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 40))
spplot


### BAR CHARTs OF QUANTIY BY SOURCE #############################################
# All units
sp_bar <- ggplot(sp,aes( x = Source, y = Quantity, group = Material.Family)) +
  geom_bar(stat="identity",aes(fill = Material.Family))+
  scale_fill_manual(breaks = sp$Material.Family, values = c("#30A100","#201082","#B0001F","#BC9500")) +
  coord_flip()+
  labs(title = "Quantity Spilled From All Unit Sources")+
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 40))
sp_bar
# GALLON UNITS
spg_bar <- ggplot(spg,aes( x = Source, y = Quantity, group = Material.Family)) +
  geom_bar(stat="identity",aes(fill = Material.Family))+
  scale_fill_manual(breaks = sp$Material.Family, values = c("#30A100","#201082","#B0001F","#BC9500")) +
  coord_flip()+
  labs(title = "Quantity Spilled From Gallon Sources")+
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 20))
spg_bar

# POUND UNITS
splb_bar <- ggplot(splb,aes( x = Source, y = Quantity, group = Material.Family)) +
  geom_bar(stat="identity",aes(fill = Material.Family))+
  scale_fill_manual(breaks = sp$Material.Family, values = c("#30A100","#201082","#B0001F","#BC9500")) +
  coord_flip()+
  labs(title = "Quantity Spilled From Pound Sources")+
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 20))
splb_bar

# NA UNITS
sp__bar <- ggplot(sp_,aes( x = Source, y = Quantity, group = Material.Family)) +
  geom_bar(stat="identity",aes(fill = Material.Family))+
  scale_fill_manual(breaks = sp$Material.Family, values = c("#30A100","#201082","#B0001F","#BC9500")) +
  coord_flip()+
  labs(title = "Quantity Spilled From NA Units Sources")+
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .5, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold",vjust = 1.2, size = 12)
        ,axis.title = element_text(family = "serif", face = "bold",vjust = 1.2, size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", hjust = .9, size = 20))
sp__bar
### BUBBLE PLOT OF SPILLS for GALLONS ##########################################

spg_bub <- ggplot(spg, aes(x = Spill , y = Quantity,size = Quantity, color= Material.Family))+
  geom_point(alpha = 0.7)
       
ordered <- spg[order(spg$Quantity, decreasing = TRUE), ]    
splits <- split(ordered, ordered$County)
heads <- lapply(splits, head)
do.call(rbind, heads)


#### AGGREGATING QUANTITY BY UNITS, COUNTY #####################################
# Summarizing by County and Units
spGRP <- sp %>%
  dplyr::group_by(Units, County) %>%
  dplyr::summarize(ctotSpill = sum(Quantity), cavgSpill = mean(Quantity))
# Initial Plot to see the data
ggplot(spGRP, aes(x = County, y = ctotSpill, color = Units)) +
  geom_point() +
  coord_flip() +
  xlab("Date") + ylab("Total Spilled") +
  ggtitle("Relationship Between County and Spillage") 

# Creating Aggregate by County regardless of units, for sort column
spGRP_Tot <- sp %>%
  dplyr::group_by(County) %>%
  dplyr::summarize( C_Total = sum(Quantity))
# Using plyr to join the total column to the double grouped aggregate
spGRP_J <- plyr::join(spGRP, spGRP_Tot, by = "County")
# Using plyr to arrange by the Grand total that is regardless of Units
spGRP_J <- plyr::arrange(spGRP_J,by_group= desc(C_Total))

library(forcats)
# using forcats to convert to an ordered factor for graphing
spGRP_J$County <- factor(spGRP_J$County, levels = rev(unique(spGRP_J$County)), ordered=TRUE)

spGRP_J %>%
  ggplot( aes(x = County, y = C_Total)) +
  geom_point( aes(x = County, y = ctotSpill, color = Units)) +
  coord_flip() +
  xlab("County") + ylab("Total Spilled") +
  ggtitle("Relationship Between County and Spillage")+
  scale_color_manual(breaks = sp$Units, values = c("#6DE800","#E7004A","#FFFFFF"))+
  theme(axis.ticks.y = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major.y = element_line(linewidth = .05, color = "#8B0000" )
        ,panel.grid.major.x = element_line(linewidth = .05, color = "#8B0000" )
        ,panel.background = element_rect(fill = c("#FFF5EE"))
        ,plot.background = element_rect(fill = c("#DEEBFF"))
        ,axis.ticks.y.left = element_line(linewidth = 1.5)
        ,axis.title.x.bottom = element_text(family = "serif", face = "bold", size = 15)
        ,axis.title.y.left = element_text(family = "serif", face = "bold", size = 15)
        ,axis.text = element_text(family = "serif", face = "bold", size = 10)
        ,axis.title = element_text(family = "serif", face = "bold", size = 20)
        ,plot.title = element_text(family = "serif", face = "bold", size = 20))

# Simple plot of total over time ( not relevant for graphing)
spPl1 <- ggplot(spGRP_J %>% dplyr::arrange(C_Total), aes(x = County, y = C_Total, group = ctotSpill)) +
  geom_point() +
  coord_flip() +
  xlab("County") + ylab("Total Spilled") +
  ggtitle("Relationship Between County and Spillage") 

### MAPPING IN NEW YORK STATE SECTION ##########################################
spg_county_agg$Group.1 <- tolower(spg_county_agg$Group.1)
spg_county_agg$Group.1 <- paste0("new york,",spg_county_agg$Group.1)
# map stored in m
m <- map("county", regions = "New York", boundary =  TRUE,
         )
# matching county database to spg_county_agg data
# nymap <- map(database = "county", region="New York")
state.order <- match.map(database = "county", regions = spg_county_agg$Group.1
                         , exact = TRUE, warn = TRUE)
state.order[is.na(state.order)] <- 0

### Generating Colors for Heatmap of Quantity spilled by county
library(plotrix)
library(dplyr)
library(maps)
library(ggmap)
library(RColorBrewer)
library(vctrs)
library(rlang)
library(tidygeocoder)
# Choosing colors for scale
cols <- colorRampPalette(c("seashell","red4"))
# choosing number of colors in scale
num.cols <- 7
# creating color frame of num.cols
ny_cols <- cols(num.cols)
# rescaling x to create color index

spg_county_agg$colindex2 <- cut(x = spg_county_agg$x
                                ,breaks = c(0
                                            ,100
                                            ,1000 #0ne
                                            ,100000 #two

                                            ,1000000 #four

                                            ,2500000 #six

                                            ,5000000
                                            #eight
                                            ))
# using color frame to apply colors

spg_county_agg$color2 <- ny_cols[spg_county_agg$colindex2]
# Placing the data on a map
par(mar = c(1,1,1,1))
spg_c <- map("county", regions = "New York", fill = TRUE
            , col = spg_county_agg$color2
            , resolution = 0, lty = 1, lwd = 3
            , projection = "polyconic", border = "black")
leg.txt <- c("0 to 100","100 to 1000","100000 to 1e+6","1e+6 to 2.5e+6","2.5e+6 to 5e+6")
leg.txt
legend("topright",y = NULL
       , leg.txt, horiz = FALSE
       , fill = ny_cols[1:4]
       , cex = 1 
       , title = "Gallon Bins"
        )

"https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2"
# create a dataframe with addresses
zipfix <- c("11432","10470","11210","13126","11768","11557","11572","13205","13205","14219"
            ,"12550","12553","13126","11510","11433","14201","10583","13478","12601")
sp_TopSpills$ZIP.Code <- zipfix
addylist <- paste0(sp_TopSpills$Street.1,", ",sp_TopSpills$County,", ","NY")
sp_TopSpills$addy <- paste0(sp_TopSpills$Street.1,", ",sp_TopSpills$County,", ","NY")
# geocode the addresses
lat_longs <- sp_TopSpills %>%
  geocode(addy, method = 'osm', lat = latitude , long = longitude)
#> Passing 3 addresses to the Nominatim single address geocoder
#> Query completed in: 3 seconds

?geocode

geocode(sp_TopSpills, address = addy, method = "osm", full_results)

nymap1 <- ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("county") + geom_point() +
  ggrepel::geom_label_repel(aes(label = Spill.Number)) +
  theme_void()

### Another attempt at a map 

spg_county_agg %>%
  ggplot(aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = NA) +
  scale_fill_gradient(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, colors = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urban_map()



table(libs$CITY)
index <- which(libs$CITY %in% c("SYRACUSE","DEWITT", "FAYETTEVILLE"))
addy <- paste(libs$ADDRESS[index],libs$CITY[index],libs$STABR[index]
              , sep = ", ")
addy

###

map("county","new york", fill = TRUE, col = "orange")
g.codes <- geocode(addy, source = "DSK")
points(g.codes$lon, g.codes$lat, col = "blue", cex = 1.1, pch = 16)

       
