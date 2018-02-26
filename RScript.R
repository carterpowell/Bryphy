library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sp) # A package for spatial data


#Creates command: BIEN_occurrence_higher_plant_group
BIEN_occurrence_higher_plant_group <- function (higher_plant_group, cultivated = FALSE, only.new.world = TRUE, 
                                                print.query = FALSE, observation.type = FALSE, all.taxonomy = FALSE, 
                                                native.status = FALSE, political.boundaries = FALSE, ...) 
{
  is_char(higher_plant_group)
  is_log(cultivated)
  is_log(only.new.world)
  is_log(print.query)
  is_log(observation.type)
  is_log(all.taxonomy)
  is_log(native.status)
  is_log(political.boundaries)
  if (!cultivated) {
    cultivated_query <- "AND (is_cultivated = 0 OR is_cultivated IS NULL)"
    cultivated_select <- ""
  }
  else {
    cultivated_query <- ""
    cultivated_select <- ",is_cultivated,is_cultivated_in_region"
  }
  if (only.new.world) {
    newworld_query <- "AND is_new_world = 1 "
    newworld_select <- ""
  }
  else {
    newworld_query <- ""
    newworld_select <- ",is_new_world"
  }
  if (!all.taxonomy) {
    taxon_select <- ""
  }
  else {
    taxon_select <- "verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,taxonomic_status,scrubbed_author,"
  }
  if (!native.status) {
    native_select <- ""
  }
  else {
    native_select <- "native_status,native_status_reason,native_status_sources,isintroduced,native_status_country,native_status_state_province,native_status_county_parish,"
  }
  if (!observation.type) {
    observation_select <- ""
  }
  else {
    observation_select <- ",observation_type"
  }
  if (!political.boundaries) {
    political_select <- ""
  }
  else {
    political_select <- "country,state_province,county,locality,"
  }
  query <- paste("SELECT higher_plant_group,", taxon_select, native_select, 
                 political_select, "scrubbed_species_binomial, latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code", 
                 paste(cultivated_select, newworld_select, observation_select), 
                 "\n                 FROM view_full_occurrence_individual \n                 WHERE higher_plant_group in (", 
                 paste(shQuote(higher_plant_group, type = "sh"), collapse = ", "), 
                 ")", paste(cultivated_query, newworld_query), " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) ORDER BY scrubbed_species_binomial;")
  if (print.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
                  perl = TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
}

#Runs Command and stores as Data Set
bryophyte_occurrence <- BIEN_occurrence_higher_plant_group("bryophytes")

#Creates Command BIEN_taxonomy_higher_plant_group to access species names of bryophytes in BIEN
BIEN_taxonomy_higher_plant_group <- function (higher_plant_group, print.query = FALSE, ...) 
{
  is_char(higher_plant_group)
  is_log(print.query)
  sql_select <- paste("SELECT DISTINCT higher_plant_group, \"class\", superorder, \"order\", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status")
  sql_from <- paste(" FROM bien_taxonomy")
  sql_where <- paste(" WHERE higher_plant_group in (", paste(shQuote(higher_plant_group, 
                                                                     type = "sh"), collapse = ", "), ") AND scrubbed_species_binomial IS NOT NULL")
  sql_order_by <- paste(" ORDER BY higher_plant_group,scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author ")
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, 
                 ";")
  if (print.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
                  perl = TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
}


#Creates Bryophyte Database
bryophytes <- BIEN_taxonomy_higher_plant_group("bryophytes", print.query = FALSE)

#Importing SpeciesPresence
SpeciesPresence <- read_csv("~/Downloads/SpeciesPresence_by_100km_Cell.csv")

#Replacing "_" with " " in SpeciesPrescence
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)

#Filter Dataset
FilteredOccurrence=SpeciesPresence[SpeciesPresence$Species %in% unique(bryophyte_occurrence$scrubbed_species_binomial), ]
View(FilteredOccurrence)

#Tally
tally <- tally(group_by(FilteredOccurrence, CellID))
colnames(tally)[2] <- "Richness"

#RasterTutorial
require(maps)
require(maptools)
require(raster)
require(dismo)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)

#Import Raster
BIENblank <-raster("~/Downloads/blank_100km_raster.tif")

#Create Data with all Columns
Moss_Richness <- numeric(15038)
Moss_Richness[tally$CellID] <- tally$Richness

#Plot Data
RichnessRaster <- setValues(BIENblank, Moss_Richness)
plot(RichnessRaster)

#Not sure what this does
#BIEN_Richness <- read_csv("~/Downloads/Richness_by_100km_Cell (1).csv")

#Take off gray
RichnessVector<-Moss_Richness
RichnessVector[which(RichnessVector==0)]=NA
RichnessRasterNA <- setValues(BIENblank, RichnessVector)
plot(RichnessRasterNA)

#Repeated from above
Moss_Richness <- numeric(15038)
Moss_Richness[tally$CellID] <- tally$Richness
RichnessRaster <- setValues(BIENblank, Moss_Richness)
plot(RichnessRaster)

#Trial of Range Mapping with Abietinella
Abietinella <- filter(FilteredOccurrence, Species=="Abietinella abietina")
View(Abietinella)

Moss_Range <- numeric(15038)
Moss_Range[tally1$CellID] <- tally1$Richness
RichnessRaster <- setValues(BIENblank, Moss_Range)
plot(RichnessRaster)

#Attempt to overlap 
Bryum <- filter(FilteredOccurrence, Species=="Bryum weigelii")
View(Bryum)

Moss_Range1 <- numeric(15038)
Moss_Range1[tally2$CellID] <- tally2$Richness
RichnessRaster1 <- setValues(BIENblank, Moss_Range1)
plot(RichnessRaster1 + RichnessRaster)

#Attempt to overlap 2
Schistidium <- filter(FilteredOccurrence, Species=="Schistidium pulchrum")
View(Schistidium)

tally3 <- tally(group_by(Schistidium, CellID))

colnames(tally3)[2] <- "Richness"

Moss_Range2 <- numeric(15038)
Moss_Range2[tally3$CellID] <- tally3$Richness
RichnessRaster2 <- setValues(BIENblank, Moss_Range2)
plot(RichnessRaster2+RichnessRaster+RichnessRaster1)

#Create Dataset showing Range
Range <- tally(group_by(FilteredOccurrence, Species))
Filter <- merge(Range, FilteredOccurrence, by = "Species")

#AP's Way
#Filter <- subset(FilteredOccurrence, select = c("Species")) %>%
 # group_by(Species) %>%
  #mutate(Cnt = n())



#Find Means of CellIDs
filter.2 <- subset(Filter, select = c("n", "CellID")) %>%
     group_by(CellID) %>%
     summarize(Avg = median(n))

#Create Range Map
BIENblank <-raster("~/Downloads/blank_100km_raster.tif")
Moss_Range.3 <- numeric(15038)
Moss_Range.3[filter.2$CellID] <- filter.2$Avg
RangeRaster <- setValues(BIENblank, Moss_Range.3)
plot(RangeRaster)

#RangeRaster <- setValues(BIENblank, Moss_Range.3)
#plot(RangeRaster)
#BIEN_Richness <- read_csv("~/Downloads/Richness_by_100km_Cell (1).csv")

#Take off gray
RangeVector<-Moss_Range.3
RangeVector[which(RangeVector==0)]=NA
RangeRasterNA <- setValues(BIENblank, RangeVector)
plot(RangeRasterNA)

model <- lm(Avg ~ CellID, data = filter.2)
AOV <- aov(Avg ~ as.factor(CellID), data = filter.2)
tukey <- TukeyHSD(AOV, CellID, conf.level = 0.95)

model <- lm(Avg ~ CellID, data = filter.2)
AOV <- aov(Avg ~ as.factor(CellID), data = filter.2)
tukey <- TukeyHSD(AOV, as.factor(filter.2$CellID), conf.level = 0.95)
tukey
#ANOVA
range.lm=lm(Avg ~ CellID, data=filter.2)
anova(range.lm)

#TukeyTest
TukeyHSD(range.lm)
myresults <- HSD.test(range.lm, "CellId", group="TRUE")
myresults

#Graphing Relationship
Test <- merge(filter.2, tally, by = "CellID")
require(ggplot2)
testplot <- ggplot(data = Test, aes(Richness, log(Avg)))

#Black and White Graph
testplot + geom_point()+ ggtitle("Relationship Between Richness and Median") + ylab("log(Median Range per Cell)") + xlab("Richness(Species per Cell)") 

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))

#This adds a column of color values
# based on the y values
Test$Col <- rbPal(10)[as.numeric(cut(Test$CellID,breaks = 10))]

#Graphs With Color as Variable
testplot + geom_point(col=Test$Col) + scale_color_gradient(low = "blue", high = "red") + ggtitle("Relationship Between Richness and Median") + ylab("log(Median Range per Cell)") + xlab("Richness(Species per Cell)") 

#Graph in Base Package With Legend
plot(log(Test$Avg)~Test$Richness, data = Test, col=Test$Col, pch=20, xlab="Richness(Species per Cell)", ylab= "log(Median Range Size per Cell)")
legend("bottomright",title="Decile",legend=c(1:10),col =rbPal(10),pch=20)

#Non-Log
plot(Test$Avg~Test$Richness, data = Test, col=Test$Col, pch=20, xlab="Richness(Species per Cell)", ylab= "log(Median Range Size per Cell)")



testplot + geom_point(col=Test$Col) + geom_smooth(method=lm)

legend("bottomright",title="Decile",legend=c(1:10),col =rbPal(10),pch=20)


model <- lm(log(Avg)~Richness + CellID, data = Test)
ggplot(Test, aes(Avg, model$fitted.values)) + geom_line()
summary(model)$r.squared

colnames(filter.2)[2] <- "Median"
write.csv(Test, "~/Desktop/SendtoAP.csv")
