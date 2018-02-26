require(dplyr)
require(ggplot2)
#Create Dataset with a Count of Occurrences per Row
LiverwortTally <- tally(group_by(liverwortoccurrence, Row))
colnames(LiverwortTally)[2] <- "Richness"

aaa <- ggplot(LiverwortTally, aes(Row, Richness))
aaa +
  geom_point()+
  geom_vline(xintercept = 75, col= "blue")+
  ggtitle("Liverwort Richness vs. Latitude")+
  theme_bw()



HornwortTally <- tally(group_by(hornwortoccurrence, Row))
colnames(HornwortTally)[2] <- "Richness"

bbb <- ggplot(HornwortTally, aes(Row, Richness))
bbb +
  geom_point()+
  geom_vline(xintercept = 75, col= "blue")+
  ggtitle("Hornwort Richness vs. Latitude")+
  theme_bw()

MossTally <- tally(group_by(mossoccurrence, Row))
colnames(MossTally)[2] <- "Richness"

ccc <- ggplot(MossTally, aes(Row, Richness))
ccc +
  geom_point()+
  geom_vline(xintercept = 75, col= "blue")+
  ggtitle("Moss Richness vs. Latitude")+
  theme_bw()



LiverwortTally1 <- tally(group_by(liverwortoccurrence, CellID))
colnames(LiverwortTally1)[2] <- "Richness"

xxx <- ggplot(LiverwortTally1, aes(CellID, Richness))
xxx +
  geom_point()+
  ggtitle("Liverwort Richness vs. Latitude\nUsing CellID")+
  theme_dark()

#Subsetting based on Unique Occurrence in the Row
uniqueliverwortoccurrence <- liverwortoccurrence[!duplicated(liverwortoccurrence[1:2]),]
uniquemossoccurrence <- mossoccurrence[!duplicated(mossoccurrence[1:2]),]
uniquehornwortoccurrence <- hornwortoccurrence[!duplicated(hornwortoccurrence[1:2]),]
uniquehornandliveroccurrence <- hornandliveroccurrence[!duplicated(hornandliveroccurrence[1:2]),]

#Mapping with the this Unique Data
#Liverwort
uniqueLiverwortTally <- tally(group_by(uniqueliverwortoccurrence, Row))
colnames(uniqueLiverwortTally)[2] <- "Richness"

eee <- ggplot(uniqueLiverwortTally, aes(Row, Richness))
eee +
  geom_point()+
  geom_vline(xintercept = 75, col= "blue")+
  ggtitle("Liverwort Richness vs. Latitude\nUnique Rows")

ggplot(uniqueLiverwortTally, aes(Row, Richness))+
  geom_line(aes(col="red"))+
  geom_vline(xintercept = 75, col= "black")+
  geom_line(data=uniquehornwortTally, aes(Row, Richness, col = "green"))+
  geom_line(data = uniquemossTally, aes(Row, Richness, col = "purple"))+
  labs(title = "Richness vs. Latitude, Unique Rows", x = "Latitude (Row)", y = "Species Richness", color = "Family") +
  scale_color_manual(labels = c("Hornworts", "Moss", "Liverworts"), values = c("purple", "aquamarine3", "dodgerblue3")) 

#Hornwort
uniquehornwortTally <- tally(group_by(uniquehornwortoccurrence, Row))
colnames(uniquehornwortTally)[2] <- "Richness"

fff <- ggplot(uniquehornwortTally, aes(Row, Richness))
fff +
  geom_point()+
  geom_vline(xintercept = 75, col= "blue")+
  ggtitle("Hornwort Richness vs. Latitude\nUnique Rows")

#Moss
uniquemossTally <- tally(group_by(uniquemossoccurrence, Row))
colnames(uniquemossTally)[2] <- "Richness"

ggg <- ggplot(uniquemossTally, aes(Row, Richness))
ggg +
  geom_point(col = "darkgreen")+
  geom_vline(xintercept = 85, col= "blue")+
  ggtitle("Moss Richness vs. Latitude")


#Graph of Hornworts AND Liverworts
uniquehornandliveroccurrence <- hornandliveroccurrence[!duplicated(hornandliveroccurrence[1:2]),]
uniquehornandliverTally <- tally(group_by(uniquehornandliveroccurrence, Row))
colnames(uniquehornandliverTally)[2] <- "Richness"

hhh <- ggplot(uniquehornandliverTally, aes(Row, Richness))
hhh +
  geom_point()+
  geom_vline(xintercept = 85, col= "blue")+
  ggtitle("Hornwort and Liverwort Richness vs. Latitude")

ggplot(uniquehornandliverTally, aes(Row, Richness))+
  geom_line(aes(col="red"))+
  geom_vline(xintercept = 85, col= "black")+
  geom_line(data = uniquemossTally, aes(Row, Richness, col = "purple"))+
  labs(title = "Richness vs. Latitude", x = "Latitude (Row)", y = "Species Richness", color = "Family") +
  scale_color_manual(labels = c("Mosses", "Hornworts\nand Liverworts"), values = c("aquamarine3", "dodgerblue3")) 




