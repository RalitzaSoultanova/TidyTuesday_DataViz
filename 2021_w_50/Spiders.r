library(tidyverse)
library(dplyr)
library(treemap)
library(data.tree)
library(DiagrammeR)
library(networkD3)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

#write.csv("spiders.csv")


spiders_Balkans <-  filter(spiders, grepl("Bulgaria|Albania|Greece|Bosnia|Kosovo|Macedonia|Montenegro|Romania|Serbia", distribution))


spiders_Balkans$pathString <- paste("spiders_Balkans", spiders_Balkans$family, spiders_Balkans$genus, sep = "/")
population <- as.Node(spiders_Balkans)
print(population, "species",  limit = 50)

#define the hierarchy (family/genus)
spiders_Balkans$pathString <- paste("Spiders", spiders_Balkans$family, spiders_Balkans$genus, sep = "|")
#convert to Node
spiders_Balkans <- as.Node(spiders_Balkans, pathDelimiter = "|")
useRtreeList <- ToListExplicit(spiders_Balkans, unname = TRUE)

radialNetwork(useRtreeList, fontSize = 7,
              fontFamily = "sans serif", linkColour = "#ccc", nodeColour = "#fff",
              nodeStroke = "steelblue", textColour = "#111", opacity = 1,
              margin = NULL)
# title and credits added in Photoshop 
