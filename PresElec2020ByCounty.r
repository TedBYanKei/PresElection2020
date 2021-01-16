# Created by Ted Braun
# For academic / learning purposes only.
#  Just messing about making maps of 2020 US presidential election.
#  Really like mapdeck.

library(choroplethr)
library(choroplethrMaps)
library(tidyverse)
library(stringr)
library(here)
library(mapdeck)

set_token("pk.eyJ1IjoidGpicmF1biIsImEiOiJja2swNzdmMWIwZWs4MnV0MXp0enMwbHViIn0.D1RoqyhqYBucrwy2O23R8w")
data(county.regions)
CurrDir <- here::here()

library(rvest)

#Get FIPS to Longitude/Latitude data
url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table" 
page <- read_html(url) #Creates an html document from URL
table <- html_table(page, fill = TRUE) #Parses tables into data frames
table_1<-table[[1]] 
str(table_1)
FIPS_wLL <- table_1 %>% 
  select(FIPS, Longitude, Latitude)
FIPS_wLL$FIPS <- as.character(FIPS_wLL$FIPS)
FIPS_wLL$FIPS <- stringr::str_pad(FIPS_wLL$FIPS, 5, side = "left", pad = 0)
str(FIPS_wLL)
str(county.regions)
CountyFIPS_GEO <- inner_join(county.regions, FIPS_wLL, by = c("county.fips.character" = "FIPS"))
str(CountyFIPS_GEO)



PresElecData <- read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv")
#PresElecData[] <- lapply(PresElecData, as.character)
PresElecData %>% mutate_if(is.factor, as.character) -> PresElecData
#PresElecData <- as.data.frame(PresElecData)

PresElecData$county_fips <- as.character(PresElecData$county_fips)
PresElecData$county_fips <- stringr::str_pad(PresElecData$county_fips, 5, side = "left", pad = 0)
str(PresElecData)




PEDwFLL <- inner_join(CountyFIPS_GEO, PresElecData, by = c("county.fips.character" = "county_fips"))

PEDwFLL_ForOutput <- PEDwFLL %>% 
  select(Longitude, Latitude, votes_gop, votes_dem) %>% 
  mutate(votes_gop_rd = round((votes_gop * 0.01), digits = 0), 
         votes_dem_rd = round((votes_dem * 0.01), digits = 0)) %>% 
  select(Longitude, Latitude, votes_gop_rd, votes_dem_rd)
#PEDwFLL_ForOutput <- PEDwFLL_ForOutput[!is.na(PEDwFLL_ForOutput$Longitude), ]
#PEDwFLL_ForOutput <- PEDwFLL_ForOutput[!is.na(PEDwFLL_ForOutput$Latitude), ]

PEDwFLL_ForOutput$Longitude <- gsub('°', '', PEDwFLL_ForOutput$Longitude)
PEDwFLL_ForOutput$Latitude <- gsub('°', '', PEDwFLL_ForOutput$Latitude)
str(PEDwFLL_ForOutput)
PEDwFLL_ForOutput$Longitude <- gsub('+', '', PEDwFLL_ForOutput$Longitude)
PEDwFLL_ForOutput$Latitude <- gsub('+', '', PEDwFLL_ForOutput$Latitude)
str(PEDwFLL_ForOutput)
PEDwFLL_ForOutput$Longitude <- gsub('–', '-', PEDwFLL_ForOutput$Longitude)
PEDwFLL_ForOutput$Latitude <- gsub('–', '-', PEDwFLL_ForOutput$Latitude)
str(PEDwFLL_ForOutput)
PEDwFLL_ForOutput$Longitude <- as.double(PEDwFLL_ForOutput$Longitude)
PEDwFLL_ForOutput$Latitude <- as.double(PEDwFLL_ForOutput$Latitude)

setwd(CurrDir)
Dems <- PEDwFLL_ForOutput %>% 
  mutate(Longitude = Longitude - 0.25) %>% 
  slice(rep(row_number(), votes_dem_rd))
Reps <- PEDwFLL_ForOutput %>% 
  mutate(Longitude = Longitude + 0.25) %>% 
  slice(rep(row_number(), votes_gop_rd))
str(Dems)
str(Reps)
mapdeck( style = mapdeck_style("light"), pitch = 35) %>%
  add_hexagon(
    data = Reps[]
    , lat = "Latitude"
    , lon = "Longitude"
    , radius = 9000
    , layer_id = "hex_layer2"
    , elevation_scale = 1000
    , colour_range = c("#FB5862", "#FB5862", "#FB5862", "#FB5862", "#FB5862", "#FB5862")
  ) %>%
  add_hexagon(
    data = Dems[]
    , lat = "Latitude"
    , lon = "Longitude"
    , radius = 9000
    , layer_id = "hex_layer"
    , elevation_scale = 1000
    , colour_range = c("#33A4FF", "#33A4FF", "#33A4FF", "#33A4FF", "#33A4FF", "#33A4FF")
  )
  



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PresElecData <- PresElecData %>% 
  mutate(DemRepPctAdded = per_gop + per_dem) %>% 
  mutate(PctRep = round((per_gop / DemRepPctAdded), digits = 4),
         PctDec = round((per_dem / DemRepPctAdded), digits = 4))

str(county.regions)
tmp3 <- county.regions %>% filter(str_detect(county.name,"alameda")) 
str(tmp3)
str(PresElecData)

PEDwFips <- inner_join(county.regions, PresElecData, by = c("county.fips.character" = "county_fips"))

PEDwFipsE <- PEDwFips %>% 
  rename(value = per_dem) %>% 
  select(region, value)

county_choropleth(PEDwFipsE,
                  title ="  2020 US Presidential Election by County", 
                  legend = "Pct Dem Vot", num_colors = 1,) + 
  #scale_colour_gradient(low = "red", high = "blue") + 
  scale_fill_gradient(low = "#FB5862", high = "blue")
  #scale_fill_brewer(palette="YlOrRd") +
  #scale_colour_manual(values = c("#0000e6", "#e60000"))
