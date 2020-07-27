# loading packages -----------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(ggmap)
library(magick)
library(cowplot)
theme_set(theme_cowplot())

# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_2015 <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_2015 <- TNZ_2015 %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 

TNZ_2015$CHADEMA <- as.character(TNZ_2015$CHADEMA)

sapply(TNZ_2015, class)

TNZ_2015$CHADEMA <- as.numeric(as.character(TNZ_2015$CHADEMA))

TNZ_2015[["CHADEMA"]][is.na(TNZ_2015[["CHADEMA"]])] <- 0



TNZ_2015 %<>% select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`, -Electoral_Constituency)  %>%
  group_by(Region) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


TNZ_2015 <-  gather(TNZ_2015, key = "Political_Parties", 
                             value = "Total_Votes",
                             -Region) 

TNZ_2015 <- TNZ_2015 %>%group_by(Region) %>%
  filter(Total_Votes == max(Total_Votes))


# Maps -----------------------------------------------------------------------------------------
library(sp)
library(broom)
library(sf)

# Read in the map files
TZA_Lev0 <- readRDS("../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev0_sp.rds")

TZA_Lev1 <- readRDS("../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev1_sp.rds")

# Convert the SpatialPolygonDataFrame to a normal data frame
TZA_Lev0 %<>% tidy()
TZA_Lev1 %<>% tidy()


# Gather the map region names
Regions <- read_csv("Patches/Region.csv",
                    col_types="cc")


# Joining the names to map
TZA_Lev1 %<>% left_join(Regions)


# Joining the data to tbl
TZA_Lev1 %<>% left_join(TNZ_2015,
                        by=c("Region_Name"="Region"))

# Plotting in ggplot2
ggplot(data = TZA_Lev1,
       mapping = aes(x = long, y = lat,
                     group = group)) +
geom_polygon(fill = "white", color = "black")


ggplot(data = TZA_Lev1,
            aes(x = long, y = lat,
                group = group, fill = Region_Name)) +
geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)




Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")

my_map <- ggplot(data = TZA_Lev1,
            aes(x = long, y = lat,
                group = group, fill = Political_Parties, color=id))+
geom_polygon(color = "black", size = 0.1) +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = ("       United Republic of Tanzania") ,
       subtitle = ("          Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  )) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic")) 

  ggdraw() +
  draw_image("Court of Arm.png", x =0.42, y =0.39, scale = .2) +
  draw_plot(my_map)


ggsave("Graphs/TZA_Lev1.jpg", device="jpg")


