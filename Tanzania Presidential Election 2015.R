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

# changing CHADEMA column from character to numeric
TNZ_2015$CHADEMA <- as.character(TNZ_2015$CHADEMA)

sapply(TNZ_2015, class)

TNZ_2015$CHADEMA <- as.numeric(as.character(TNZ_2015$CHADEMA))

# change NA to 0
TNZ_2015[["CHADEMA"]][is.na(TNZ_2015[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
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

# gathering data into three columns
TNZ_2015 <-  gather(TNZ_2015, key = "Political_Parties", 
                             value = "Total_Votes",
                             -Region) 

# filtering political parties with highest votes for each region
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
# a map of tanzania with empty inputs
ggplot(data = TZA_Lev1,
       mapping = aes(x = long, y = lat,
                     group = group)) +
geom_polygon(fill = "white", color = "black")

# filling each region with color within a map
ggplot(data = TZA_Lev1,
            aes(x = long, y = lat,
                group = group, fill = Region_Name)) +
geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)



# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")

# final plot of map of tanzania with region colored with political party that had majority votes
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

# importing logo inside the map
  ggdraw() +
  draw_image("Court of Arm.png", x =0.42, y =0.39, scale = .2) +
  draw_plot(my_map)


ggsave("Graphs/TZA_Lev1.jpg", device="jpg")




# REGIONS 
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

# changing CHADEMA column from character to numeric
TNZ_2015$CHADEMA <- as.character(TNZ_2015$CHADEMA)

sapply(TNZ_2015, class)

TNZ_2015$CHADEMA <- as.numeric(as.character(TNZ_2015$CHADEMA))

# change NA to 0
TNZ_2015[["CHADEMA"]][is.na(TNZ_2015[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
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

# gathering data into three columns
TNZ_2015 <-  gather(TNZ_2015, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Region) 



# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for ARUSHA CONSTITUENCY
REG_plot <- ggplot(TNZ_2015, aes(x=Total_Votes, y=Region, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,1600000)) +
  labs(title = (" United Republic of Tanzania") ,
       subtitle = ("      Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(REG_plot)


ggsave("Graphs/REG_plot.jpg", device="jpg")








# ARUSHA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "ARUSHA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for ARUSHA CONSTITUENCY
AR_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,220000)) +
  labs(title = (" United Republic of Tanzania") ,
       subtitle = ("      Arusha Electoral Constituency\n Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(AR_plot)


ggsave("Graphs/AR_plot.jpg", device="jpg")




# DAR ES SALAAM REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "DAR ES SALAAM") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for DAR ES SALAAM CONSTITUENCY
DAR_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,240000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Dar es Salaam Electoral Constituency\n Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = 0.42, y =0.39, scale = .2) +
  draw_plot(DAR_plot)


ggsave("Graphs/DAR_plot.jpg", device="jpg")





# DODOMA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "DODOMA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for DODOMA CONSTITUENCY
DOD_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,180000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("     Dodoma Electoral Constituency\n Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = 0.42, y =0.39, scale = .2) +
  draw_plot(DOD_plot)


ggsave("Graphs/DOD_plot.jpg", device="jpg")




# GEITA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "GEITA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for DODOMA CONSTITUENCY
GEI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Geita Electoral Constituency\n Presidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(GEI_plot)


ggsave("Graphs/GEI_plot.jpg", device="jpg")




# IRINGA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# KAGERA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")









# KASKAZINI PEMBA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")






# KASKAZINI UNGUJA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")








# KATAVI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")








# KIGOMA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# KILIMANJARO REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")








# KUSINI PEMBA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# KUSINI UNGUJA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# LINDI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# MANYARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# MARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")








# MBEYA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")









# MJINI MAGHARIBI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")








# MOROGORO REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# MTWARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")









# MWANZA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# NJOMBE REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")






# PWANI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# RUKWA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# RUVUMA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")







# SHINYANGA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "SHINYANGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
SHI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("  Shinyanga Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(SHI_plot)


ggsave("Graphs/SHI_plot.jpg", device="jpg")







# SIMIYU REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "SIMIYU") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for SIMIYU CONSTITUENCY
SIM_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,150000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Simiyu Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(SIM_plot)


ggsave("Graphs/SIM_plot.jpg", device="jpg")






# SINGIDA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "SINGIDA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRICONSTITUENCY
SIN_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,80000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("      Singida Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(SIN_plot)


ggsave("Graphs/SIN_plot.jpg", device="jpg")







# TABORA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "TABORA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for IRINGA CONSTITUENCY
TAB_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Tabora Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(TAB_plot)


ggsave("Graphs/TAB_plot.jpg", device="jpg")








# TANGA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")


# Cleaning Data --------------------------------------------------------------------------------
# Rename variables ( Constituency and Region)
TNZ_ELEC <- TNZ_ELEC %>% rename(Electoral_Constituency = Constituency,
                                ACT = "Anna E. Mgwihira", 
                                ADC = "Chief L. Yemba", 
                                CCM = "Dkt. John P. Magufuli", 
                                CHADEMA = "Edward N. Lowassa", 
                                CHAUMA = "Hashim R. Spunda", 
                                NRA = "Kasambala J. Malik", 
                                TLP = "Elifatio M. Lyimo", 
                                UPDP = "Dovutwa F. Nassoro") 


# changing CHADEMA column from character to numeric
TNZ_ELEC$CHADEMA <- as.character(TNZ_ELEC$CHADEMA)

sapply(TNZ_ELEC, class)

TNZ_ELEC$CHADEMA <- as.numeric(as.character(TNZ_ELEC$CHADEMA))

# change NA to 0
TNZ_ELEC[["CHADEMA"]][is.na(TNZ_ELEC[["CHADEMA"]])] <- 0


# Finding the sum of each political party and removing some columns
TNZ_ELEC %<>% filter(Region == "TANGA") %>% 
  select(-`Registered Voters`, -Voters, -`Valid Voters`, -`Invalid Voters`)  %>%
  group_by(Electoral_Constituency) %>%
  summarize(ACT = sum(ACT),
            ADC = sum(ADC),
            CCM = sum(CCM),
            CHADEMA = sum(CHADEMA),
            CHAUMA = sum(CHAUMA),
            NRA = sum(NRA),
            TLP =sum(TLP),
            UPDP = sum(UPDP)) %>%
  ungroup()


# gathering data into three columns
TNZ_ELEC <-  gather(TNZ_ELEC, key = "Political_Parties", 
                    value = "Total_Votes",
                    -Electoral_Constituency) 

# giving each party it's color to be represented in plot
Party_color <- c("ACT" = "Purple", "ADC" = "Orange", "CCM" = "green", "CHADEMA" = "Blue", "CHAUMA" = "brown", "NRA" = "violet", "TLP" = "red", "UPDP" = "yellow")


# Plotting a grap for TANGA CONSTITUENCY
TAN_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,150000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("      Tanga Electoral Constituency\nPresidential Election Results in 2015"),
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(TAN_plot)


ggsave("Graphs/TAN_plot.jpg", device="jpg")
