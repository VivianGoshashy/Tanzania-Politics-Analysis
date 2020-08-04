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
TNZ_2015 <- read_csv("../../NEC/tanzania-election-result-2015.csv")


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
TZA_Lev0 <- readRDS("../../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev0_sp.rds")

TZA_Lev1 <- readRDS("../../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev1_sp.rds")

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
<<<<<<< HEAD
TNZ_2015 <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_2015 <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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


<<<<<<< HEAD
# Plotting a grap for Regions
=======
# Plotting a grap for ARUSHA CONSTITUENCY
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  ) 

=======
  )
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(REG_plot)


ggsave("Graphs/REG_plot.jpg", device="jpg")








# ARUSHA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KAGERA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KAGERA CONSTITUENCY
KAG_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
<<<<<<< HEAD
       subtitle = ("       Kagera Electoral Constituency\nPresidential Election Results in 2015"),
=======
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KAG_plot)


ggsave("Graphs/KAG_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5









# KASKAZINI PEMBA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KASKAZINI PEMBA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KASKAZINI PEMBA CONSTITUENCY
KAS_PEM_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,14000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Kaskazini Pemba Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KAS_PEM_plot)


ggsave("Graphs/KAS_PEM_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5






# KASKAZINI UNGUJA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KASKAZINI UNGUJA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KASKAZINI UNGUJA CONSTITUENCY
KAS_UNG_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,10000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Kaskazini Unguja Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KAS_UNG_plot)


ggsave("Graphs/KAS_UNG_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5








# KATAVI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KATAVI") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KATAVI CONSTITUENCY
KAT_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,54000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Katavi Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KAT_plot)


ggsave("Graphs/KAT_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5








# KIGOMA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KIGOMA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KIGOMA CONSTITUENCY
KIG_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Kigoma Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KIG_plot)


ggsave("Graphs/KIG_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# KILIMANJARO REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KILIMANJARO") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KILIMANJARO CONSTITUENCY
KIL_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("  Kilimanjaro Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KIL_plot)


ggsave("Graphs/KIL_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5








# KUSINI PEMBA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KUSINI PEMBA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KUSINI PEMBA CONSTITUENCY
KUS_PEM_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,12000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Kusini Pemba Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KUS_PEM_plot)


ggsave("Graphs/KUS_PEM_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# KUSINI UNGUJA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "KUSINI UNGUJA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for KUSINI UNGUJA CONSTITUENCY
KUS_UNG_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,15000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Kusini Unguja Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(KUS_UNG_plot)


ggsave("Graphs/KUS_UNG_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# LINDI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "LINDI") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for LINDI CONSTITUENCY
LIN_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,80000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Lindi Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(LIN_plot)


ggsave("Graphs/LIN_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# MANYARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MANYARA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MANYARA CONSTITUENCY
MAN_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
<<<<<<< HEAD
       subtitle = ("    Manyara Electoral Constituency\nPresidential Election Results in 2015"),
=======
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MAN_plot)


ggsave("Graphs/MAN_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# MARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MARA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MARA CONSTITUENCY
MARA_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Mara Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MARA_plot)


ggsave("Graphs/MARA_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5








# MBEYA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MBEYA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
MBE_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,180000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Mbeya Electoral Constituency\nPresidential Election Results in 2015"),
=======
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MBE_plot)


ggsave("Graphs/MBE_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5









# MJINI MAGHARIBI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MJINI MAGHARIBI") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MJINI MAGHARIBI CONSTITUENCY
MJI_MAG_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,20000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("Mjini Magharibi Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MJI_MAG_plot)


ggsave("Graphs/MJI_MAG_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5








# MOROGORO REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MOROGORO") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MOROGORO CONSTITUENCY
MOR_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,180000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("   Morogoro Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
       caption = ("Source: \"https://www.nec.go.tz\"\n done by: Vivian J. Goshashy"  ),
       x = ("Total Votes"),
       y = ("Electoral Constituency")) + 
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
<<<<<<< HEAD
  ) +
  coord_flip()

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = 0.4, y =-0.4, scale = .2) +
  draw_plot(MOR_plot)


ggsave("Graphs/MOR_plot.jpg", device="jpg")
=======
  )

# importing logo inside the map
ggdraw() +
  draw_image("Court of Arm.png", x = -0.42, y =0.4, scale = .2) +
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# MTWARA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MTWARA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MTWARA CONSTITUENCY
MTW_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Mtwara Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MTW_plot)


ggsave("Graphs/MTW_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5









# MWANZA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "MWANZA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for MWANZA CONSTITUENCY
MWA_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,200000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("     Mwanza Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(MWA_plot)


ggsave("Graphs/MWA_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# NJOMBE REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "NJOMBE") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for NJOMBE CONSTITUENCY
NJO_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,60000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("     Njombe Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(NJO_plot)


ggsave("Graphs/NJO_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5






# PWANI REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "PWANI") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for PWANI CONSTITUENCY
PWA_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Pwani Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(PWA_plot)


ggsave("Graphs/PWA_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# RUKWA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "RUKWA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
RUKWA_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Rukwa Electoral Constituency\nPresidential Election Results in 2015"),
=======
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(RUKWA_plot)


ggsave("Graphs/RUKWA_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# RUVUMA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC %<>% filter(Region == "RUVUMA") %>% 
=======
TNZ_ELEC %<>% filter(Region == "IRINGA") %>% 
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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


<<<<<<< HEAD
# Plotting a grap for RUVUMA CONSTITUENCY
RUVUMA_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("      Ruvuma Electoral Constituency\nPresidential Election Results in 2015"),
=======
# Plotting a grap for IRINGA CONSTITUENCY
IRI_plot <- ggplot(TNZ_ELEC, aes(x=Total_Votes, y=Electoral_Constituency, fill = Political_Parties)) +
  geom_col() +
  scale_fill_manual(name="Political Parties",values = Party_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(limits = c(0,120000)) +
  labs(title = ("United Republic of Tanzania") ,
       subtitle = ("       Iringa Electoral Constituency\nPresidential Election Results in 2015"),
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5
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
<<<<<<< HEAD
  draw_plot(RUVUMA_plot)


ggsave("Graphs/RUVUMA_plot.jpg", device="jpg")
=======
  draw_plot(IRI_plot)


ggsave("Graphs/IRI_plot.jpg", device="jpg")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5







# SHINYANGA REGION
# Cleares the environment ----------------------------------------------------------------------
rm(list=ls())

# Importing File -------------------------------------------------------------------------------
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
<<<<<<< HEAD
TNZ_ELEC <- read_csv("../../NEC/tanzania-election-result-2015.csv")
=======
TNZ_ELEC <- read_csv("../NEC/tanzania-election-result-2015.csv")
>>>>>>> e6ad5f8a1d9f101b3ec81f992d626d71c2dfdff5


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
