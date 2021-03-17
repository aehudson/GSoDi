
# You may need to install these packages first
library(tidyverse)
library(ggplot2)
library(magick)
library(reshape2)
library(readxl)
library(forcats)

#### Colours ####
# IDEA primary colours -- two versions of the translation from CMYK to Hex
# C:70 M:4 Y:2 K:0 -> #4DF5FA or #4ab1d8 teal
# C:50 M:0 Y:80 K:0 -> #80FF33 or #81c063 green
# C:1 M:92 Y:42 K:0 -> #FC1494 or #d9385f mauve
# C:0 M:10 Y:100 K:0 -> #FFE600 or #f7da1c yellow
# C:90 M:90 Y:0 K:0 -> #1919FF or #383880 purple
# C:0 M:72 Y:100 K:0 -> #FF4700 or #e26429 orange

# IDEA_clrs <- c("#4DF5FA","#80FF33","#FC1494","#FFE600","#1919FF","#FF4700")
IDEA_clrs <- c("#4ab1d8","#81c063","#d9385f","#f7da1c","#383880","#e26429","#f39c32")


# read in GSOD data
gsodi <- read.csv("GSOD2019.csv", header = TRUE)
head(gsodi)

# read in regime performance data
regime_perf_dat <- read_excel("Global Regimes Categories (1975-2019).xlsx")
head(regime_perf_dat)

# read in significant changes by country
sig_change_dat <- read.csv("Sig Change by Country - 2020.csv", header = TRUE)
head(sig_change_dat)

# read in advances and declines by indicator
indicator_change_dat <- read.csv("Dem. Advancer and Decliner; All Timespan Global - 2020.csv", header = TRUE)
head(indicator_change_dat)

# load the IDEA logo
idea_logo <- image_read("IDEA.png")


#### Example 1: Make a plot to accompany a tweet on the Netherlands ####
# The data are subsetted and plotted within a single pipe here.
# You could do this in several steps and save a smaller dataframe along the way
jpeg("ndl_tweet_2.jpg", width = 9, height = 6, units = 'in', res = 600) # open an image file
gsodi %>% # select a dataframe to pull information from 
  filter(ID_country_name == "Netherlands") %>% # filter the data by the name of the country of interest
  dplyr::select(ID_year, C_A1, C_SD23C,v_23_31) %>% # select the variables that we want
  pivot_longer(cols = -ID_year) %>%  # reshape data to a long format
  ggplot(aes(ID_year, value, color = name)) + # data and axes
  theme_classic() + # select the classic theme to get rid of some "chart junk"
  geom_line(size = 2) + # line plot of the included vars in melted data
  labs(title = "Democracy and Gender in the Netherlands", x = "", y = "", # add some labels
       caption = "source: Global State of Democracy Data") +
  theme(legend.position = "bottom", legend.title = element_blank()) + # move the legend to the bottom
  scale_color_manual(labels = c("Representative democracy", "Gender equality","Power distributed by gender"), 
                     values = IDEA_clrs[1:3]) + # manually add names to the variables DOUBLE CHECK!
  geom_vline(xintercept = 2017, linetype="dotted", 
             color = "black", size=1.5) # add a vertical line at an interesting data point
# add the IDEA logo
grid::grid.raster(idea_logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(2.5, 'cm'))
dev.off() # close the image file



#### Example 2: Make plots to accompany a tweet on presidential elections in Chad and Benin ####
# This one is more complicated since we have to map both countries and variables

# create a smaller dataframe with only our variables of interest
af_pres_dat <-gsodi %>% 
  filter(ID_country_name %in%  c("Chad","Benin")) %>% # filter on country list
  dplyr::select(c(ID_country_name, ID_year, v_42_01, v_31_02)) # select variables of interest: 
#executive respects constitution, executive oversight

# change data from wide to tall/long
# note that we have two index variables -- country and year
af_pres_dat_melted <- melt(af_pres_dat, id = c('ID_country_name', 'ID_year')) 

# new variable that combines the two variables that will map the color and linetype
af_pres_dat_melted$sv <- paste(af_pres_dat_melted$ID_country_name,af_pres_dat_melted$variable,sep='-') 

jpeg("chad_benin_tweet_2.jpg", width = 9, height = 6, units = 'in', res = 600) # open an image file
ggplot(data=af_pres_dat_melted, aes(x=ID_year, y=value, linetype=sv, col=sv)) + # specify plot axes and mapping for the lines
  theme_classic() + # remove chart junk
  labs(title = "Executive oversight and the constitution", x = "", y = "", # add labels
       caption = "source: Global State of Democracy Data") +
  # theme(legend.position = "bottom", legend.title = element_blank()) +
  # theme(panel.background = element_rect(fill = "#fbf5e7",
  #                                       colour = "#fbf5e7",
  #                                       size = 0.5, linetype = "solid"))+
  geom_line(size = 1.5) + # add lines
  scale_colour_manual(name = "" # manually enter colours and label names
                      , values=c("#4ab1d8","#4ab1d8","#81c063","#81c063")
                      , labels=c("Benin: exec. oversight","Benin: exec. respects const."
                                 ,"Chad: exec. oversight","Chad: exec. respects const.")
  ) +
  scale_linetype_manual(name = "" # distinguish variables by line type NOTE: labels must match colour scale above
                        , values=c(3,1,3,1)
                        , labels=c("Benin: exec. oversight","Benin: exec. respects const."
                                   ,"Chad: exec. oversight","Chad: exec. respects const.")) + 
  theme(legend.position="bottom") # move legend
# add IDEA logo
grid::grid.raster(idea_logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(2.5, 'cm'))
dev.off()

#### Example 3: Regime performance heatmap for Latin America ####

# first do some filtering and transformations with the regime performance data
LatAm_regime_perf_dat <- regime_perf_dat %>%
  filter(Subregion %in%  c("Central America","South America") | Country %in% c("Cuba","Dominican Republic","Haiti")) %>% #&
  filter(!Country %in% c("Central America","South America")) %>%
  # create a new variable that replaces Democracy with the democratic performance variable
  mutate(regime_and_perf = case_when(Regime == "Authoritarian Regime" ~ "Authoritarian regime",
                                     Regime == "Hybrid Regime" ~ "Hybrid regime",
                                     Regime == "Democracy" ~ `Democratic Performance`)) %>%
  # recode the regime and performance variable (otherwise ggplot2 orders the levels alphabetically)
  mutate(regime_and_perf = case_when(regime_and_perf == "Authoritarian regime" ~ "5",
                                     regime_and_perf == "Hybrid regime" ~ "4",
                                     regime_and_perf == "Low" ~ "3",
                                     regime_and_perf == "MidR" ~ "2",
                                     regime_and_perf == "High" ~ "1")) %>%
  dplyr::select(c(Country, Year, regime_and_perf)) #%>% # select variables of interest

# Use IDEA colours for regime type
#Greens for democratic performance
# darkest #49cba8
# medium #7fe0cb
# light #bfefe5

jpeg("LatAm_regime_heatmap_5.jpg", width = 9, height = 6, units = 'in', res = 600) # open an image file
# start the plot, use forcats to reorder the country names so they appear in alphabetical order
ggplot(data = subset(LatAm_regime_perf_dat, Year > 1976), aes(x=Year, y=forcats::fct_rev(Country), fill=regime_and_perf)) + geom_tile() +
  # manually name the regime types and assign them colours
  scale_fill_manual(labels = c("Régimen democrático (alto)","Régimen democrático (medio)","Régimen democrático (bajo)","Régimen híbrido","Régimen autoritario"),
                    values = c("#49cba8","#7fe0cb","#bfefe5","#383880","#e26429")) +
  labs(title = "Tipo de régimen", x = "", y = "", # add labels
       caption = "Fuente: Datos del Estado Global de la Democracia, IDEA Internacional") +
  theme_minimal() + # get rid of chart junk
  theme(legend.title = element_blank())
# add idea logo (on the right this time)
grid::grid.raster(idea_logo, x = 0.9, y = 0.01, just = c('left', 'bottom'), width = unit(2, 'cm'))
dev.off()