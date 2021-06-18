# basic figure styles for the GSoD Report

library(extrafont)
# for windows
windowsFonts(sans="Roboto") # make sure that ggplot will use the roboto font 
loadfonts(device="win")
loadfonts(device="postscript")

library(tidyverse)
library(ggrepel)
library(magick)
library(maps)
library(ggmap)


# define a bunch of colors that follow IDEA's definitions ####
# basic pallette
# teal, green, magenta, yellow, indigo, orange
idea_colours <- c("#20B6E8","#8CCAAE","#E62D5F","#FFDD00","#43358B","#EB6209")

# democratic performance colors
# green, green, green, blue, orange
demperf_colours <- c("#8CCAAE","#badfce","#dcefe6","#336A97","#EB6209")

# regime classification colours
# basic version of the demperf colours
regime_colours <- demperf_colours[c(1,4,5)]

# colors for showing indicator performance levels
traffic_colours <- idea_colours[c(2,4,3)]

# colors for advancers and decliners
adv_decl_colours <- c("#004587","#890A3E")

# read in GSODI data
gsodi <- read.csv("GSoDI v5 (1975-2020).csv") #%>%
# filter(ID_country_code < 961)
# head(gsodi)

# read in long version of GSODI data
gsod_long <- read.csv("GSODI Long v5 (1975-2020).csv")
head(gsod_long)

# make democratic performance name a correctly ordered factor
gsodi$democratic_performance_name <- fct_reorder(gsodi$democratic_performance_name, gsodi$democratic_performance_numeric)
gsodi$regime_status_name <- fct_reorder(gsodi$regime_status_name, gsodi$democratic_performance_numeric)

gsodi_long$democratic_performance_name <- fct_reorder(gsodi_long$democratic_performance_name, gsodi_long$democratic_performance_numeric)
gsodi_long$regime_status_name <- fct_reorder(gsodi_long$regime_status_name, gsodi_long$democratic_performance_numeric)

# define the list of subattributes
gsod_subattributes <- c("Clean Elections","Inclusive Suffrage","Free Political Parties","Elected Government",
                        "Access to Justice","Civil Liberties","Social Rights and Equality",
                        "Effective Parliament","Judicial Independence","Media Integrity",
                        "Absence of Corruption","Predictable Enforcement",
                        "Civil Society Participation","Electoral Participation","Direct Democracy","Local Democracy")

##

# line plot ####

# prepare the data
gsodi_long %>%
  filter(ID_country_name == "Iran") %>% # filter the country of interest
  filter(ID_variable_name == "Clean Elections") %>% # filter the variable of interest 
# begin the plot
ggplot(aes(x=ID_year, y=value, ymin=lower_value, ymax=upper_value, color = ID_variable_name)) + # define the axes and bound for CI if applicable
  ylim(0,1) + # set the limits of the y axis to cover the full scale of the indicator
  scale_x_continuous(breaks = scales::pretty_breaks()) + # make sure that there are no strange numbers on the x axis
  scale_color_manual(values = idea_colours[c(1)]) + # make the lines use the IDEA define colors
  geom_line(size=2) + # draw a line, make it bigger
  geom_ribbon(linetype=0, alpha=0.1, fill = idea_colours[c(1)]) + # add a ribbon for the CI, make it the same color as the line
  theme_minimal() + # remove chart junk
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_line()) + # remove more chart junk
  theme(legend.position = "none") + # remove the legend if there is only one indicator being plotted
  labs(x = "", y = "Indicator value", # axis labels NB add a title if necessary
       caption = "Source: International IDEA, The Global State of Democracy Indices, 1975-2020") +
  geom_label_repel(data = . %>% filter(ID_year %in% c(1975,2020)), size = 4, 
                   show.legend = FALSE, point.padding	= .25, min.segment.length = 0, 
                   aes(label =round(value, digits = 2)))
ggsave("clean_elec_iran_1975-2020.png", width = 9, height = 6, units = "in") # save your plot as a png


# barplot ####

gsodi_long %>%
  filter(ID_year == 2020 & ID_region_name == "Asia/Pacific") %>% # filter the region and year
  filter(ID_variable_name == "Representative Government") %>% # filter down to one row per country year
  group_by(ID_subregion_name) %>% # group so the counts will work
  count(regime_status_name) %>% # count the regime types within the groups
  group_by(ID_subregion_name) %>% # group again so we can get more counts
  mutate(subregion_countries = sum(n)) %>% # count the number of countries in each subregion
  mutate(percentage = scales::percent(n/subregion_countries)) %>% # create a variable with the percentage of each regime type in each subregion
  mutate(proportion = (n/subregion_countries)) %>% # get a proportion of each regime type
  mutate(label_text = paste(percentage," (",n,")", sep = "")) %>% # create a new variable that pastes together the count and the percentage to get a nice label
ggplot(aes(ID_subregion_name, y=proportion, fill = regime_status_name)) + # basic elements of plot
  geom_col() + # draw columns
  scale_fill_manual(values = regime_colours) + # align colours with IDEA palette
  scale_y_continuous(labels = scales::percent) + # scale the y axis as a percentage
  theme_minimal() + # remove chart junk
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_line()) +
  theme(legend.title = element_blank()) + # remove legend title
  theme(legend.position = "bottom") + # move legend to bottom
  labs(title = "Regime types in Asia and the Pacific", # add labels
       subtitle = "2020",
       x = "", y = "Percentage of countries by subregion") +
  geom_text(aes(label = label_text), # add the labels to the column segments using variable created above
            position = position_stack(vjust = .5))
ggsave("regime_types_AP_2020.png",  width = 9, height = 6, units = "in") # save the plot

# dumbell plot ####

# prepare the data
gsodi_long %>%
  filter(ID_year %in% c(2015, 2020)) %>% # select years of interest
  filter(ID_variable_name == "Clean Elections") %>% # select indicator of interest
  dplyr::select(ID_country_name, ID_year, value, democratic_performance_name) %>% # subset the data for simplicity
  group_by(ID_country_name) %>% # group by the variable that will make the calculation work
  mutate(difference = value - lag(value)) %>% # compute the difference. note that in this case we only need to lag one because we already subsetted the years
  ungroup() %>%
  # arrange(difference) # uncomment, print out, and check the top and bottom
  filter(ID_country_name %in% c("Sudan","Yemen","Venezuela","Mauritius","Poland",
                                "Benin","Botswana","Code d'Ivoire","Brazil","Cambodia")) %>% # manually select the countries of interest
  mutate(paired = rep(1:(n()/2),each=2), # pair the 2015 and 2020 values for each country
         year=factor(ID_year)) %>%
# begin the plot
ggplot(aes(x= value, y= reorder(ID_country_name,-value))) + # set the parameters of the plot
  theme_minimal() + # remove chart junk
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_line()) + # remove more chart junk
  geom_line(aes(group = paired))+ # draw a line to connec the points
  geom_point(aes(color=democratic_performance_name), size=4) + # draw the points
  labs(y="Country",x="Clean Elections") + # add labels to the axes
  scale_color_manual(values = demperf_colours[c(2,4,5)]) + # use the democratic performance colors. NB adjust to match levels actually found in data
  theme(legend.title = element_blank()) # remove legend title
ggsave("clean_elec_dumbbell_all_data_v5.png", width = 9, height = 6, units = "in") # save the plot as a png



# donut plot ####

# make a little table with the counts of the categories
donut_data <- gsodi_long %>%
  filter(ID_year == 2020 & ID_variable_name == "Clean Elections") %>% # subset to the year and indicator of interest
  group_by(perform_class_var_name) %>% # group the observations by the performance level
  count() # summarize the data into a simple count
# Compute percentages
donut_data$fraction = donut_data$n / sum(donut_data$n)
# Compute the cumulative percentages (top of each rectangle)
donut_data$ymax = cumsum(donut_data$fraction)
# Compute the bottom of each rectangle
donut_data$ymin = c(0, head(donut_data$ymax, n=-1))
# Compute label position
donut_data$labelPosition <- (donut_data$ymax + donut_data$ymin) / 2
# create a good label by pasting the variable name and performance class together with the percentage
donut_data$label <- paste0(donut_data$perform_class_var_name, "\n", round(donut_data$fraction, digits = 3)*100,"%")

# Make the plot
ggplot(donut_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=perform_class_var_name)) + # define the basic components
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5, fill = "white") +
  scale_fill_manual(values = traffic_colours[c(3,2,1)]) + #in this case, we need to reorder the colors to match the labels
  coord_polar(theta="y") + # this wraps the polygons into the donut shape
  xlim(c(2, 4)) + # this creates the donut hole
  theme_void() + # remove all chart junk
  theme(legend.position = "none") + # remove legend
  labs(caption = "Source: International IDEA, The Global State of Democracy Indices, 1975-2020") # add caption
ggsave("clean_elections_donut.png", width = 10, height = 6, units = "in") # save as a png

# heatmaps for regime types by region ####

gsodi %>%
  filter(ID_region_name == "Latin America/Caribbean" & ID_year > 1999 &
           !is.na(democratic_performance_name)) %>%
  ggplot(aes(x=ID_year, y=forcats::fct_rev(ID_country_name), fill=democratic_performance_name)) + geom_tile() +
  scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0)) +
  scale_fill_manual(values = demperf_colours) +
  labs(title = "Democratic performance over time", subtitle = "Latin America/Caribbean", x = "", y = "",
       caption = "Source: International IDEA, The Global State of Democracy Indices, 1975-2020") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_line()) +
  theme(legend.title = element_blank(), legend.position = "bottom")
ggsave("heatmap_2000_Latin America_Caribbean.png", width = 10, height = 6, units = "in")


#### Map of regime types ####

# prepare regime data
regime_map_data <- gsodi %>%
  filter(ID_country_code < 961 & ID_year == 2020) %>%
  # filter(!is.na(democratic_performance_name)) %>%
  dplyr::select(ID_country_name, democratic_performance_name) %>%
  mutate(ID_country_name = case_when(ID_country_name == "United States" ~ "USA", TRUE ~ ID_country_name)) %>%
  mutate(ID_country_name = case_when(ID_country_name == "United Kingdom" ~ "UK", TRUE ~ ID_country_name)) %>%
  mutate(ID_country_name = case_when(ID_country_name == "Cote d'Ivoire" ~ "Ivory Coast", TRUE ~ ID_country_name)) %>%
  mutate(ID_country_name = case_when(ID_country_name == "Czechia" ~ "Czech Republic", TRUE ~ ID_country_name)) %>%
  mutate(ID_country_name = case_when(ID_country_name == "Republic of Korea" ~ "South Korea", TRUE ~ ID_country_name)) %>%
  mutate(ID_country_name = case_when(ID_country_name == "Democratic People's Republic of Korea" ~ "North Korea", TRUE ~ ID_country_name)) #%>%
str(regime_map_data)
# theme to ditch chart junk 
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

earth <- map_data("world")

regime_shapes <- left_join(earth, regime_map_data, by = c("region" = "ID_country_name"))
head(regime_shapes)

jpeg("Africa_regimes_map.jpg", width = 9, height = 9, units = 'in', res = 600)
ggplot(data = regime_shapes, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = regime_shapes, aes(fill = democratic_performance_name)) +
  geom_polygon(color = "grey", fill = NA, size = 0.25) +
  theme_bw() +
  ditch_the_axes +
  coord_fixed(xlim = c(-20, 52),  ylim = c(-32, 35), ratio = 1.3) + # set the limits of the plot area to center on Africa
  scale_fill_manual(values = demperf_colours, na.value = "lightgrey") +
  theme(legend.position = "bottom") +
  labs(fill = "", caption = "Source: International IDEA, The Global State of Democracy Indices, 1975-2020") +
  labs(title = "Regime Types in Africa", subtitle = "2020") 
dev.off()

