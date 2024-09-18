### Doctor of Philosophy (Ph.D.)
## Chapter 1: Changes in the marine fishing industry of Malaysia and Singapore over the past century
## Clement Ng

#### Historical Ecology ####
## Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Load libraries
## Packages used for wrangling 
library(tidyverse)
library(readxl)
library(rrapply)

## Packages for Trend Analysis
library(Kendall)

## Packages for Change Point Analysis
#library(bcp)
#devtools::install_version("bcp", version = "4.0.3", repos = "https://cran.r-project.org/src/contrib/Archive/bcp/bcp_4.0.3.tar.gz")

## Packages for Granger Causality Test and Information Criterion for selecting lag intervals 
library(tseries)
library(vars)
library(lmtest)

## Packages for figures and maps
library(ggmap)
library(ggplot2)

cbPalette <- c("#D55E00", "#E69F00", "#009E73", "#56B4E9", "#0072B2", "#F0E442", "#CC79A7", "#999999") 

## Load datasets 
fishery_catch <- read_excel("1931_2021_Fishery_Data.xlsx", sheet = 1)
number_fishermen <- read_excel("1921_2021_Number of Fishermen.xlsx", sheet = 1, col_names = TRUE)
number_boat <- read_excel("1927_2021_Boat Types.xlsx", sheet = 1)
oil_prices <- read.csv('BP-CRUDE_OIL_PRICES.csv')

#load("2024_Historical_Ecology.RData")

######################################################################################################
######################################################################################################

colnames(fishery_catch)[colnames(fishery_catch) == 'Trengganu'] <- 'Terengganu'
colnames(number_fishermen)[colnames(number_fishermen) == 'Trengganu'] <- 'Terengganu'
colnames(number_boat)[colnames(number_boat) == 'Trengganu'] <- 'Terengganu'

### Fishery catch data ####
## Capture information for all fishery products per State and Year from 1931 to 2021. 
## All data converted from katties (estimate of a single basket) to picul (traditional estimate of how much a man can carry; modern definition is 60 kg) to a ton. 
## Names were converted, to the best of my abilities, from their local pre-war names to their post-war Scientific names. 

str(fishery_catch)
fishery_catch <- fishery_catch %>% mutate_at(c(13:28), as.numeric) # convert columns to numeric format to reflect catch data
fishery_catch <- fishery_catch %>% 
  subset(select = c(1:3, 10:11, 13:28)) %>%
  pivot_longer(!c(1:5), names_to = "State", values_to = "Harvest")
## Remove columns of fish names (FishBase or Reports) and type. 

colSums(is.na(fishery_catch))

## Identify all categories without a grouping name
fishery_catch %>%
  filter(is.na(Grouping_Name)) %>%
  distinct(Report_Malay) %>%
  print(n=Inf) # Identify potential species based on the above list. 
fishery_catch[, 7][is.na(fishery_catch[, 7])] <- 0 # replace NAs with zero
# Replace zero values with NAs for years with no catch data. 

## Create a data frame which combines harvests across year, state and grouping name. 
## This data frame ignores repeated report names in the data frame, for example, fish which are of different sizes. 
fishery_catch <- aggregate(Harvest ~ Year + State + Grouping_Name, fishery_catch, sum)

## Create additional grouping categories for each Region and Country
fishery_catch <- fishery_catch %>%
  mutate(Region = case_when(State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West" ~ "West Coast",
                            State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East" ~ "East Coast",
                            State == "Sabah" | State == "Sarawak" | State == "Labuan" ~ "Borneo",
                            State == "Singapore" ~ "Singapore",
                            FALSE ~ "others"))

fishery_catch <- fishery_catch %>% 
  mutate(Country = case_when(Region == "West Coast" | Region == "East Coast" | Region == "Borneo" ~ "Malaysia",
                             Region == "Singapore" ~ "Singapore",
                             FALSE ~ "others"))

colnames(fishery_catch)
data.table::setcolorder(fishery_catch, c("Year","Country","Region","State","Grouping_Name","Harvest"))

### Remove groups comprising of freshwater fish and other seafood products. 
fishery_catch <- fishery_catch %>%
  filter(!Grouping_Name == "Anabas testudineus" & !Grouping_Name == "Belachan" & !Grouping_Name == "Boiled Fish" & !Grouping_Name == "Chirocentridae Roe" & !Grouping_Name == "Clarias spp.; Pangasius spp.; Wallago  spp. and Macrones spp. or Mystus spp." & 
           !Grouping_Name == "Cyprinidae" & !Grouping_Name == "Diadromous Shads Roe" & !Grouping_Name == "Dried Prawns" & !Grouping_Name == "Fertiliser" & !Grouping_Name == "Fish Dust" & 
           !Grouping_Name == "Fish Maws" & !Grouping_Name == "Fish Sauce" & !Grouping_Name == "Freshwater Prawns" & !Grouping_Name == "Iced Fish" & !Grouping_Name == "Ophiocephalus spp." &
           !Grouping_Name == "Osphronemus goramy" & !Grouping_Name == "Pickled Fish" & !Grouping_Name == "Prawn Dust" & !Grouping_Name == "Prawn Shell" & !Grouping_Name == "Ray Skins" & 
           !Grouping_Name == "Scomberomorus spp. Roe" & !Grouping_Name == "Sharks' Fins" & !Grouping_Name == "Toxotes spp." & !Grouping_Name == "Trichogaster spp.")

######################################################################################################
######################################################################################################

### Generate data frames to produce preliminary plots and check the data ####
state_catch <- aggregate(Harvest ~ Year + State + Grouping_Name, fishery_catch, sum) 
species_catch <- aggregate(Harvest ~ Year + Grouping_Name, fishery_catch, sum) 
country_catch <- aggregate(Harvest ~ Year + Country, fishery_catch, sum) 
overall_catch <- aggregate(Harvest ~ Year, fishery_catch, sum)

state_catch[state_catch == 0] <- NA
species_catch[species_catch == 0] <- NA
country_catch[country_catch == 0] <- NA
overall_catch[overall_catch == 0] <- NA

## Create a dummy catch data frame to merge the harvest data.
years <- data.frame(Year = c(1930:2025))
states <- data.frame(State = unique(fishery_catch$State))
countries <- data.frame(Country = unique(fishery_catch$Country))
groups <- data.frame(Grouping_Name = unique(fishery_catch$Grouping_Name)) 

## The crossing function creates a long data frame with all possible combinations between the three variables. 
catch_states <- crossing(years, states, groups) 
state_catch <- merge(catch_states, state_catch, by = c("Year","State","Grouping_Name"), all = T)
state_catch$Harvest <- state_catch$Harvest / 1000 # change the units to thousand tonnes.
state_catch[is.na(state_catch)] <- 0
## Amend the NA values to zero, as for plotting as it may reflect two results.
## Either the data is not recorded in fishery reports or that there was no catch in the given year.

catch_species <- crossing(years, groups) 
species_catch <- merge(catch_species, species_catch, by = c("Year","Grouping_Name"), all = T)
species_catch$Harvest <- species_catch$Harvest / 1000
species_catch[is.na(species_catch)] <- 0

country_catch$Harvest <- country_catch$Harvest / 1000

catch_overall <- merge(years, overall_catch, by = c("Year"), all = T)
overall_catch$Harvest <- overall_catch$Harvest / 1000
overall_catch[is.na(overall_catch)] <- 0

rm(years, states, groups, countries, catch_states, catch_species, catch_overall) # remove all dummy variables

### Create a preliminary plot on the change in fish harvests between states over time. 
## Filter function removes all fishery based products and allows us to investigate seafood only. 
state_catch %>% 
  ggplot(aes(x=Year, y=Harvest, group=Grouping_Name, fill=Grouping_Name))+
  geom_area(colour='black', linewidth=0.15, position = "stack")+
  facet_wrap(.~State, scales = "free") +
  scale_x_continuous(breaks = c(1930,1950,1970,1990,2010), labels = c(1930,1950,1970,1990,2010))+
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = 'none')

## Prepare a plot for Caesionidae
caesionidae_data <- fishery_catch
caesionidae_data <- caesionidae_data %>% filter(Grouping_Name == "Caesionidae")
caesionidae_data <- aggregate(Harvest ~ Year + Region + Grouping_Name, caesionidae_data, sum) 
caesionidae_data[caesionidae_data == 0] <- NA
caesionidae_data$Harvest <- caesionidae_data$Harvest / 1000

plot7 <- caesionidae_data %>% 
  mutate(Region = case_when(Region == "Borneo" ~ "East Malaysia", T ~ Region)) %>%
  mutate(Region = case_when(Region == "East Coast" ~ "East Peninsular Malaysia", T ~ Region)) %>%
  mutate(Region = case_when(Region == "West Coast" ~ "West Peninsular Malaysia", T ~ Region)) %>%
  ggplot(aes(x=Year, y=Harvest, group=Region))+
  geom_point(aes(colour=Region, shape=Region), size=2.5)+
  scale_x_continuous(breaks = c(1930,1950,1970,1990,2010), labels = c(1930,1950,1970,1990,2010))+
  scale_y_continuous(name='Landings (thousand tonnes)', limits = c(0, 6), breaks = c(0,1,2,3,4,5,6), labels = c('0.0','1.0','2.0','3.0','4.0','5.0','6.0'))+
  scale_shape_manual(values=c(15,16,17,18))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", size=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=12),
        legend.key=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot7

### Create a preliminary plot on the change in species specific fish harvests over time. 
## This includes the removal of certain fish groups, like fish products and fertilisers. 
## Inclusion of the filter code allows us to explore how trends change for each fish product, in this case, groupers. 
unique(species_catch$Grouping_Name)
species_catch %>% 
  filter(Grouping_Name == "Caesionidae") %>%  ## Change the variable here to assess other fish products 
  ggplot(aes(x=Year, y=Harvest, group=Grouping_Name, fill=Grouping_Name))+
  geom_line(linewidth=0.4, position = "stack")+
  geom_area(linewidth=0.15, position = "stack")+
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = 'none'
  )

country_catch %>% 
  ggplot(aes(x=Year, y=Harvest, group=Country, color=Country))+
  geom_point(size=2.2, aes(shape = Country), color="black")+ 
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", size=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

### Create a preliminary plot describing the change in fish harvests over time. 
## There is an increasing harvest trend in Malaysia and Singapore over the past century. 
overall_catch %>% 
  ggplot(aes(x=Year, y=Harvest))+
  geom_point(size=1)+ 
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", size=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = 'none'
  )

## There is consistent harvest data in the pre war period from 1931 to 1938 and the post war period from 1968 to 2021. 
## There are years where we lack species specific catch information, from 1948 to 1958 and 1961 to 1967. 

#rm(state_catch, species_catch)

######################################################################################################
######################################################################################################

### Addition of data from the early post war period that does not include species specific catch information #### 
## Merge with the overall catch information to assess the large scale trends. 
## While the information is split between east and west coast, it provides an indication to the intensity of harvest during those years. 
post_fishery_catch <- read_excel("1946_1967_Fishery_Data.xlsx", sheet = 1)
post_fishery_catch <- as.data.frame(post_fishery_catch)
post_fishery_catch$Harvest <- post_fishery_catch$Harvest / 1000

post_fishery_catch <- post_fishery_catch %>% subset(select = c(Year, Harvest))
overall_catch <- rbind(overall_catch, post_fishery_catch) 
overall_catch <- overall_catch %>% arrange(Year) 

overall_catch %>% 
  ggplot(aes(x=Year, y=Harvest))+
  geom_point(size=1)+ 
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  scale_x_continuous(limits=c(1930, 2022), breaks= c(1930,1950,1970,1990,2010), labels= c('1930','1950','1970','1990','2010'))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", size=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = 'none'
  )

## While there appears to be an increasing overall trend, there are years which lack harvest information. 
## For example, in Singapore and the East Malaysian states of Brunei, Sabah, and Sarawak during certain years in the overall time series. 

post_fishery_catch$Country <- "No information"
combined_overall <- bind_rows(lst(country_catch, post_fishery_catch))
combined_overall <- combined_overall %>% arrange(Year)
combined_overall$Country <- factor(combined_overall$Country, levels=c('Malaysia','Singapore','No information'))
combined_overall <- combined_overall %>% 
  mutate(Harvest = case_when(Country == "Singapore" ~ Harvest * 10, 
                             TRUE ~ Harvest))
combined_overall

country_catch %>% 
  filter(Country == "Singapore") %>%
  drop_na(Harvest) %>%
  summarise(mean = mean(Harvest), sd = sd(Harvest), n = n(), se = sd / sqrt(n))

country_catch %>% 
  filter(Country == "Malaysia") %>%
  summarise(mean = mean(Harvest), sd = sd(Harvest), n = n(), se = sd / sqrt(n))

plot6 <- combined_overall %>% 
  ggplot()+
  #geom_vline(xintercept = 1976, colour = 'grey35', linetype = 'dashed', size = 0.5)+ 
  geom_vline(xintercept = 1986, colour = 'grey35', linetype = 'dashed', size = 0.5)+ 
  #geom_vline(xintercept = 1991, colour = 'grey35', linetype = 'dashed', size = 0.5)+
  geom_vline(xintercept = 1996, colour = 'grey35', linetype = 'dashed', size = 0.5)+
  geom_vline(xintercept = 2005, colour = 'grey35', linetype = 'dashed', size = 0.5)+
  geom_line(data = overall_catch, aes(x=Year, y=Harvest), colour = 'black', alpha = 0.8, linewidth = 0.7)+
  geom_point(aes(x=Year, y=Harvest, color=Country, shape=Country), size = 2.1)+
  scale_y_continuous(name = 'Landings (thousand tonnes)', sec.axis = sec_axis(~. / 100, name = "Landings in Singapore (hundred tonnes)"))+
  scale_x_continuous(limits=c(1928, 2023), breaks = c(1930,1950,1970,1990,2010), labels = c(1930,1950,1970,1990,2010))+
  scale_shape_manual(values=c(19, 17, 15))+
  scale_color_manual(values=c('#F8766D', '#619CFF', '#00BA38'))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", size=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.key=element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
plot6

state_catch_aggregate <- aggregate(Harvest ~ Year + State, fishery_catch, sum) 
state_catch_aggregate[state_catch_aggregate == 0] <- NA
state_catch_aggregate <- na.omit(state_catch_aggregate)
state_catch_aggregate

## State catch
state_catch %>%
  mutate(State = case_when(State == "Johor West" ~ "Johore West", 
                           T ~ State)) %>%
  ggplot(aes(x=Year, y=Harvest))+
  geom_area(aes(fill=Grouping_Name), colour=NA, position = "stack")+
  stat_summary(fun = sum, geom = "line", size = 0.5, colour='black') +
  facet_wrap(.~State, ncol = 4, scales = 'free') +
  scale_x_continuous(breaks = c(1920,1940,1960,1980,2000,2020), labels = c(1920,1940,1960,1980,2000,2020))+
  scale_y_continuous(name='Landings (thousand tonnes)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.position = 'none')

######################################################################################################
######################################################################################################

### Bayesian Change Point Analysis #### 
## Test the data for the overall catch data frame 
set.seed(100)

overall_bcp <- overall_catch
overall_bcp <- bcp(overall_bcp$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000)
plot(overall_bcp)

overall_bcp_plot <- cbind(overall_catch$Year, overall_bcp$posterior.prob)
overall_bcp_plot <- as.data.frame(overall_bcp_plot)

overall_bcp_plot <- overall_bcp_plot %>% rename(Year = V1, posterior.prob = V2)
overall_bcp_plot <- round(overall_bcp_plot, 8)
overall_bcp_plot <- overall_bcp_plot %>% arrange(desc(posterior.prob))

## Set threshold for posterior probability as 0.85
overall_bcp_plot %>% filter(posterior.prob > 0.85)

overall_bcp_plot %>% 
  filter(!posterior.prob == 'NA') %>% 
  ggplot(aes(y=posterior.prob, x=Year))+
  geom_line(linewidth = 0.5)+ 
  geom_hline(yintercept = 0.70, colour = 'red', linetype = 'dashed')+
  theme_bw()
## We are unable to perform a change point analysis for 2021. 

### Calculate the rolling mean for the overall catch data 
years <- data.frame(Year = c(1931:2021))
years <- left_join(years, overall_catch) 

rolling <- rollapply(years, width=3, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")
rolling <- as.data.frame(rolling)
rolling <- rolling %>% filter(!(Year=="1931.5"))
rolling[nrow(rolling) + 1,] = c("2021", NA)
rolling <- rolling %>% 
  mutate_at(c(1:2), as.numeric) %>% 
  rename(rolling = Harvest)

overall_catch <- left_join(overall_catch, rolling)
rm(years)

## There are five significant changes in the overall harvest trends in Malaysia and Singapore. 
plot1 <- overall_catch %>% 
  ggplot(aes(x=Year, y=Harvest))+
  geom_point(size=1.5)+ 
  #geom_line(aes(colour=rolling), colour='black', data=overall_catch)+
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  scale_x_continuous(limits=c(1930, 2022), breaks= c(1930,1950,1970,1990,2010), labels= c('1930','1950','1970','1990','2010'))+ 
  #geom_vline(xintercept = 1976, colour = 'red', linetype = 'dashed')+ 
  #geom_vline(xintercept = 1986, colour = 'red', linetype = 'dashed')+ 
  #geom_vline(xintercept = 1991, colour = 'red', linetype = 'dashed')+
  #geom_vline(xintercept = 1996, colour = 'red', linetype = 'dashed')+
  #geom_vline(xintercept = 2005, colour = 'red', linetype = 'dashed')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot1

## Observed dips in the overall trend may reflect changes in the fishing industry. 
## It is difficult to draw conclusions as this varies between State and for different fish groups. 
## In terms of the overall trend, there were five significant changes in the harvest rate of fishery-related products from Malaysia and Singapore. 

######################################################################################################
######################################################################################################

### Number of fisherman #### 
## Total number of fishermen in each state in Malaysia and Singapore from 1907 to 2021. 
number_fishermen <- number_fishermen %>% mutate_at(c(2:17), as.numeric) 
colSums(is.na(number_fishermen))

number_fishermen <- number_fishermen %>% 
  pivot_longer(c(2:17), names_to = "State", values_to = "Fishermen")
unique(number_fishermen$State)
number_fishermen

## Create additional grouping categories for each Region and Country
number_fishermen <- number_fishermen %>%
  mutate(Region = case_when(State == "Perlis" | State == "Kedah" | State == "Penang" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johore West" ~ "West Coast",
                            State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East" ~ "East Coast",
                            State == "Sabah" | State == "Sarawak" | State == "Labuan" ~ "Borneo",
                            State == "Singapore" ~ "Singapore",
                            FALSE ~ "others"))

number_fishermen <- number_fishermen %>% 
  mutate(Country = case_when(Region == "West Coast" | Region == "East Coast" | Region == "Borneo" ~ "Malaysia",
                             Region == "Singapore" ~ "Singapore",
                             FALSE ~ "others"))

colnames(number_fishermen)
data.table::setcolorder(number_fishermen, c("Year","Country","Region","State","Fishermen"))

### Bayesian Change Point Analysis ####
set.seed(100)

## Remove NAs to prepare the data for the change point analysis
number_fishermen <- na.omit(number_fishermen)

### Assess the probability of a change in mean and posterior mean for each state ####
## For the change point analysis, we will set a posterior of 0.05, burnin of 1000 iterations, and mcmc repetition of 11000 iterations.  
fishermen_bcp <- number_fishermen %>% 
  group_by(State) %>%
  group_map(~ bcp::bcp(.x$Fishermen, p0 = 0.05, burnin = 1000, mcmc = 11000))
fishermen_bcp 

name_fishermen <- number_fishermen %>% 
  group_by(State) %>% 
  summarize(list = list(State)) %>% 
  subset(select = State)
name_fishermen$group <- 1:nrow(name_fishermen) 

### Compress the nested data into a single compressed data frame
fishermen_bcp <- rrapply(fishermen_bcp, how = 'melt') 

## Extract the mean and posterior mean from the state-specific bcp 
fishermen_bcp_data <- fishermen_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
fishermen_bcp_data <- lapply(fishermen_bcp_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of groups 
fishermen_bcp_data <- fishermen_bcp_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
fishermen_bcp_data$id <- seq.int(nrow(fishermen_bcp_data)) 
fishermen_bcp_data

## Posterior probability values
fishermen_bcp_prob <- fishermen_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
fishermen_bcp_prob <- lapply(fishermen_bcp_prob$value, as.data.frame)
fishermen_bcp_prob <- fishermen_bcp_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
fishermen_bcp_prob$id <- seq.int(nrow(fishermen_bcp_prob))
fishermen_bcp_prob

######################################################################################################
######################################################################################################

### Compile the data and merge with the grouping information 
fishermen_data <- merge(fishermen_bcp_data, fishermen_bcp_prob, by = c('id','group')) 
fishermen_data <- fishermen_data %>% arrange(id) 
fishermen_data <- merge(fishermen_data, name_fishermen, by = "group")
fishermen_data <- fishermen_data %>% 
  arrange(State, year)
fishermen_data

fishermen_year <- number_fishermen %>% 
  arrange(State, Year) %>% 
  subset(select = c(Year, State))
fishermen_year$id <- seq.int(nrow(fishermen_year))

fishermen_data <- merge(fishermen_data, fishermen_year[,c("Year","id")], by = "id")
fishermen_data <- fishermen_data %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(fishermen_data)
data.table::setcolorder(fishermen_data, c("year","State","values","posterior.prob"))
fishermen_data

## Create a dummy number of fishermen data frame to merge the harvest data.
years <- data.frame(Year = c(1900:2025))
states <- data.frame(State = unique(number_fishermen$State))

## The crossing function creates a long data frame with all possible combinations between the two variables. 
dummy_fishermen <- crossing(years, states) 
dummy_fishermen <- dummy_fishermen %>% rename(year = Year)
fishermen_data <- merge(fishermen_data, dummy_fishermen, by = c("year","State"), all = T)
fishermen_data <- fishermen_data %>% mutate_at(vars(values), ~replace_na(., 0)) 
fishermen_data[fishermen_data == 0] <- NA
fishermen_data <- fishermen_data %>% drop_na(values) 
fishermen_data

## Create a posterior probability data frame based on the fishermen data
fishermen_prob <- fishermen_data %>% 
  subset(select = -c(values)) %>%
  filter(posterior.prob >= 0.70)
fishermen_prob

## Amend the NA values to zero, as for plotting as it may reflect two results.
## Either the data is not recorded in fishery reports or that there was no fishermen in the given year.
rm(years, states, dummy_fishermen)

## Changes in the number of fishermen across state and over time.
plot2 <- fishermen_data %>% 
  mutate(State = case_when(State == "Penang" ~ "Penang and Province Wellesley", 
                           T ~ State)) %>%
  ggplot(aes(x=year, y=values))+
  geom_point(aes(colour=State, group=State), colour='black', size=0.5)+
  facet_wrap(.~State, scales='free')+ 
  scale_x_continuous(limits=c(1900,2023), breaks=c(1900,1930,1960,1990,2020), labels=c(1900,1930,1960,1990,2020), name='Year')+
  scale_y_continuous(name='Number of Fishermen')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot2

fishermen_prob %>% group_by(year) %>% summarize(n = n()) %>% print(n=Inf)
fishermen_prob %>% 
  ggplot(aes(x=year))+
  geom_histogram()

######################################################################################################
######################################################################################################

### Boat type #### 
## Total number of boat type in each state in Malaysia and Singapore from 1910 to 2021. 
number_boat$Type[is.na(number_boat$Type)] <- "All" 
number_boat <- number_boat %>% mutate_at(c(3:19), as.numeric) 
number_boat <- number_boat %>% 
  pivot_longer(c(3:19), names_to = "State", values_to = "Number") 
number_boat

unique(number_boat$Type)
boat_type <- number_boat %>% 
  filter(!(Type == "All")) %>% 
  mutate(power_type = case_when(Type == "Powered" | Type == "Outboard" | Type == "Inboard" ~ "Powered",
                                Type == "Non-powered" ~ "Non-powered",
                                FALSE ~ "Others"))
boat_type

number_boat <- number_boat %>% 
  group_by(State, Year) %>%
  summarize(Type = sum(Number))
number_boat <- number_boat %>% 
  rename(Number = Type)
number_boat

### Bayesian Change Point Analysis ####
set.seed(100)

## Remove NAs to prepare the data for the change point analysis
number_boat <- na.omit(number_boat)

### Assess the probability of a change in mean and posterior mean for each state ####
## For the change point analysis, we will set a posterior of 0.05, burnin of 1000 iterations, and mcmc repetition of 11000 iterations.  
number_boat_bcp <- number_boat %>% 
  group_by(State) %>%
  group_map(~ bcp::bcp(.x$Number, p0 = 0.05, burnin = 1000, mcmc = 11000))
number_boat_bcp 

name_boat <- number_boat %>% 
  group_by(State) %>% 
  summarize(list = list(State)) %>% 
  subset(select = State)
name_boat$group <- 1:nrow(name_boat) 

### Compress the nested data into a single compressed data frame
number_boat_bcp <- rrapply(number_boat_bcp, how = 'melt') 

## Extract the mean and posterior mean from the state-specific bcp 
number_boat_bcp_data <- number_boat_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
number_boat_bcp_data <- lapply(number_boat_bcp_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of groups 
number_boat_bcp_data <- number_boat_bcp_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
number_boat_bcp_data$id <- seq.int(nrow(number_boat_bcp_data)) 
number_boat_bcp_data

## Posterior probability values
number_boat_bcp_prob <- number_boat_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
number_boat_bcp_prob <- lapply(number_boat_bcp_prob$value, as.data.frame)
number_boat_bcp_prob <- number_boat_bcp_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
number_boat_bcp_prob$id <- seq.int(nrow(number_boat_bcp_prob))
number_boat_bcp_prob

### Compile the data and merge with the grouping information 
boat_data <- merge(number_boat_bcp_data, number_boat_bcp_prob, by = c('id','group')) 
boat_data <- boat_data %>% arrange(id) 
boat_data <- merge(boat_data, name_boat, by = "group")
boat_data <- boat_data %>% 
  arrange(State, year)
boat_data

number_boat_year <- number_boat %>% 
  arrange(State, Year) %>% 
  subset(select = c(Year, State))
number_boat_year$id <- seq.int(nrow(number_boat_year))

boat_data <- merge(boat_data, number_boat_year[,c("Year","id")], by = "id")
boat_data <- boat_data %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(boat_data)
data.table::setcolorder(boat_data, c("year","State","values","posterior.prob"))
boat_data

## Create a dummy number of boat data frame to merge the harvest data.
years <- data.frame(Year = c(1900:2025))
states <- data.frame(State = unique(number_boat$State))

## The crossing function creates a long data frame with all possible combinations between the two variables. 
dummy_boat <- crossing(years, states) 
dummy_boat <- dummy_boat %>% rename(year = Year)
boat_data <- merge(boat_data, dummy_boat, by = c("year","State"), all = T)
boat_data <- boat_data %>% mutate_at(vars(values), ~replace_na(., 0)) 
boat_data[boat_data == 0] <- NA
boat_data <- boat_data %>% drop_na(values) 
boat_data

## Create a posterior probability data frame based on the boat data
boat_prob <- boat_data %>% 
  subset(select = -c(values)) %>%
  filter(posterior.prob >= 0.70)
boat_prob

## Amend the NA values to zero, as for plotting as it may reflect two results.
## Either the data is not recorded in fishery reports or that there was no boat in the given year.
rm(years, states, dummy_boat)

## Changes in the number of fishing boats across state and over time.
boat_data %>% 
  ggplot(aes(x=year, y=values))+
  geom_vline(aes(xintercept = year, group = State), data=boat_prob, colour = 'navy', linetype = 'dashed')+
  geom_point(aes(colour=State, group=State), size=1)+
  facet_wrap(.~State, scales='free')+ 
  scale_x_continuous(limits=c(1900,2023), breaks=c(1900,1930,1960,1990,2020), labels=c(1900,1930,1960,1990,2020), name='Year')+
  scale_y_continuous(name='Number of Fishermen')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

######################################################################################################
######################################################################################################

### Track changes in boat type over time ####
### Bayesian Change Point Analysis ####
set.seed(100)

## Remove NAs to prepare the data for the change point analysis
boat_type <- na.omit(boat_type)
boat_type <- boat_type %>% subset(select = -c(power_type))
boat_type <- boat_type %>% 
  mutate(Type = case_when(Type == "Powered" | Type == "Outboard" | Type == "Inboard" ~ "Powered",
                          Type == "Non-powered" ~ "Non-powered",
                          FALSE ~ "Others"))
boat_type <- boat_type %>% 
  group_by(Year, Type, State) %>%
  summarize(Number = sum(Number))
colSums(is.na(boat_type))
boat_type <- boat_type %>%
  group_by(State, Type)

### Assess the probability of a change in mean and posterior mean for each state ####
## For the change point analysis, we will set a posterior of 0.05, burnin of 1000 iterations, and mcmc repetition of 11000 iterations.  
boat_type_bcp <- boat_type %>% 
  group_by(State, Type) %>%
  group_map(~ bcp::bcp(.x$Number, p0 = 0.05, burnin = 1000, mcmc = 11000))
boat_type_bcp 

name_boat_type <- boat_type %>% 
  group_by(State, Type) %>% 
  summarize(list = list(State)) %>% 
  subset(select = c(State, Type))
name_boat_type$group <- 1:nrow(name_boat_type) 

### Compress the nested data into a single compressed data frame
boat_type_bcp <- rrapply(boat_type_bcp, how = 'melt') 

## Extract the mean and posterior mean from the state-specific bcp 
boat_type_bcp_data <- boat_type_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
boat_type_bcp_data <- lapply(boat_type_bcp_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of groups 
boat_type_bcp_data <- boat_type_bcp_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
boat_type_bcp_data$id <- seq.int(nrow(boat_type_bcp_data)) 
boat_type_bcp_data

## Posterior probability values
boat_type_bcp_prob <- boat_type_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
boat_type_bcp_prob <- lapply(boat_type_bcp_prob$value, as.data.frame)
boat_type_bcp_prob <- boat_type_bcp_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
boat_type_bcp_prob$id <- seq.int(nrow(boat_type_bcp_prob))
boat_type_bcp_prob

### Compile the data and merge with the grouping information 
boat_type_data <- merge(boat_type_bcp_data, boat_type_bcp_prob, by = c('id','group')) 
boat_type_data <- boat_type_data %>% arrange(id) 
boat_type_data <- merge(boat_type_data, name_boat_type, by = "group")
boat_type_data <- boat_type_data %>% 
  arrange(State, year)
boat_type_data

boat_type_year <- boat_type %>% 
  arrange(State, Year, Type) %>% 
  subset(select = c(Year, Type, State))
boat_type_year$id <- seq.int(nrow(boat_type_year))

boat_type_data <- boat_type_data %>% 
  subset(select = -c(id)) 
boat_type_data$id <- seq.int(nrow(boat_type_data))

boat_type_data <- merge(boat_type_data, boat_type_year[,c("Year","id")], by = "id")
boat_type_data <- boat_type_data %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(boat_type_data)
data.table::setcolorder(boat_type_data, c("year","State","Type","values","posterior.prob"))
boat_type_data

## Create a dummy number of boat data frame to merge the harvest data.
years <- data.frame(Year = c(1900:2025))
type <- data.frame(Type = unique(boat_type$Type))
states <- data.frame(State = unique(boat_type$State))

## The crossing function creates a long data frame with all possible combinations between the two variables. 
dummy_boat <- crossing(years, type, states) 
dummy_boat <- dummy_boat %>% rename(year = Year)
boat_type_data <- merge(boat_type_data, dummy_boat, by = c("year","Type","State"), all = T)
boat_type_data <- boat_type_data %>% mutate_at(vars(values), ~replace_na(., 0)) 
boat_type_data[boat_type_data == 0] <- NA
boat_type_data <- boat_type_data %>% drop_na(values) 
boat_type_data

## Create a posterior probability data frame based on the boat data
boat_type_prob <- boat_type_data %>% 
  subset(select = -c(values)) %>%
  filter(posterior.prob >= 0.70)
boat_type_prob <- boat_type_prob %>% 
  mutate(State = case_when(State == "Penang and Province Wellesley" ~ "Penang", 
                           T ~ State))
boat_type_prob

## Amend the NA values to zero, as for plotting as it may reflect two results.
## Either the data is not recorded in fishery reports or that there was no boat in the given year.
rm(years, states, dummy_boat)

boat_type_prob %>%
  filter(Type == "Powered" & posterior.prob > "0.70") %>% 
  arrange(year)

## Changes in the number of fishermen across state and over time.
plot3 <- boat_type_data %>% 
  mutate(State = case_when(State == "Johor West" ~ "Johore West", 
                           T ~ State)) %>%
  mutate(State = case_when(State == "Penang and Province Wellesley" ~ "Penang and Province\nWellesley", 
                           T ~ State)) %>%
  filter(State != "Penang") %>%
  ggplot(aes(x=year, y=values))+
  geom_point(aes(group=Type, colour=Type, shape=Type), size=1)+
  facet_wrap(.~State, scales='free', ncol = 4)+
  scale_x_continuous(limits=c(1920,2023), breaks=c(1930,1960,1990,2020), labels=c(1930,1960,1990,2020), name='Year')+
  scale_y_continuous(name='Number of Fishing Vessels (By Type)')+
  scale_shape_manual(values=c(19, 17))+
  scale_color_manual(values=c('#F8766D', '#619CFF'))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot3

######################################################################################################
######################################################################################################

### Changes in oil prices over time #### 
oil_prices <- oil_prices %>% 
  rename(year = Date, price = Price.in.Contemporary.Dollars, adj_price = Price.in.2021.Dollars)
colSums(is.na(oil_prices))
oil_prices$year <- as.Date(oil_prices$year)
oil_prices$year <- format(as.Date(oil_prices$year), "%Y")
oil_prices

oil_prices_bcp <- oil_prices %>% 
  group_map(~ bcp::bcp(.x$adj_price, p0 = 0.05, burnin = 1000, mcmc = 11000))
oil_prices_bcp 

### Compress the nested data into a single compressed data frame
oil_prices_bcp <- rrapply(oil_prices_bcp, how = 'melt') 

## Extract the mean and posterior mean from the state-specific bcp 
oil_prices_bcp_data <- oil_prices_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
oil_prices_bcp_data <- lapply(oil_prices_bcp_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of groups 
oil_prices_bcp_data <- oil_prices_bcp_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
oil_prices_bcp_data$id <- seq.int(nrow(oil_prices_bcp_data)) 
oil_prices_bcp_data

## Posterior probability values
oil_prices_bcp_prob <- oil_prices_bcp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
oil_prices_bcp_prob <- lapply(oil_prices_bcp_prob$value, as.data.frame)
oil_prices_bcp_prob <- oil_prices_bcp_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
oil_prices_bcp_prob$id <- seq.int(nrow(oil_prices_bcp_prob))
oil_prices_bcp_prob

### Compile the data and merge with the grouping information 
oil_prices_data <- merge(oil_prices_bcp_data, oil_prices_bcp_prob, by = c('id','group')) 
oil_prices_data <- oil_prices_data %>% arrange(id) 

name_oil <- oil_prices %>% 
  group_by(year) %>% 
  subset(select = year)
name_oil$id <- 1:nrow(name_oil) 

oil_prices_data <- merge(oil_prices_data, name_oil, by = "id")
oil_prices_data <- oil_prices_data %>% 
  subset(select = -c(id, group, year.x)) %>%
  rename(year = year.y) %>% 
  arrange(year, values, posterior.prob) 
data.table::setcolorder(oil_prices_data, c("year","values","posterior.prob"))
oil_prices_data 

## Create a posterior probability data frame based on the boat data
oil_prob <- oil_prices_data %>% 
  subset(select = -c(values)) %>%
  filter(posterior.prob >= 0.70)
oil_prob

## Changes in the number of fishermen across state and over time.
oil_prices_data$year <- as.numeric(oil_prices_data$year)
oil_prob$year <- as.numeric(oil_prob$year)

oil_prices_data %>% 
  ggplot(aes(x=year, y=values))+
  geom_point(size=1)+
  #geom_vline(aes(xintercept = year), data=oil_prob, colour = 'navy', linetype = 'dashed')+
  scale_x_continuous(name='Year', limits=c(1860,2021), breaks=c(1860,1900,1940,1980,2020), labels=c(1860,1900,1940,1980,2020))+
  scale_y_continuous(name='Adjusted Oil Prices (USD)', breaks=c(0,25,50,75,100,125), labels=c(0,25,50,75,100,125))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

######################################################################################################
######################################################################################################

### Overall patterns and trends in the fisheries data ####
## Societal variables 
overall_catch <- overall_catch %>% 
  subset(select = -c(rolling)) %>% 
  rename(year = Year)

overall_fishermen <- fishermen_data %>% 
  group_by(year) %>%
  summarize(values = sum(values)) %>%
  rename(fishermen = values)

overall_fishermen %>%
  ggplot(aes(x=year, y=fishermen))+
  geom_point()+ 
  theme_bw()

overall_boats <- boat_type_data %>%
  group_by(year, Type) %>%
  summarize(values = sum(values)) %>%
  rename(boats = values) %>%
  pivot_wider(names_from = Type, values_from = boats)

overall_boats %>%
  ggplot(aes(x=year))+
  geom_point(aes(y=`Non-powered`, colour=`Non-powered`), colour = "red", data = overall_boats)+
  geom_point(aes(y=Powered, colour=Powered), colour = "blue", data = overall_boats)+
  theme_bw()

overall_prices <- oil_prices_data %>% 
  subset(select = -c(posterior.prob)) %>%
  rename(prices = values)

overall_list = list(overall_catch,overall_fishermen,overall_boats,overall_prices)
overall_data <- overall_list %>% reduce(inner_join, by='year')
overall_years <- overall_data %>% 
  subset(select = "year")
overall_data <- overall_data %>%
  subset(select = c(2:6))

### Assess for stationarity
overall_stationarity <- overall_data %>% drop_na(1) %>% pull(1) %>% diff() 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## p value of <0.05 based on the KPSS test indicates that the time-series is non stationary. 

overall_stationarity <- overall_data %>% drop_na(1) %>% pull(1) %>% diff(differences = 2) 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

overall_stationarity <- overall_data %>% drop_na(2) %>% pull(2) %>% diff() 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## p value of >0.05 based on the ADF test indicates that the time-series is non stationary. 

overall_stationarity <- overall_data %>% drop_na(2) %>% pull(2) %>% diff(differences = 2) 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

overall_stationarity <- overall_data %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## p value of >0.05 based on the ADF test indicates that the time-series is non stationary. 

overall_stationarity <- overall_data %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

overall_stationarity <- overall_data %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## p value of >0.05 based on the ADF test indicates that the time-series is non stationary. 

overall_stationarity <- overall_data %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

overall_stationarity <- overall_data %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## p value of >0.05 based on the ADF test indicates that the time-series is non stationary. 

overall_stationarity <- overall_data %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(overall_stationarity) ; tseries::kpss.test(overall_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Establish lag for each variable 
VARselect(overall_data, type = c("const", "trend", "both", "none"), lag.max = 10)
overall_data <- as.data.frame(lapply(overall_data, diff, differences = 2))

overall_ts <- VAR(overall_data, p = 10)
bruceR::granger_causality(overall_ts) 

######################################################################################################
######################################################################################################

### State specific harvests for each fish group ####
## Revert the zeros to NAs and remove them for the change point analysis. 
state_catch[state_catch == 0] <- NA 
state_catch <- na.omit(state_catch)

state_catch <- state_catch %>% mutate_if(is.character, str_trim)
state_catch <- state_catch %>% 
  mutate(Period = case_when(Year < 1940 ~ "Pre",
                            Year > 1940 ~ "Post", 
                            F ~ ""))
state_catch$Harvest <- round(state_catch$Harvest, 8)
state_catch$var <- paste(state_catch$State, state_catch$Grouping_Name, sep = "_")
data.table::setcolorder(state_catch, c("Year","Period","State","Grouping_Name","var","Harvest"))
state_catch <- as.data.frame(state_catch)

## Filter the data for when there are at least three years of data. 
state_catch <- state_catch %>% 
  group_by(var) %>% 
  filter(n() > 3) %>% 
  ungroup()

### Bayesian Change Point Analysis ####
## Assess the probability of a change in mean and posterior mean for each fish group ##
## For the change point analysis, we will set a posterior of 0.05, burnin of 1000 iterations, and mcmc repetition of 11000 iterations. 
## The analysis is repeated for each state as the list is too big for R to process at one go. 
unique(state_catch$State)

### State-specific Bayesian Change Point Analysis ####
bcp_kd <- state_catch %>% 
  filter(State == "Kedah") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_kl <- state_catch %>% 
  filter(State == "Kelantan") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_ml <- state_catch %>% 
  filter(State == "Malacca") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_ns <- state_catch %>% 
  filter(State == "Negri Sembilan") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_pa <- state_catch %>% 
  filter(State == "Pahang") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_pp <- state_catch %>% 
  filter(State == "Penang and Province Wellesley") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_pr <- state_catch %>% 
  filter(State == "Perak") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_sl <- state_catch %>% 
  filter(State == "Selangor") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_sg <- state_catch %>% 
  filter(State == "Singapore") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_tr <- state_catch %>% 
  filter(State == "Terengganu") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_la <- state_catch %>% 
  filter(State == "Labuan") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_je <- state_catch %>% 
  filter(State == "Johore East") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_pe <- state_catch %>% 
  filter(State == "Perlis") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_jw <- state_catch %>% 
  filter(State == "Johor West") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_sb <- state_catch %>% 
  filter(State == "Sabah") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))

bcp_sr <- state_catch %>% 
  filter(State == "Sarawak") %>%
  group_by(var) %>% 
  group_map(~ bcp::bcp(.x$Harvest, p0 = 0.05, burnin = 1000, mcmc = 11000))
## There are no issues with the formatting and running of the data. We will proceed with data processing.

######################################################################################################
######################################################################################################

### State-specific fish groups ####
## Create a list of variables for the assignment of names.
## There were some issues assigning names for nested lists, so I bypassed this using the merge function. 

name_kd <- state_catch %>% 
  filter(State == "Kedah") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>% 
  subset(select = var)
name_kd$group <- 1:nrow(name_kd) 

name_kl <- state_catch %>% 
  filter(State == "Kelantan") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_kl$group <- 1:nrow(name_kl) 

name_ml <- state_catch %>% 
  filter(State == "Malacca") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_ml$group <- 1:nrow(name_ml) 

name_ns <- state_catch %>% 
  filter(State == "Negri Sembilan") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_ns$group <- 1:nrow(name_ns) 

name_pa <- state_catch %>% 
  filter(State == "Pahang") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_pa$group <- 1:nrow(name_pa) 

name_pp <- state_catch %>% 
  filter(State == "Penang and Province Wellesley") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_pp$group <- 1:nrow(name_pp) 

name_pr <- state_catch %>% 
  filter(State == "Perak") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_pr$group <- 1:nrow(name_pr) 

name_sl <- state_catch %>% 
  filter(State == "Selangor") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_sl$group <- 1:nrow(name_sl) 

name_sg <- state_catch %>% 
  filter(State == "Singapore") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_sg$group <- 1:nrow(name_sg) 

name_tr <- state_catch %>% 
  filter(State == "Terengganu") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_tr$group <- 1:nrow(name_tr) 

name_la <- state_catch %>% 
  filter(State == "Labuan") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_la$group <- 1:nrow(name_la) 

name_je <- state_catch %>% 
  filter(State == "Johore East") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_je$group <- 1:nrow(name_je) 

name_pe <- state_catch %>% 
  filter(State == "Perlis") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_pe$group <- 1:nrow(name_pe) 

name_jw <- state_catch %>% 
  filter(State == "Johor West") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_jw$group <- 1:nrow(name_jw) 

name_sb <- state_catch %>% 
  filter(State == "Sabah") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_sb$group <- 1:nrow(name_sb) 

name_sr <- state_catch %>% 
  filter(State == "Sarawak") %>%
  group_by(var) %>% 
  summarize(list = list(var)) %>%
  subset(select = var)
name_sr$group <- 1:nrow(name_sr) 

### Compress the nested data into a single compressed data frame
bcp_kd <- rrapply(bcp_kd, how = 'melt') 
bcp_kl <- rrapply(bcp_kl, how = 'melt') 
bcp_ml <- rrapply(bcp_ml, how = 'melt') 
bcp_ns <- rrapply(bcp_ns, how = 'melt') 
bcp_pa <- rrapply(bcp_pa, how = 'melt') 
bcp_pp <- rrapply(bcp_pp, how = 'melt') 
bcp_pr <- rrapply(bcp_pr, how = 'melt') 
bcp_sl <- rrapply(bcp_sl, how = 'melt') 
bcp_sg <- rrapply(bcp_sg, how = 'melt') 
bcp_tr <- rrapply(bcp_tr, how = 'melt') 
bcp_la <- rrapply(bcp_la, how = 'melt') 
bcp_je <- rrapply(bcp_je, how = 'melt') 
bcp_pe <- rrapply(bcp_pe, how = 'melt') 
bcp_jw <- rrapply(bcp_jw, how = 'melt') 
bcp_sb <- rrapply(bcp_sb, how = 'melt') 
bcp_sr <- rrapply(bcp_sr, how = 'melt') 

######################################################################################################
######################################################################################################

### Kedah 
## Extract the mean and posterior mean from the state-specific bcp 
bcp_kd_data <- bcp_kd %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_kd_data <- lapply(bcp_kd_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_kd_data <- bcp_kd_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_kd_data$id <- seq.int(nrow(bcp_kd_data)) 
bcp_kd_data

## Posterior probability values
bcp_kd_prob <- bcp_kd %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_kd_prob <- lapply(bcp_kd_prob$value, as.data.frame)
bcp_kd_prob <- bcp_kd_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_kd_prob$id <- seq.int(nrow(bcp_kd_prob))
bcp_kd_prob

### Compile the data and merge with the grouping information 
data_kd <- merge(bcp_kd_data, bcp_kd_prob, by = c('id','group')) 
data_kd <- data_kd %>% arrange(id) 
data_kd <- merge(data_kd, name_kd, by = "group")
data_kd 

year_kd <- state_catch %>% 
  filter(State == "Kedah") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_kd$id <- seq.int(nrow(year_kd))

data_kd <- merge(data_kd, year_kd[,c("Year","id")], by = "id")
data_kd <- data_kd %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_kd)
data.table::setcolorder(data_kd, c("year","var","values","posterior.prob"))
data_kd

### Kelantan 
## Extract the mean and posterior mean from the state-specific bcp 
bcp_kl_data <- bcp_kl %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_kl_data <- lapply(bcp_kl_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_kl_data <- bcp_kl_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_kl_data$id <- seq.int(nrow(bcp_kl_data)) 
bcp_kl_data

## Posterior probability values
bcp_kl_prob <- bcp_kl %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_kl_prob <- lapply(bcp_kl_prob$value, as.data.frame)
bcp_kl_prob <- bcp_kl_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_kl_prob$id <- seq.int(nrow(bcp_kl_prob))
bcp_kl_prob

### Compile the data and merge with the grouping information 
data_kl <- merge(bcp_kl_data, bcp_kl_prob, by = c('id','group')) 
data_kl <- data_kl %>% arrange(id) 
data_kl <- merge(data_kl, name_kl, by = "group")
data_kl

year_kl <- state_catch %>% 
  filter(State == "Kelantan") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_kl$id <- seq.int(nrow(year_kl))

data_kl <- merge(data_kl, year_kl[,c("Year","id")], by = "id")
data_kl <- data_kl %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_kl)
data.table::setcolorder(data_kl, c("year","var","values","posterior.prob"))
data_kl

### Malacca 
## Extract the mean and posterior mean from the state-specific bcp 
bcp_ml_data <- bcp_ml %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_ml_data <- lapply(bcp_ml_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_ml_data <- bcp_ml_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_ml_data$id <- seq.int(nrow(bcp_ml_data)) 
bcp_ml_data

## Posterior probability values
bcp_ml_prob <- bcp_ml %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_ml_prob <- lapply(bcp_ml_prob$value, as.data.frame)
bcp_ml_prob <- bcp_ml_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_ml_prob$id <- seq.int(nrow(bcp_ml_prob))
bcp_ml_prob

### Compile the data and merge with the grouping information 
data_ml <- merge(bcp_ml_data, bcp_ml_prob, by = c('id','group')) 
data_ml <- data_ml %>% arrange(id) 
data_ml <- merge(data_ml, name_ml, by = "group")
data_ml

year_ml <- state_catch %>% 
  filter(State == "Malacca") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_ml$id <- seq.int(nrow(year_ml))

data_ml <- merge(data_ml, year_ml[,c("Year","id")], by = "id")
data_ml <- data_ml %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_ml)
data.table::setcolorder(data_ml, c("year","var","values","posterior.prob"))
data_ml

### Negri Sembilan 
## Extract the mean and posterior mean from the state-specific bcp 
bcp_ns_data <- bcp_ns %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_ns_data <- lapply(bcp_ns_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_ns_data <- bcp_ns_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_ns_data$id <- seq.int(nrow(bcp_ns_data)) 
bcp_ns_data

## Posterior probability values
bcp_ns_prob <- bcp_ns %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_ns_prob <- lapply(bcp_ns_prob$value, as.data.frame)
bcp_ns_prob <- bcp_ns_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_ns_prob$id <- seq.int(nrow(bcp_ns_prob))
bcp_ns_prob

### Compile the data and merge with the grouping information 
data_ns <- merge(bcp_ns_data, bcp_ns_prob, by = c('id','group')) 
data_ns <- data_ns %>% arrange(id) 
data_ns <- merge(data_ns, name_ns, by = "group")
data_ns

year_ns <- state_catch %>% 
  filter(State == "Negri Sembilan") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_ns$id <- seq.int(nrow(year_ns))

data_ns <- merge(data_ns, year_ns[,c("Year","id")], by = "id")
data_ns <- data_ns %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_ns)
data.table::setcolorder(data_ns, c("year","var","values","posterior.prob"))
data_ns

### Pahang
## Extract the mean and posterior mean from the state-specific bcp 
bcp_pa_data <- bcp_pa %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_pa_data <- lapply(bcp_pa_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_pa_data <- bcp_pa_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_pa_data$id <- seq.int(nrow(bcp_pa_data)) 
bcp_pa_data

## Posterior probability values
bcp_pa_prob <- bcp_pa %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_pa_prob <- lapply(bcp_pa_prob$value, as.data.frame)
bcp_pa_prob <- bcp_pa_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_pa_prob$id <- seq.int(nrow(bcp_pa_prob))
bcp_pa_prob

### Compile the data and merge with the grouping information 
data_pa <- merge(bcp_pa_data, bcp_pa_prob, by = c('id','group')) 
data_pa <- data_pa %>% arrange(id) 
data_pa <- merge(data_pa, name_pa, by = "group")
data_pa

year_pa <- state_catch %>% 
  filter(State == "Pahang") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_pa$id <- seq.int(nrow(year_pa))

data_pa <- merge(data_pa, year_pa[,c("Year","id")], by = "id")
data_pa <- data_pa %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_pa)
data.table::setcolorder(data_pa, c("year","var","values","posterior.prob"))
data_pa

### Penang and Province Wellesley
## Extract the mean and posterior mean from the state-specific bcp 
bcp_pp_data <- bcp_pp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_pp_data <- lapply(bcp_pp_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_pp_data <- bcp_pp_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_pp_data$id <- seq.int(nrow(bcp_pp_data)) 
bcp_pp_data

## Posterior probability values
bcp_pp_prob <- bcp_pp %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_pp_prob <- lapply(bcp_pp_prob$value, as.data.frame)
bcp_pp_prob <- bcp_pp_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_pp_prob$id <- seq.int(nrow(bcp_pp_prob))
bcp_pp_prob

### Compile the data and merge with the grouping information 
data_pp <- merge(bcp_pp_data, bcp_pp_prob, by = c('id','group')) 
data_pp <- data_pp %>% arrange(id) 
data_pp <- merge(data_pp, name_pp, by = "group")
data_pp

year_pp <- state_catch %>% 
  filter(State == "Penang and Province Wellesley") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_pp$id <- seq.int(nrow(year_pp))

data_pp <- merge(data_pp, year_pp[,c("Year","id")], by = "id")
data_pp <- data_pp %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_pp)
data.table::setcolorder(data_pp, c("year","var","values","posterior.prob"))
data_pp

### Perak
## Extract the mean and posterior mean from the state-specific bcp 
bcp_pr_data <- bcp_pr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_pr_data <- lapply(bcp_pr_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_pr_data <- bcp_pr_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_pr_data$id <- seq.int(nrow(bcp_pr_data)) 
bcp_pr_data

## Posterior probability values
bcp_pr_prob <- bcp_pr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_pr_prob <- lapply(bcp_pr_prob$value, as.data.frame)
bcp_pr_prob <- bcp_pr_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_pr_prob$id <- seq.int(nrow(bcp_pr_prob))
bcp_pr_prob

### Compile the data and merge with the grouping information 
data_pr <- merge(bcp_pr_data, bcp_pr_prob, by = c('id','group')) 
data_pr <- data_pr %>% arrange(id) 
data_pr <- merge(data_pr, name_pr, by = "group")
data_pr

year_pr <- state_catch %>% 
  filter(State == "Perak") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_pr$id <- seq.int(nrow(year_pr))

data_pr <- merge(data_pr, year_pr[,c("Year","id")], by = "id")
data_pr <- data_pr %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_pr)
data.table::setcolorder(data_pr, c("year","var","values","posterior.prob"))
data_pr

### Selangor
## Extract the mean and posterior mean from the state-specific bcp 
bcp_sl_data <- bcp_sl %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_sl_data <- lapply(bcp_sl_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_sl_data <- bcp_sl_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_sl_data$id <- seq.int(nrow(bcp_sl_data)) 
bcp_sl_data

## Posterior probability values
bcp_sl_prob <- bcp_sl %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_sl_prob <- lapply(bcp_sl_prob$value, as.data.frame)
bcp_sl_prob <- bcp_sl_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_sl_prob$id <- seq.int(nrow(bcp_sl_prob))
bcp_sl_prob

### Compile the data and merge with the grouping information 
data_sl <- merge(bcp_sl_data, bcp_sl_prob, by = c('id','group')) 
data_sl <- data_sl %>% arrange(id) 
data_sl <- merge(data_sl, name_sl, by = "group")
data_sl

year_sl <- state_catch %>% 
  filter(State == "Selangor") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_sl$id <- seq.int(nrow(year_sl))

data_sl <- merge(data_sl, year_sl[,c("Year","id")], by = "id")
data_sl <- data_sl %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_sl)
data.table::setcolorder(data_sl, c("year","var","values","posterior.prob"))
data_sl

### Singapore
## Extract the mean and posterior mean from the state-specific bcp 
bcp_sg_data <- bcp_sg %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_sg_data <- lapply(bcp_sg_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_sg_data <- bcp_sg_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_sg_data$id <- seq.int(nrow(bcp_sg_data)) 
bcp_sg_data

## Posterior probability values
bcp_sg_prob <- bcp_sg %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_sg_prob <- lapply(bcp_sg_prob$value, as.data.frame)
bcp_sg_prob <- bcp_sg_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_sg_prob$id <- seq.int(nrow(bcp_sg_prob))
bcp_sg_prob

### Compile the data and merge with the grouping information 
data_sg <- merge(bcp_sg_data, bcp_sg_prob, by = c('id','group')) 
data_sg <- data_sg %>% arrange(id) 
data_sg <- merge(data_sg, name_sg, by = "group")
data_sg

year_sg <- state_catch %>% 
  filter(State == "Singapore") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_sg$id <- seq.int(nrow(year_sg))

data_sg <- merge(data_sg, year_sg[,c("Year","id")], by = "id")
data_sg <- data_sg %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_sg)
data.table::setcolorder(data_sg, c("year","var","values","posterior.prob"))
data_sg

### Terengganu
## Extract the mean and posterior mean from the state-specific bcp 
bcp_tr_data <- bcp_tr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_tr_data <- lapply(bcp_tr_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_tr_data <- bcp_tr_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_tr_data$id <- seq.int(nrow(bcp_tr_data)) 
bcp_tr_data

## Posterior probability values
bcp_tr_prob <- bcp_tr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_tr_prob <- lapply(bcp_tr_prob$value, as.data.frame)
bcp_tr_prob <- bcp_tr_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_tr_prob$id <- seq.int(nrow(bcp_tr_prob))
bcp_tr_prob

### Compile the data and merge with the grouping information 
data_tr <- merge(bcp_tr_data, bcp_tr_prob, by = c('id','group')) 
data_tr <- data_tr %>% arrange(id) 
data_tr <- merge(data_tr, name_tr, by = "group")
data_tr

year_tr <- state_catch %>% 
  filter(State == "Terengganu") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_tr$id <- seq.int(nrow(year_tr))

data_tr <- merge(data_tr, year_tr[,c("Year","id")], by = "id")
data_tr <- data_tr %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_tr)
data.table::setcolorder(data_tr, c("year","var","values","posterior.prob"))
data_tr

### Labuan
## Extract the mean and posterior mean from the state-specific bcp 
bcp_la_data <- bcp_la %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_la_data <- lapply(bcp_la_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_la_data <- bcp_la_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_la_data$id <- seq.int(nrow(bcp_la_data)) 
bcp_la_data

## Posterior probability values
bcp_la_prob <- bcp_la %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_la_prob <- lapply(bcp_la_prob$value, as.data.frame)
bcp_la_prob <- bcp_la_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_la_prob$id <- seq.int(nrow(bcp_la_prob))
bcp_la_prob

### Compile the data and merge with the grouping information 
data_la <- merge(bcp_la_data, bcp_la_prob, by = c('id','group')) 
data_la <- data_la %>% arrange(id) 
data_la <- merge(data_la, name_la, by = "group")
data_la

year_la <- state_catch %>% 
  filter(State == "Labuan") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_la$id <- seq.int(nrow(year_la))

data_la <- merge(data_la, year_la[,c("Year","id")], by = "id")
data_la <- data_la %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_la)
data.table::setcolorder(data_la, c("year","var","values","posterior.prob"))
data_la

### Johore East
## Extract the mean and posterior mean from the state-specific bcp 
bcp_je_data <- bcp_je %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_je_data <- lapply(bcp_je_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_je_data <- bcp_je_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_je_data$id <- seq.int(nrow(bcp_je_data)) 
bcp_je_data

## Posterior probability values
bcp_je_prob <- bcp_je %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_je_prob <- lapply(bcp_je_prob$value, as.data.frame)
bcp_je_prob <- bcp_je_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_je_prob$id <- seq.int(nrow(bcp_je_prob))
bcp_je_prob

### Compile the data and merge with the grouping information 
data_je <- merge(bcp_je_data, bcp_je_prob, by = c('id','group')) 
data_je <- data_je %>% arrange(id) 
data_je <- merge(data_je, name_je, by = "group")
data_je

year_je <- state_catch %>% 
  filter(State == "Johore East") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_je$id <- seq.int(nrow(year_je))

data_je <- merge(data_je, year_je[,c("Year","id")], by = "id")
data_je <- data_je %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_je)
data.table::setcolorder(data_je, c("year","var","values","posterior.prob"))
data_je

### Perlis
## Extract the mean and posterior mean from the state-specific bcp 
bcp_pe_data <- bcp_pe %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_pe_data <- lapply(bcp_pe_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_pe_data <- bcp_pe_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_pe_data$id <- seq.int(nrow(bcp_pe_data)) 
bcp_pe_data

## Posterior probability values
bcp_pe_prob <- bcp_pe %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_pe_prob <- lapply(bcp_pe_prob$value, as.data.frame)
bcp_pe_prob <- bcp_pe_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_pe_prob$id <- seq.int(nrow(bcp_pe_prob))
bcp_pe_prob

### Compile the data and merge with the grouping information 
data_pe <- merge(bcp_pe_data, bcp_pe_prob, by = c('id','group')) 
data_pe <- data_pe %>% arrange(id) 
data_pe <- merge(data_pe, name_pe, by = "group")
data_pe

year_pe <- state_catch %>% 
  filter(State == "Perlis") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_pe$id <- seq.int(nrow(year_pe))

data_pe <- merge(data_pe, year_pe[,c("Year","id")], by = "id")
data_pe <- data_pe %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_pe)
data.table::setcolorder(data_pe, c("year","var","values","posterior.prob"))
data_pe

### Johor West
## Extract the mean and posterior mean from the state-specific bcp 
bcp_jw_data <- bcp_jw %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_jw_data <- lapply(bcp_jw_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_jw_data <- bcp_jw_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_jw_data$id <- seq.int(nrow(bcp_jw_data)) 
bcp_jw_data

## Posterior probability values
bcp_jw_prob <- bcp_jw %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_jw_prob <- lapply(bcp_jw_prob$value, as.data.frame)
bcp_jw_prob <- bcp_jw_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_jw_prob$id <- seq.int(nrow(bcp_jw_prob))
bcp_jw_prob

### Compile the data and merge with the grouping information 
data_jw <- merge(bcp_jw_data, bcp_jw_prob, by = c('id','group')) 
data_jw <- data_jw %>% arrange(id) 
data_jw <- merge(data_jw, name_jw, by = "group")
data_jw

year_jw <- state_catch %>% 
  filter(State == "Johor West") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_jw$id <- seq.int(nrow(year_jw))

data_jw <- merge(data_jw, year_jw[,c("Year","id")], by = "id")
data_jw <- data_jw %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_jw)
data.table::setcolorder(data_jw, c("year","var","values","posterior.prob"))
data_jw

### Sabah
## Extract the mean and posterior mean from the state-specific bcp 
bcp_sb_data <- bcp_sb %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_sb_data <- lapply(bcp_sb_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_sb_data <- bcp_sb_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_sb_data$id <- seq.int(nrow(bcp_sb_data)) 
bcp_sb_data

## Posterior probability values
bcp_sb_prob <- bcp_sb %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_sb_prob <- lapply(bcp_sb_prob$value, as.data.frame)
bcp_sb_prob <- bcp_sb_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_sb_prob$id <- seq.int(nrow(bcp_sb_prob))
bcp_sb_prob

### Compile the data and merge with the grouping information 
data_sb <- merge(bcp_sb_data, bcp_sb_prob, by = c('id','group')) 
data_sb <- data_sb %>% arrange(id) 
data_sb <- merge(data_sb, name_sb, by = "group")
data_sb

year_sb <- state_catch %>% 
  filter(State == "Sabah") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_sb$id <- seq.int(nrow(year_sb))

data_sb <- merge(data_sb, year_sb[,c("Year","id")], by = "id")
data_sb <- data_sb %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_sb)
data.table::setcolorder(data_sb, c("year","var","values","posterior.prob"))
data_sb

### Sarawak
## Extract the mean and posterior mean from the state-specific bcp 
bcp_sr_data <- bcp_sr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "data"))
bcp_sr_data <- lapply(bcp_sr_data$value, as.data.frame)

## Convert to a single data frame and group the rows by their id
## Create an id variable for the alignment of fish groups 
bcp_sr_data <- bcp_sr_data %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(year = V1, values = V2) 
bcp_sr_data$id <- seq.int(nrow(bcp_sr_data)) 
bcp_sr_data

## Posterior probability values
bcp_sr_prob <- bcp_sr %>% 
  rename(id = L1, type = L2) %>% 
  subset((type == "posterior.prob"))
bcp_sr_prob <- lapply(bcp_sr_prob$value, as.data.frame)
bcp_sr_prob <- bcp_sr_prob %>% 
  purrr::map(as.data.frame) %>% 
  dplyr::bind_rows(.id = "group") %>% 
  rename(posterior.prob = `X[[i]]`)
bcp_sr_prob$id <- seq.int(nrow(bcp_sr_prob))
bcp_sr_prob

### Compile the data and merge with the grouping information 
data_sr <- merge(bcp_sr_data, bcp_sr_prob, by = c('id','group')) 
data_sr <- data_sr %>% arrange(id) 
data_sr <- merge(data_sr, name_sr, by = "group")
data_sr

year_sr <- state_catch %>% 
  filter(State == "Sarawak") %>%
  arrange(var, Year) %>% 
  subset(select = c(Year, var))
year_sr$id <- seq.int(nrow(year_sr))

data_sr <- merge(data_sr, year_sr[,c("Year","id")], by = "id")
data_sr <- data_sr %>% 
  subset(select = -c(year,id,group)) %>% 
  rename(year = Year)

colnames(data_sr)
data.table::setcolorder(data_sr, c("year","var","values","posterior.prob"))
data_sr

######################################################################################################
######################################################################################################

### Create new columns based on the var term ####
data_kd <- data_kd %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_kl <- data_kl %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_ml <- data_ml %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_ns <- data_ns %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_pa <- data_pa %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_pp <- data_pp %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_pr <- data_pr %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_sl <- data_sl %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_sg <- data_sg %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_tr <- data_tr %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_la <- data_la %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_je <- data_je %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_pe <- data_pe %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_jw <- data_jw %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_sb <- data_sb %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)
data_sr <- data_sr %>% separate(var, c("State", "Grouping Name"), "_", remove = FALSE)

catch_data <- do.call("rbind", list(data_kd,data_kl,data_ml,data_ns,data_pa,data_pp,data_pr,data_sl,data_sg,data_tr,data_la,data_je,data_pe,data_jw,data_sb,data_sr))
catch_data <- catch_data %>% 
  arrange(`Grouping Name`, State, year) %>%
  rename(Grouping_Name = `Grouping Name`)
catch_data

## Create a dummy catch data frame to merge the harvest data.
years <- data.frame(Year = c(1930:2025))
states <- data.frame(State = unique(fishery_catch$State))
groups <- data.frame(Grouping_Name = unique(fishery_catch$Grouping_Name)) 

## The crossing function creates a long data frame with all possible combinations between the three variables. 
dummy_catch <- crossing(years, states, groups) 
dummy_catch <- dummy_catch %>% rename(year = Year)
catch_data <- merge(catch_data, dummy_catch, by = c("year","State","Grouping_Name"), all = T)
catch_data <- catch_data %>% mutate_at(vars(values), ~replace_na(., 0)) 
catch_data <- catch_data %>% drop_na(var) 
catch_data

## Create a posterior probability data frame based on the catch data
catch_prob <- catch_data %>% 
  subset(select = -c(values)) %>%
  filter(posterior.prob >= 0.70)

## Amend the NA values to zero, as for plotting as it may reflect two results.
## Either the data is not recorded in fishery reports or that there was no catch in the given year.
rm(years, states, groups, dummy_catch)

unique(catch_data$Grouping_Name)

## Case examples for specific species across space and time
catch_data %>% 
  #filter(Grouping_Name == "Lutjanidae") %>% 
  ggplot(aes(x=year, y=values))+
  #geom_vline(aes(xintercept = year), data=catch_prob %>% filter(Grouping_Name == "Lutjanidae"), colour = 'navy', linetype = 'dashed')+
  geom_point(aes(colour=State, group=State), size=1.5)+
  facet_wrap(.~State, scales='free')+ 
  scale_x_continuous(limits=c(1930,2023), breaks=c(1930,1950,1970,1990,2010), labels=c(1930,1950,1970,1990,2010), name='Year')+
  scale_y_continuous(name='Harvest (thousand tons)')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        legend.position='none',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

######################################################################################################
######################################################################################################

### Assessing cause and effect in time-series data ####
### Granger-Causality Test ###
## Granger causality is a statistical concept used to determine whether one time series can predict the future values of another time series. 
## However, a pre-requisite to the Granger-Causality test is that it assumes X and Y are stationary time-series. 
## The Augmented Dickey Fuller Test and KPSS tests are commonly used to check if there is stationarity in the data. 

### State specific changes in fishery data over time 
## Arrange the data in the correct format for analysis
df1 <- catch_data %>% 
  group_by(State, year) %>% 
  summarise(values = sum(values)) %>% 
  arrange(year) %>% 
  rename(catch = values) %>% 
  mutate(State = case_when(State == "Penang and Province Wellesley" ~ "Penang", 
                           State == "Johor West" ~ "Johore West", 
                           TRUE ~ State))
data.table::setcolorder(df1, c("year","State","catch"))

df2 <- fishermen_data %>% 
  group_by(State, year) %>% 
  summarise(values = sum(values)) %>% 
  arrange(year) %>% 
  rename(fishermen = values) 
data.table::setcolorder(df2, c("year","State","fishermen"))

df3 <- boat_type_data %>% 
  group_by(State, year, Type) %>% 
  summarise(values = sum(values)) %>% 
  pivot_wider(names_from = Type, values_from = values) %>% 
  arrange(year) %>% 
  mutate(State = case_when(State == "Penang and Province Wellesley" ~ "Penang", 
                           State == "Johor West" ~ "Johore West", 
                           TRUE ~ State))
data.table::setcolorder(df3, c("year","State"))

df4 <- oil_prices_data %>% 
  subset(select = -c(posterior.prob)) %>% 
  arrange(year) %>% 
  rename(oil_prices = values)

## Align all data frames to df2, which contains the longest temporal range. 
min(df2$year)

compiled_data <- df2 %>% 
  left_join(df3, by = c('year', 'State')) %>% 
  left_join(df1, by = c('year', 'State')) %>% 
  left_join(df4, by = c("year"))
compiled_data

rm(df1, df2, df3, df4)

######################################################################################################
######################################################################################################

### Data preparation to assess the state-specific trends ####
unique(compiled_data$State)

df_kd <- compiled_data %>% filter(State == "Kedah") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_kl <- compiled_data %>% filter(State == "Kelantan") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_ml <- compiled_data %>% filter(State == "Malacca") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_ns <- compiled_data %>% filter(State == "Negri Sembilan") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_pa <- compiled_data %>% filter(State == "Pahang") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_pp <- compiled_data %>% filter(State == "Penang") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_pr <- compiled_data %>% filter(State == "Perak") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_sl <- compiled_data %>% filter(State == "Selangor") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_sg <- compiled_data %>% filter(State == "Singapore") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_tr <- compiled_data %>% filter(State == "Terengganu") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_la <- compiled_data %>% filter(State == "Labuan") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_je <- compiled_data %>% filter(State == "Johore East") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_pe <- compiled_data %>% filter(State == "Perlis") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_jw <- compiled_data %>% filter(State == "Johore West") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_sb <- compiled_data %>% filter(State == "Sabah") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 
df_sr <- compiled_data %>% filter(State == "Sarawak") %>% mutate(`Non-powered` = ifelse(is.na(`Non-powered`), 0, `Non-powered`)) 

### Check for stationarity in the data ####
## Kedah
### Number of fishermen 
kd_stationarity <- df_kd %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

kd_stationarity <- df_kd %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
kd_stationarity <- df_kd %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
kd_stationarity <- df_kd %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

kd_stationarity <- df_kd %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
kd_stationarity <- df_kd %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
kd_stationarity <- df_kd %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

kd_stationarity <- df_kd %>% drop_na(7) %>% pull(7) %>% diff(differences = 2) 
tseries::adf.test(kd_stationarity) ; tseries::kpss.test(kd_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Kelantan
### Number of fishermen 
kl_stationarity <- df_kl %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
kl_stationarity <- df_kl %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
kl_stationarity <- df_kl %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

kl_stationarity <- df_kl %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
kl_stationarity <- df_kl %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
kl_stationarity <- df_kl %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

kl_stationarity <- df_kl %>% drop_na(7) %>% pull(7) %>% diff(differences = 2) 
tseries::adf.test(kl_stationarity) ; tseries::kpss.test(kl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Malacca
### Number of fishermen 
ml_stationarity <- df_ml %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
ml_stationarity <- df_ml %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
ml_stationarity <- df_ml %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

ml_stationarity <- df_ml %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
ml_stationarity <- df_ml %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
ml_stationarity <- df_ml %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(ml_stationarity) ; tseries::kpss.test(ml_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Negri Sembilan
### Number of fishermen 
ns_stationarity <- df_ns %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(ns_stationarity) ; tseries::kpss.test(ns_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
ns_stationarity <- df_ns %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(ns_stationarity) ; tseries::kpss.test(ns_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
ns_stationarity <- df_ns %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(ns_stationarity) ; tseries::kpss.test(ns_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
ns_stationarity <- df_ns %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(ns_stationarity) ; tseries::kpss.test(ns_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
ns_stationarity <- df_ns %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(ns_stationarity) ; tseries::kpss.test(ns_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Pahang
### Number of fishermen 
pa_stationarity <- df_pa %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pa_stationarity <- df_pa %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
pa_stationarity <- df_pa %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pa_stationarity <- df_pa %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
pa_stationarity <- df_pa %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pa_stationarity <- df_pa %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
pa_stationarity <- df_pa %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
pa_stationarity <- df_pa %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(pa_stationarity) ; tseries::kpss.test(pa_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Penang
### Number of fishermen 
pp_stationarity <- df_pp %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
pp_stationarity <- df_pp %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
pp_stationarity <- df_pp %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pp_stationarity <- df_pp %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
pp_stationarity <- df_pp %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
pp_stationarity <- df_pp %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(pp_stationarity) ; tseries::kpss.test(pp_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Perak
### Number of fishermen 
pr_stationarity <- df_pr %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pr_stationarity) ; tseries::kpss.test(pr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
pr_stationarity <- df_pr %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pr_stationarity) ; tseries::kpss.test(pr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
pr_stationarity <- df_pr %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pr_stationarity) ; tseries::kpss.test(pr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
pr_stationarity <- df_pr %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(pr_stationarity) ; tseries::kpss.test(pr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
pr_stationarity <- df_pr %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(pr_stationarity) ; tseries::kpss.test(pr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Selangor
### Number of fishermen 
sl_stationarity <- df_sl %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
sl_stationarity <- df_sl %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
sl_stationarity <- df_sl %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
sl_stationarity <- df_sl %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sl_stationarity <- df_sl %>% drop_na(6) %>% pull(6) %>% diff(differences = 2) 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
sl_stationarity <- df_sl %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(sl_stationarity) ; tseries::kpss.test(sl_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Singapore
### Number of fishermen 
sg_stationarity <- df_sg %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
sg_stationarity <- df_sg %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sg_stationarity <- df_sg %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
sg_stationarity <- df_sg %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sg_stationarity <- df_sg %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
sg_stationarity <- df_sg %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
sg_stationarity <- df_sg %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(sg_stationarity) ; tseries::kpss.test(sg_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Terengganu
### Number of fishermen 
tr_stationarity <- df_tr %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
tr_stationarity <- df_tr %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## p value of >0.05 based on the KPSS test indicates that the time-series is non stationary. 

tr_stationarity <- df_tr %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
tr_stationarity <- df_tr %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
tr_stationarity <- df_tr %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
tr_stationarity <- df_tr %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

tr_stationarity <- df_tr %>% drop_na(7) %>% pull(7) %>% diff(differences = 2) 
tseries::adf.test(tr_stationarity) ; tseries::kpss.test(tr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Labuan
### Number of fishermen 
la_stationarity <- df_la %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

la_stationarity <- df_la %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 
## This is likely due to the truncated time series data present. 

### Number of Non-Powered Fishing Vessels 
la_stationarity <- df_la %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

la_stationarity <- df_la %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
la_stationarity <- df_la %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

la_stationarity <- df_la %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 
## This is likely due to the truncated time series data present. 

### Total catch
la_stationarity <- df_la %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

la_stationarity <- df_la %>% drop_na(6) %>% pull(6) %>% diff(differences = 2) 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
la_stationarity <- df_la %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

la_stationarity <- df_la %>% drop_na(7) %>% pull(7) %>% diff(differences = 2) 
tseries::adf.test(la_stationarity) ; tseries::kpss.test(la_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Based on the non-stationary patterns in two fishery-related trends, we will not use labuan in the Granger-Causality Test. 

######################################################################################################

## Johore East
### Number of fishermen 
je_stationarity <- df_je %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

je_stationarity <- df_je %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
je_stationarity <- df_je %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
je_stationarity <- df_je %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

je_stationarity <- df_je %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
je_stationarity <- df_je %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
je_stationarity <- df_je %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(je_stationarity) ; tseries::kpss.test(je_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Perlis
### Number of fishermen 
pe_stationarity <- df_pe %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller and KPSS test indicates that the time-series is non stationary. 

pe_stationarity <- df_pe %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
pe_stationarity <- df_pe %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
pe_stationarity <- df_pe %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pe_stationarity <- df_pe %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
pe_stationarity <- df_pe %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
pe_stationarity <- df_pe %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

pe_stationarity <- df_pe %>% drop_na(7) %>% pull(7) %>% diff(differences = 2) 
tseries::adf.test(pe_stationarity) ; tseries::kpss.test(pe_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Johore West
### Number of fishermen 
jw_stationarity <- df_jw %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

jw_stationarity <- df_jw %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
jw_stationarity <- df_jw %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
jw_stationarity <- df_jw %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
jw_stationarity <- df_jw %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
jw_stationarity <- df_jw %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(jw_stationarity) ; tseries::kpss.test(jw_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Sabah
### Number of fishermen 
sb_stationarity <- df_sb %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sb_stationarity <- df_sb %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
sb_stationarity <- df_sb %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sb_stationarity <- df_sb %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
sb_stationarity <- df_sb %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sb_stationarity <- df_sb %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
sb_stationarity <- df_sb %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sb_stationarity <- df_sb %>% drop_na(6) %>% pull(6) %>% diff(differences = 2) 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
sb_stationarity <- df_sb %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(sb_stationarity) ; tseries::kpss.test(sb_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################

## Sarawak
### Number of fishermen 
sr_stationarity <- df_sr %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sr_stationarity <- df_sr %>% drop_na(3) %>% pull(3) %>% diff(differences = 2) 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Non-Powered Fishing Vessels 
sr_stationarity <- df_sr %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sr_stationarity <- df_sr %>% drop_na(4) %>% pull(4) %>% diff(differences = 2) 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Number of Powered Fishing Vessels 
sr_stationarity <- df_sr %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sr_stationarity <- df_sr %>% drop_na(5) %>% pull(5) %>% diff(differences = 2) 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Total catch
sr_stationarity <- df_sr %>% drop_na(6) %>% pull(6) %>% diff() 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## p value of >0.05 based on the Augmented Dickey Fuller test indicates that the time-series is non stationary. 

sr_stationarity <- df_sr %>% drop_na(6) %>% pull(6) %>% diff(differences = 2) 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

### Oil prices
sr_stationarity <- df_sr %>% drop_na(7) %>% pull(7) %>% diff() 
tseries::adf.test(sr_stationarity) ; tseries::kpss.test(sr_stationarity,null="Trend")
## Stationarity is present in the first group based on both Augmented Dickey Fuller and KPSS tests.

######################################################################################################
######################################################################################################

rm(kd_stationarity,kl_stationarity,ml_stationarity,ns_stationarity,
   pa_stationarity,pp_stationarity,pr_stationarity,sl_stationarity,
   sg_stationarity,tr_stationarity,la_stationarity,je_stationarity,
   pe_stationarity,jw_stationarity,sb_stationarity,sr_stationarity)

## Truncate the data frame and remove all NA values from each State specific data frame. 
## Note that there are only 15 data frames here due to non-stationarity in the Labuan data. 
df_kd <- df_kd %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_kl <- df_kl %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_ml <- df_ml %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_ns <- df_ns %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_pa <- df_pa %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_pp <- df_pp %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_pr <- df_pr %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_sl <- df_sl %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_sg <- df_sg %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_tr <- df_tr %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_je <- df_je %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_pe <- df_pe %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_jw <- df_jw %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_sb <- df_sb %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))
df_sr <- df_sr %>% filter(year >= "1968") %>% drop_na() %>% subset(select = -c(year, State))

### Identify the maximum lag length on frequency of the data
## https://www.linkedin.com/advice/3/how-do-you-choose-optimal-lag-length-granger-causality#:~:text=There%20is%20no%20definitive%20answer,and%20length%20of%20your%20data. 
mVAR_kd <- VAR(df_kd, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_kl <- VAR(df_kl, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_ml <- VAR(df_ml, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_ns <- VAR(df_ns, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_pa <- VAR(df_pa, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_pp <- VAR(df_pp, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_pr <- VAR(df_pr, lag.max = 8, type = c("const", "trend", "both", "none"))
mVAR_sl <- VAR(df_sl, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_sg <- VAR(df_sg, lag.max = 5, type = c("const", "trend", "both", "none"))
mVAR_tr <- VAR(df_tr, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_je <- VAR(df_je, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_pe <- VAR(df_pe, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_jw <- VAR(df_jw, lag.max = 7, type = c("const", "trend", "both", "none"))
mVAR_sb <- VAR(df_sb, lag.max = 4, type = c("const", "trend", "both", "none"))
mVAR_sr <- VAR(df_sr, lag.max = 4, type = c("const", "trend", "both", "none"))

## There is no hard-and-fast-rule on the choice of lag order. 
## It is basically an empirical issue. However, it is often advised to use the AIC in selecting the lag order with the smallest value.
## The lag interval was selected based on the AIC value identified in each VAR model.

######################################################################################################
######################################################################################################

### Calculate second order differences for each variable in the data frame to remove the effect of stationarity. 
df_kd <- as.data.frame(lapply(df_kd, diff, differences = 2))
df_kl <- as.data.frame(lapply(df_kl, diff, differences = 2))
df_ml <- as.data.frame(lapply(df_ml, diff, differences = 2))
df_ns <- as.data.frame(lapply(df_ns, diff, differences = 2))
df_pa <- as.data.frame(lapply(df_pa, diff, differences = 2))
df_pp <- as.data.frame(lapply(df_pp, diff, differences = 2))
df_pr <- as.data.frame(lapply(df_pr, diff, differences = 2))
df_sl <- as.data.frame(lapply(df_sl, diff, differences = 2))
df_sg <- as.data.frame(lapply(df_sg, diff, differences = 2))
df_tr <- as.data.frame(lapply(df_tr, diff, differences = 2))
df_je <- as.data.frame(lapply(df_je, diff, differences = 2))
df_pe <- as.data.frame(lapply(df_pe, diff, differences = 2))
df_jw <- as.data.frame(lapply(df_jw, diff, differences = 2))
df_sb <- as.data.frame(lapply(df_sb, diff, differences = 2))
df_sr <- as.data.frame(lapply(df_sr, diff, differences = 2))

### Perform Granger-Causality Test for each State ####
## We need to perform comparisons between catch and the the following columns, 
## Number of fishermen, number of non-powered fishing vessels, number of powered fishing vessels, total catch and oil prices. 
colnames(compiled_data)

### Kedah 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_kd) 
grangertest(catch ~ oil_prices, order = 7, data = df_kd) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_kd) 
grangertest(catch ~ Powered, order = 7, data = df_kd) 
## Here, we see that in the State of Kedah, the total catch is dependent on the number of fishermen present.
## Catch is also dependent on the oil prices and number of powered vessels. 

## We will need to rule out the possibility of reverse causation. 
grangertest(fishermen ~ catch, order = 7, data = df_kd) 
grangertest(`Non.powered` ~ catch, order = 7, data = df_kd) 
## We cannot rule out the possibility that the number of fishermen and number of non-powered vessels dependent on the amount of catch. 

### Kelantan 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_kl) 
grangertest(catch ~ oil_prices, order = 7, data = df_kl) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_kl) 
grangertest(catch ~ Powered, order = 7, data = df_kl) 
## Here, we see that in the State of Kelantan, the total catch is dependent on the number of powered vessels. 

## We will need to rule out the possibility of reverse causation. 
grangertest(Powered ~ catch, order = 7, data = df_kl) 
## This implies that the number of fishermen are not dependent on the number of catch. However, catch is dependent on the total number of powered vessels. 

### Malacca 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_ml) 
grangertest(catch ~ oil_prices, order = 7, data = df_ml) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_ml) 
grangertest(catch ~ Powered, order = 7, data = df_ml) 
## Here, we see that in the State of Malacca, the total catch is dependent on the number of non-powered fishing vessels present. 

## We will need to rule out the possibility of reverse causation. 
grangertest(`Non.powered` ~ catch, order = 7, data = df_ml) 
## This implies that the number of non-powered fishing vessels are not dependent on the number of catch. 

### Negri Sembilan 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_ns) 
grangertest(catch ~ oil_prices, order = 7, data = df_ns) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_ns) 
grangertest(catch ~ Powered, order = 7, data = df_ns) 
## Here, we see that in the State of Negri Sembilan, the total catch is not dependent on any of the societal variables. 

### Pahang 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_pa) 
grangertest(catch ~ oil_prices, order = 7, data = df_pa) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_pa) 
grangertest(catch ~ Powered, order = 7, data = df_pa) 
## Here, we see that in the State of Pahang, the total catch is not dependent on any of the societal variables. 

### Penang 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_pp) 
grangertest(catch ~ oil_prices, order = 7, data = df_pp) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_pp) 
grangertest(catch ~ Powered, order = 7, data = df_pp) 
## Here, we see that in the State of Penang, the total catch is not dependent on any of the societal variables. 

### Perak 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 8, data = df_pr) 
grangertest(catch ~ oil_prices, order = 8, data = df_pr) 
grangertest(catch ~ `Non.powered`, order = 8, data = df_pr) 
grangertest(catch ~ Powered, order = 8, data = df_pr) 
## Here, we see that in the State of Perak, the total catch is dependent on oil prices. 

## We will need to rule out the possibility of reverse causation. 
grangertest(oil_prices ~ catch, order = 7, data = df_pr) 
## This implies that oil prices are not dependent on the number of catch. 

### Selangor 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_sl) 
grangertest(catch ~ oil_prices, order = 7, data = df_sl) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_sl) 
grangertest(catch ~ Powered, order = 7, data = df_sl) 
## Here, we see that in the State of Selangor, the total catch is dependent on the number of powered fishing vessels present. 

## We will need to rule out the possibility of reverse causation. 
grangertest(Powered ~ catch, order = 7, data = df_sl) 
## The number of powered fishing vessels is dependent on the number of catch present. 

### Singapore 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 5, data = df_sg) 
grangertest(catch ~ oil_prices, order = 5, data = df_sg) 
grangertest(catch ~ `Non.powered`, order = 5, data = df_sg) 
grangertest(catch ~ Powered, order = 5, data = df_sg) 
## Here, we see that in the State of Singapore, the total catch is not dependent on any of the societal variables. 

### Terengganu 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_tr) 
grangertest(catch ~ oil_prices, order = 7, data = df_tr) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_tr) 
grangertest(catch ~ Powered, order = 7, data = df_tr) 
## Here, we see that in the State of Terengganu, the total catch is not dependent on any of the societal variables. 

### Johore East 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_je) 
grangertest(catch ~ oil_prices, order = 7, data = df_je) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_je) 
grangertest(catch ~ Powered, order = 7, data = df_je) 
## Here, we see that in the State of Johore East, the total catch is not dependent on any of the societal variables. 

### Perlis 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_pe) 
grangertest(catch ~ oil_prices, order = 7, data = df_pe) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_pe) 
grangertest(catch ~ Powered, order = 7, data = df_pe) 
## Here, we see that in the State of Perlis, the total catch is not dependent on any of the societal variables. 

### Johore West  
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 7, data = df_jw) 
grangertest(catch ~ oil_prices, order = 7, data = df_jw) 
grangertest(catch ~ `Non.powered`, order = 7, data = df_jw) 
grangertest(catch ~ Powered, order = 7, data = df_jw) 
## Here, we see that in Johore West, the total catch is not dependent on any of the societal variables. 

### Sabah 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 4, data = df_sb) 
grangertest(catch ~ oil_prices, order = 4, data = df_sb) 
grangertest(catch ~ `Non.powered`, order = 4, data = df_sb) 
grangertest(catch ~ Powered, order = 4, data = df_sb) 
## Here, we see that in the State of Sabah, the total catch is not dependent on any of the societal variables. 

### Sarawak 
## Relationship between catch and the other time-series variables 
grangertest(catch ~ fishermen, order = 4, data = df_sr) 
grangertest(catch ~ oil_prices, order = 4, data = df_sr) 
grangertest(catch ~ `Non.powered`, order = 4, data = df_sr) 
grangertest(catch ~ Powered, order = 4, data = df_sr) 
## Here, we see that in the State of Sarwak, the total catch is dependent on the number of non-powered fishing vessels. 

## We will need to rule out the possibility of reverse causation. 
grangertest(`Non.powered` ~ catch, order = 4, data = df_sr) 
## The number of non-powered fishing vessels is not dependent on the number of catch present. 

### Performing multivariate Granger-Causality Test
### Create new variables based on the optimal lag interval defined by AIC(n) in VARselect().
mVAR_kd <- VAR(df_kd, p = 7)
mVAR_kl <- VAR(df_kl, p = 7)
mVAR_ml <- VAR(df_ml, p = 7)
mVAR_ns <- VAR(df_ns, p = 7)
mVAR_pa <- VAR(df_pa, p = 7)
mVAR_pp <- VAR(df_pp, p = 7)
mVAR_pr <- VAR(df_pr, p = 8)
mVAR_sl <- VAR(df_sl, p = 7)
mVAR_sg <- VAR(df_sg, p = 5)
mVAR_tr <- VAR(df_tr, p = 7)
mVAR_je <- VAR(df_je, p = 7)
mVAR_pe <- VAR(df_pe, p = 7)
mVAR_jw <- VAR(df_jw, p = 7)
mVAR_sb <- VAR(df_sb, p = 4)
mVAR_sr <- VAR(df_sr, p = 4)

### Assess F, chisquare, and p values to establish significance. 
bruceR::granger_causality(mVAR_kd) 
bruceR::granger_causality(mVAR_kl) 
bruceR::granger_causality(mVAR_ml) 
bruceR::granger_causality(mVAR_ns) 
bruceR::granger_causality(mVAR_pa) 
bruceR::granger_causality(mVAR_pp) 
bruceR::granger_causality(mVAR_pr) 
bruceR::granger_causality(mVAR_sl) 
bruceR::granger_causality(mVAR_sg) 
bruceR::granger_causality(mVAR_tr) 
bruceR::granger_causality(mVAR_je) 
bruceR::granger_causality(mVAR_pe) 
bruceR::granger_causality(mVAR_jw) 
bruceR::granger_causality(mVAR_sb) 
bruceR::granger_causality(mVAR_sr) 

######################################################################################################
######################################################################################################

### Perform Granger Causality tests for the various species between states
species_data <- catch_data 
unique(species_data$var)

species_data <- merge(compiled_data, species_data, by = c("year", "State"))
species_data <- species_data %>%
  group_by(var) %>% 
  subset(select = -c(posterior.prob)) %>%
  drop_na() %>%
  subset(select = -c(State, Grouping_Name, values)) 

species_data <- species_data %>% 
  group_by(var) %>% 
  mutate(across(everything(),  ~ coalesce(.x -lag(.x), .x))) %>%
  subset(year < 1000)

## Check each species-specific time series pattern
summary_species <- species_data %>% 
  group_by(var) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  print(n = Inf)
head(summary_species, 20) # Filter based on the length of the time series data without missing values

## Filter for Rastrelliger spp. only
catch_data %>% 
  filter(Grouping_Name == "Rastrelliger spp.") %>%
  subset(select = -c(posterior.prob)) %>%
  group_by(var) %>%
  arrange(desc(State)) %>%
  print(n = Inf)

## Filter across states to identify the group that is most commercially valuable to each state
catch_data %>%
  group_by(State) %>%
  subset(select = -c(posterior.prob)) %>%
  slice_max(values) 

catch_data %>%
  group_by(State) %>%
  filter(Grouping_Name == "Rastrelliger spp.") %>%
  subset(select = -c(posterior.prob)) %>%
  slice_max(values) 

######################################################################################################
######################################################################################################

### Analyse the state specific catch patterns of Rastrelliger spp. ####
### Perlis 
pe_rs <- species_data %>% 
  filter(var == "Perlis_Rastrelliger spp.") %>%
  subset(select = -c(1, 7))

pe_rs_stationarity <- pe_rs %>% drop_na(1) %>% pull(1) %>% diff() 
tseries::adf.test(pe_rs_stationarity) ; tseries::kpss.test(pe_rs_stationarity, null="Trend")
## Stationarity is peesent based on both Augmented Dickey Fuller and KPSS tests.

pe_rs_stationarity <- pe_rs %>% drop_na(2) %>% pull(2) %>% diff() 
tseries::adf.test(pe_rs_stationarity) ; tseries::kpss.test(pe_rs_stationarity, null="Trend")
## Stationarity is peesent based on both Augmented Dickey Fuller and KPSS tests.

pe_rs_stationarity <- pe_rs %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pe_rs_stationarity) ; tseries::kpss.test(pe_rs_stationarity, null="Trend")
## Stationarity is peesent based on Augmented Dickey Fuller test.

pe_rs_stationarity <- pe_rs %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pe_rs_stationarity) ; tseries::kpss.test(pe_rs_stationarity, null="Trend")
## Stationarity is peesent based on Augmented Dickey Fuller test.

pe_rs_stationarity <- pe_rs %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pe_rs_stationarity) ; tseries::kpss.test(pe_rs_stationarity, null="Trend")
## Stationarity is peesent based on Augmented Dickey Fuller test.

pe_rs <- as.data.frame(lapply(pe_rs, diff, differences = 2))

VARselect(pe_rs, lag.max = 10, type = c("const", "trend", "both", "none"), season = NULL, exogen = NULL)

grangertest(catch ~ fishermen, order = 3, data = pe_rs) 
grangertest(catch ~ Powered, order = 3, data = pe_rs) 
grangertest(catch ~ `Non.powered`, order = 3, data = pe_rs) 
grangertest(catch ~ oil_prices, order = 3, data = pe_rs) 

grangertest(Powered ~ catch, order = 3, data = pe_rs) 

### Kedah ####
kd_rs <- species_data %>% 
  filter(var == "Kedah_Rastrelliger spp.") %>%
  subset(select = -c(1, 7))

kd_rs_stationarity <- kd_rs %>% drop_na(1) %>% pull(1) %>% diff() 
tseries::adf.test(kd_rs_stationarity) ; tseries::kpss.test(kd_rs_stationarity, null="Trend")
## Stationarity is present based on both Augmented Dickey Fuller and KPSS tests.

kd_rs_stationarity <- kd_rs %>% drop_na(2) %>% pull(2) %>% diff() 
tseries::adf.test(kd_rs_stationarity) ; tseries::kpss.test(kd_rs_stationarity, null="Trend")
## Stationarity is present based on both Augmented Dickey Fuller and KPSS tests.

kd_rs_stationarity <- kd_rs %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(kd_rs_stationarity) ; tseries::kpss.test(kd_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

kd_rs_stationarity <- kd_rs %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(kd_rs_stationarity) ; tseries::kpss.test(kd_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

kd_rs_stationarity <- kd_rs %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(kd_rs_stationarity) ; tseries::kpss.test(kd_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

kd_rs <- as.data.frame(lapply(kd_rs, diff, differences = 2))

VARselect(kd_rs, lag.max = 10, type = c("const", "trend", "both", "none"), season = NULL, exogen = NULL)

grangertest(catch ~ fishermen, order = 4, data = kd_rs) 
grangertest(catch ~ Powered, order = 4, data = kd_rs) 
grangertest(catch ~ `Non.powered`, order = 4, data = kd_rs) 
grangertest(catch ~ oil_prices, order = 4, data = kd_rs) 

### Perak ####
pr_rs <- species_data %>% 
  filter(var == "Perak_Rastrelliger spp.") %>%
  subset(select = -c(1, 7))

pr_rs_stationarity <- pr_rs %>% drop_na(1) %>% pull(1) %>% diff() 
tseries::adf.test(pr_rs_stationarity) ; tseries::kpss.test(pr_rs_stationarity, null="Trend")
## Stationarity is present based on both Augmented Dickey Fuller and KPSS tests.

pr_rs_stationarity <- pr_rs %>% drop_na(2) %>% pull(2) %>% diff() 
tseries::adf.test(pr_rs_stationarity) ; tseries::kpss.test(pr_rs_stationarity, null="Trend")
## Stationarity is present based on both Augmented Dickey Fuller and KPSS tests.

pr_rs_stationarity <- pr_rs %>% drop_na(3) %>% pull(3) %>% diff() 
tseries::adf.test(pr_rs_stationarity) ; tseries::kpss.test(pr_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

pr_rs_stationarity <- pr_rs %>% drop_na(4) %>% pull(4) %>% diff() 
tseries::adf.test(pr_rs_stationarity) ; tseries::kpss.test(pr_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

pr_rs_stationarity <- pr_rs %>% drop_na(5) %>% pull(5) %>% diff() 
tseries::adf.test(pr_rs_stationarity) ; tseries::kpss.test(pr_rs_stationarity, null="Trend")
## Stationarity is present based on Augmented Dickey Fuller test.

pr_rs <- as.data.frame(lapply(pr_rs, diff, differences = 2))

VARselect(pr_rs, lag.max = 10, type = c("const", "trend", "both", "none"), season = NULL, exogen = NULL)

grangertest(catch ~ fishermen, order = 7, data = pr_rs) 
grangertest(catch ~ Powered, order = 7, data = pr_rs) 
grangertest(catch ~ `Non.powered`, order = 7, data = pr_rs) 
grangertest(catch ~ oil_prices, order = 7, data = pr_rs) 

grangertest(Powered ~ catch, order = 7, data = pr_rs) 

### Identify the most important variables in Rastrelliger harvest patterns
# Kedah - None 
# Perlis - Powered vessels (p = 0.0476), no reverse causation
# Perak - Powered vessels (p = 0.0293), no reverse causation

## Case examples for specific species across space and time
plot4a <- catch_data %>% 
  filter(var == "Perlis_Rastrelliger spp.") %>% 
  ggplot(aes(x=year, y=values))+
  geom_point(size=1.5, shape=15)+
  geom_smooth(stat='smooth')+
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name='Landings (thousand tons)', limits = c(-8, 54), breaks = c(0, 10, 20, 30, 40, 50))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'bottom',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4a

plot4b <- boat_type %>%
  filter(Type == "Powered" & State == "Perlis") %>%
  ggplot(aes(x=Year, y=Number))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth')+
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name='Number of Powered Boats')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'bottom',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4b

plot4c <- catch_data %>% 
  filter(var == "Perak_Rastrelliger spp.") %>% 
  ggplot(aes(x=year, y=values))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth')+
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name='Landings (thousand tons)', limits = c(-10, 75), breaks = c(0, 10, 20, 30, 40, 50, 60, 70))+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'bottom',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4c

plot4d <- boat_type %>%
  filter(Type == "Powered" & State == "Perak") %>%
  ggplot(aes(x=Year, y=Number))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth')+
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name='Number of Powered Boats')+
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'bottom',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4d

plot4 <- ggpubr::ggarrange(plot4a, plot4b, plot4c, plot4d, labels = c("A", "B", "C", "D"), align = "v")
plot4

scale1 = 20

plot4e <- ggplot()+
  geom_point(data = boat_type %>% filter(Type == "Powered" & State == "Perlis"), aes(x=Year, y=Number/scale1, colour = 'Powered boats'), size=2, shape=17)+
  geom_point(data = catch_data %>% filter(var == "Perlis_Rastrelliger spp."), aes(x=year, y=values, colour = 'Landings'), size=2, shape=15)+
  geom_smooth(data = boat_type %>% filter(Type == "Powered" & State == "Perlis"), aes(x=Year, y=Number/scale1), stat='smooth', colour = '#000000', se = F)+ 
  geom_smooth(data = catch_data %>% filter(var == "Perlis_Rastrelliger spp."), aes(x=year, y=values), stat='smooth', colour = '#000000', se = F)+ 
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name="Landings (thousand tons)", sec.axis = sec_axis(~ . * scale1, name = "Number of Powered Boats"))+
  scale_color_manual(name='Legend', breaks=c('Powered boats', 'Landings'), values=c('Powered boats'='#F8766D', 'Landings'='#619CFF')) +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4e

scale2 = 80

plot4f <- ggplot()+
  geom_point(data = boat_type %>% filter(Type == "Powered" & State == "Perak"), aes(x=Year, y=Number/scale2, colour = 'Powered boats'), size=2, shape=17)+
  geom_point(data = catch_data %>% filter(var == "Perak_Rastrelliger spp."), aes(x=year, y=values, colour = 'Landings'), size=2, shape=15)+
  geom_smooth(data = boat_type %>% filter(Type == "Powered" & State == "Perak"), aes(x=Year, y=Number/scale2), stat='smooth', colour = '#000000', se = F)+ 
  geom_smooth(data = catch_data %>% filter(var == "Perak_Rastrelliger spp."), aes(x=year, y=values), stat='smooth', colour = '#000000', se = F)+ 
  scale_x_continuous(limits=c(1920,2022), breaks=c(1920,1940,1960,1980,2000,2020), labels=c(1920,1940,1960,1980,2000,2020), name='Year')+
  scale_y_continuous(name="Landings (thousand tons)", sec.axis = sec_axis(~ . * scale2, name = "Number of Powered Boats"))+
  scale_color_manual(name='Legend', breaks=c('Power boats', 'Landings'), values=c('Powered boats'='#F8766D', 'Landings'='#619CFF')) +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=18),
        legend.position = 'right',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot4f

plot4 <- ggpubr::ggarrange(plot4e, plot4f, labels = c("A", "B"), ncol = 1, common.legend = TRUE, legend = 'right')
plot4

######################################################################################################
######################################################################################################

### Trend and change point analyses #### 
state_catch <- catch_data # Replace the state catch data frame with catch data

## Create a new data frame containing change points 
summary_catch <- catch_data %>% 
  group_by(State, Grouping_Name) %>%
  summarise(n())

## Count the number of change points in the data 
summary_cp <- catch_data %>%
  filter(posterior.prob >= 0.70) %>%
  arrange(State, Grouping_Name) %>%
  group_by(State, Grouping_Name) %>%
  summarise(n()) %>%
  rename(cp = `n()`)

summary_catch <- left_join(summary_catch, summary_cp, by = c('State', 'Grouping_Name')) %>% mutate(cp = replace(cp, is.na(cp), 0))

summary_catch %>% 
  filter(`n()` >= 20) %>% 
  arrange(Grouping_Name) %>%
  print(n = Inf) 
summary_catch

### Example of trend analysis
catch_trend <- catch_data 
catch_trend <- catch_trend %>% 
  filter(year >= "1968") %>% 
  group_by(var) %>% 
  filter(n() > 3) 

catch_trend <- catch_trend %>%
  group_by(var) %>%
  group_map(~ stats::ts(.x$values)) 
catch_trend <- catch_trend %>% map(~ Kendall::MannKendall(.))

catch_trend <- do.call("rbind", catch_trend)
catch_trend <- as.data.frame(catch_trend)
catch_trend <- catch_trend %>%
  rename("pvalue" = "sl")
catch_trend$n <- 1:nrow(catch_trend) 
data.table::setDT(catch_trend)
catch_trend <- as.data.frame(catch_trend)

group_trend <- catch_data %>% 
  filter(year >= "1968") %>% 
  group_by(var) %>% 
  filter(n() > 3) %>% 
  distinct(var)
group_trend$n <- 1:nrow(group_trend) 
group_trend <- as.data.frame(group_trend)

overall_trend <- merge(catch_trend, group_trend, by='n')
overall_trend <- splitstackshape::cSplit(overall_trend, "var", "_", drop = F)
overall_trend <- overall_trend %>%
  subset(select = -c(n)) %>%
  rename(State = var_1, Grouping_Name = var_2)
data.table::setcolorder(overall_trend, c("var","State","Grouping_Name","tau","S","D","varS","pvalue"))

overall_trend$pvalue <- as.numeric(overall_trend$pvalue)
overall_trend$pvalue <- round(overall_trend$pvalue, 5)
overall_trend <- overall_trend %>% arrange(Grouping_Name, State)
overall_trend

unique(overall_trend$Grouping_Name)
#overall_trend %>% filter(Grouping_Name == "") %>% arrange(pvalue)

######################################################################################################
######################################################################################################

### Identify seasonal patterns in harvest along the east coast of Peninsula Malaysia 
overall_trend %>% 
  filter(pvalue < 0.05 & (State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East")) %>% 
  group_by(Grouping_Name) %>%
  filter(n() > 3) %>%
  print(n=Inf)
# There are seven species which exhibit seasonal patterns in harvest patterns.

catch_data %>% 
  filter(Grouping_Name == "Lobsters" & (State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East")) %>% 
  group_by(State, Grouping_Name) %>%
  summarise(n())

catch_data %>%
  filter(Grouping_Name == "Lobsters" & year >= "1968") %>%
  ggplot(aes(x=year, y=values))+
  geom_point()+
  facet_wrap(.~State)

### Example of Lobsters
catch_cp <- catch_data %>%
  filter(posterior.prob >= 0.70 & (State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East")) %>%
  arrange(State, Grouping_Name)
catch_cp <- catch_cp %>% filter(Grouping_Name == "Lobsters")
catch_cp

neworder <- c("Perlis","Kedah","Penang and Province Wellesley","Perak","Selangor","Negri Sembilan","Malacca","Johor West",
              "Singapore","Johore East","Pahang","Terengganu","Kelantan","Sarawak","Labuan","Sabah")

catch_cp2 <- arrange(transform(catch_cp, State=factor(State,levels=neworder)),State)
catch_cp2 <- catch_cp2 %>% filter(State == "Kelantan" | State == "Terengganu" | State == "Pahang" | State == "Johore East")
catch_data2 <- arrange(transform(catch_data, State=factor(State,levels=neworder)), State)

#catch_data2 <- catch_data2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))
#catch_cp2 <- catch_cp2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))

catch_data2 <- catch_data2 %>% mutate(State = case_when(State == "Johore East" ~ "Johor East", TRUE ~ State))
catch_cp2 <- catch_cp2 %>% mutate(State = case_when(State == "Johore East" ~ "Johor East", TRUE ~ State))

neworder <- c("Johor East","Pahang","Terengganu","Kelantan")
catch_data2 <- arrange(transform(catch_data2, State=factor(State,levels=neworder)),State)
catch_cp2 <- arrange(transform(catch_cp2, State=factor(State,levels=neworder)),State)

plot5a <- catch_data2 %>% 
  filter(Grouping_Name == "Lobsters") %>%
  filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan" | State == "Johor East") %>%
  ggplot(aes(x=year, y=values, group = State))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth', se = FALSE)+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State, ncol=4)+
  scale_y_continuous(name='Landings (thousand tonnes)', limits = c(0.0, 0.4), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4), labels = c('0.0', '0.1', '0.2', '0.3', '0.4'))+
  scale_x_continuous(name= 'Year', limits=c(1968, 2022), breaks= c(1970,1990,2010), labels= c('1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.55, "cm"))
plot5a

plot5b <- catch_data2 %>% 
  filter(Grouping_Name == "Lobsters") %>%
  filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan" | State == "Johor East") %>%
  ggplot(aes(x=year, y=posterior.prob, group = State))+
  geom_line(size=0.70)+
  geom_hline(yintercept = 0.75, size = 0.70, colour = "blue", linetype = 'dotdash')+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State, ncol=4)+
  scale_y_continuous(name='Posterior Probability', limits = c(0.00,1.00), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00))+
  scale_x_continuous(name= 'Year', limits=c(1968, 2022), breaks= c(1970,1990,2010), labels= c('1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot5b

plot5 <- ggpubr::ggarrange(plot5a, plot5b, nrow=2)
plot5

######################################################################################################
######################################################################################################

### Identify seasonal patterns in harvest along the west coast of Peninsula Malaysia 
overall_trend %>% 
  filter(pvalue < 0.05 & (State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West")) %>% 
  group_by(Grouping_Name) %>%
  filter(n() > 6) %>%
  print(n=Inf)
# There are two species which exhibit seasonal patterns in harvest across seven states. There are no species with seasonal catch patterns across all 8 states along the west coast. 

catch_data %>%
  filter(Grouping_Name == "Pomfrets" & year >= "1968") %>%
  ggplot(aes(x=year, y=values))+
  geom_point()+
  facet_wrap(.~State)

### Example of Pomfrets
catch_cp <- catch_data %>%
  filter(posterior.prob >= 0.70 & (State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West")) %>%
  arrange(State, Grouping_Name)
catch_cp <- catch_cp %>% filter(Grouping_Name == "Pomfrets")

neworder <- c("Perlis","Kedah","Penang and Province Wellesley","Perak","Selangor","Negri Sembilan","Malacca","Johor West",
              "Singapore","Johore East","Pahang","Terengganu","Kelantan","Sarawak","Labuan","Sabah")

catch_cp2 <- arrange(transform(catch_cp, State=factor(State,levels=neworder)),State)
catch_cp2 <- catch_cp2 %>% filter(State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West")
catch_data2 <- arrange(transform(catch_data, State=factor(State,levels=neworder)),State)

#catch_data2 <- catch_data2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))
#catch_cp2 <- catch_cp2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))

neworder <- c("Perlis","Kedah","Penang and Province Wellesley","Perak","Selangor","Negri Sembilan","Malacca","Johor West")
catch_data2 <- arrange(transform(catch_data2, State=factor(State,levels=neworder)),State)
catch_cp2 <- arrange(transform(catch_cp2, State=factor(State,levels=neworder)),State)

plot5a <- catch_data2 %>% 
  filter(Grouping_Name == "Pomfrets") %>%
  filter(State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West") %>%
  ggplot(aes(x=year, y=values, group = State))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth')+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State, ncol=4)+
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  scale_x_continuous(name= 'Year', limits=c(1968, 2022), breaks= c(1970,1990,2010), labels= c('1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.75, "cm"))
plot5a

plot5b <- catch_data2 %>% 
  filter(Grouping_Name == "Pomfrets") %>%
  filter(State == "Perlis" | State == "Kedah" | State == "Penang and Province Wellesley" | State == "Perak" | State == "Selangor" | State == "Negri Sembilan" | State == "Malacca" | State == "Johor West") %>%
  ggplot(aes(x=year, y=posterior.prob, group = State))+
  geom_line(size=0.70)+
  geom_hline(yintercept = 0.70, size = 0.70, colour = "blue", linetype = 'dotdash')+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State, ncol=4)+
  scale_y_continuous(name='Posterior Probability')+
  scale_x_continuous(name= 'Year', limits=c(1968, 2022), breaks= c(1970,1990,2010), labels= c('1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot5b

plot5 <- ggpubr::ggarrange(plot5a, plot5b, nrow=2)
plot5

######################################################################################################
######################################################################################################

summary_catch %>%
  filter(`n()` >= 20) %>% 
  group_by(Grouping_Name) %>%
  summarize(summary = n_distinct(State)) %>% 
  print(n=Inf)

### Example of Spanish Mackerel
catch_cp <- catch_data %>%
  filter(posterior.prob >= 0.70) %>%
  arrange(State, Grouping_Name)
catch_cp <- catch_cp %>% filter(Grouping_Name == "Scomberomorus spp.")

neworder <- c("Perlis","Kedah","Penang and Province Wellesley","Perak","Selangor","Negri Sembilan","Malacca","Johor West",
              "Singapore","Johore East","Pahang","Terengganu","Kelantan","Sarawak","Labuan","Sabah")

catch_cp2 <- arrange(transform(catch_cp, State=factor(State,levels=neworder)),State)
catch_cp2 <- catch_cp2 %>% filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan")
catch_data2 <- arrange(transform(catch_data, State=factor(State,levels=neworder)),State)

#catch_data2 <- catch_data2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))
#catch_cp2 <- catch_cp2 %>% mutate(State = case_when(State == "Trengganu" ~ "Terengganu", TRUE ~ State))

neworder <- c("Pahang","Terengganu","Kelantan")
catch_data2 <- arrange(transform(catch_data2, State=factor(State,levels=neworder)),State)
catch_cp2 <- arrange(transform(catch_cp2, State=factor(State,levels=neworder)),State)

plot5a <- catch_data2 %>% 
  filter(Grouping_Name == "Scomberomorus spp.") %>%
  filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan") %>%
  ggplot(aes(x=year, y=values, group = State))+
  geom_point(size=1.5)+
  geom_smooth(stat='smooth')+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State)+
  scale_y_continuous(name='Harvest (thousand tonnes)')+
  scale_x_continuous(name= 'Year', limits=c(1930, 2022), breaks= c(1930,1950,1970,1990,2010), labels= c('1930','1950','1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 1.2, "cm"))
plot5a

plot5b <- catch_data2 %>% 
  filter(Grouping_Name == "Scomberomorus spp.") %>%
  filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan") %>%
  ggplot(aes(x=year, y=posterior.prob, group = State))+
  geom_line(size=0.70)+
  geom_hline(yintercept = 0.70, size = 0.70, colour = "blue", linetype = 'dotdash')+
  geom_vline(data=catch_cp2, aes(xintercept=year, group = State), colour = "red", linetype = 'dashed')+
  facet_wrap(.~State)+
  scale_y_continuous(name='Posterior Probability')+
  scale_x_continuous(name= 'Year', limits=c(1930, 2022), breaks= c(1930,1950,1970,1990,2010), labels= c('1930','1950','1970','1990','2010'))+ 
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        strip.text.y = element_blank(), 
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "grey15", linewidth=0.3), 
        panel.border = element_blank(),
        panel.background = element_rect(colour="grey50", linewidth=0.3, fill="white"),
        panel.grid.major = element_line(colour = "grey85", linewidth=0.15),
        panel.grid.minor = element_line(colour = "grey85", linewidth=0.15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=14),
        panel.spacing = unit(1, "lines"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
plot5b

plot5 <- ggpubr::ggarrange(plot5a, plot5b, nrow=2)
plot5

catch_data2 %>% 
  filter(Grouping_Name == "Scomberomorus spp.") %>%
  filter(State == "Pahang" | State == "Terengganu" | State == "Kelantan") %>%
  filter(posterior.prob >= "0.70")

write.csv(unique(catch_data2$Grouping_Name), "marine_names.csv", row.names=FALSE)

######################################################################################################
######################################################################################################

### Trend analysis
## Assess for gradual trends in the time series data
## R guide from https://finnstats.com/2021/11/28/time-series-trend-analysis-in-r/ 

## Example for Scomberomorus from the three east coast states
## Pahang
catch_trend <- catch_data
catch_pa <- catch_trend %>% 
  filter(Grouping_Name == "Scomberomorus spp." & State == "Pahang" & year >= "1968") %>% 
  subset(select = c(values))
catch_pa <- ts(catch_pa)
MannKendall(catch_pa) ; plot(catch_pa)

## Terengganu
catch_tr <- catch_trend %>% 
  filter(Grouping_Name == "Scomberomorus spp." & State == "Terengganu" & year >= "1968") %>% 
  subset(select = c(values))
catch_tr <- ts(catch_tr)
MannKendall(catch_tr) ; plot(catch_tr) 

## Kelantan
catch_kl <- catch_trend %>% 
  filter(Grouping_Name == "Scomberomorus spp." & State == "Kelantan" & year >= "1968") %>% 
  subset(select = c(values))
catch_kl <- ts(catch_kl)
MannKendall(catch_kl) ; plot(catch_kl) 


### Example
catch_data %>%
  filter(Grouping_Name == "Squids" & year >= "1968") %>%
  ggplot(aes(x=year, y=values))+
  geom_point()+
  facet_wrap(.~State)

t1 <- catch_data %>%
  filter(Grouping_Name == "Squids" & year >= "1968" & State == "Singapore") %>%
  subset(select = c(values))
t1 <- ts(t1)
MannKendall(t1) ; plot(t1) 

summary_catch %>%
  filter(`n()` >= 20) %>% 
  group_by(Grouping_Name) %>%
  summarize(summary = n_distinct(State)) %>% 
  print(n=Inf)

######################################################################################################
######################################################################################################

remove.packages("ggmap")
devtools::install_github("stadiamaps/ggmap", force = T) 
library(sf)
library(ggforce)
library(rmapshaper)
library(rnaturalearth)
library(cowplot)

register_stadiamaps(key = "4eef9e3f-e6c1-4911-bdb3-8c243f7ef226")
spatial_Malaya <- get_stadiamap(bbox = c(top = 10, bottom = -10, left = 95, right = 120), maptype = c("alidade_smooth"), crop = FALSE, zoom = 7) 
ggmap(spatial_Malaya)

Malaysia_shp <- read_sf('MYS_adm1.shp')
Malaysia_shp <- ms_simplify(Malaysia_shp, keep = 0.01)

Singapore_shp <- read_sf('SGP_adm0.shp')
Singapore_shp <- ms_simplify(Singapore_shp, keep = 0.01)

base_map <- ggmap(spatial_Malaya) + 
  geom_sf(data = Singapore_shp, aes(geometry = geometry), alpha = 0.3, colour = 'black', linewidth = 0.2, fill = c("royalblue4"), inherit.aes = FALSE) + 
  geom_sf(data = Malaysia_shp, aes(geometry = geometry), alpha = 0.3, colour = 'black', linewidth = 0.2, fill = c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4", "brown"), inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  geom_segment(aes(x = 103.8, y = 1.35, xend = 104.8, yend = 0.35), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 100.2, y = 6.5, xend = 99.2, yend = 6.8), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 100.8, y = 4.3, xend = 99.8, yend = 4.1), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 103.1, y = 5, xend = 104.1, yend = 5.4), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 103.25, y = 3.35, xend = 104.25, yend = 3.40), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 103.9, y = 2.1, xend = 104.9, yend = 1.9), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 100.8, y = 2.2, xend = 99.8, yend = 2.4), color = "black", size = 0.5, alpha = 0.8) + 
  geom_segment(aes(x = 102.9, y = 1.5, xend = 100.9, yend = 1), color = "black", size = 0.5, alpha = 0.8) + 
  
  geom_circle(aes(x0 = 100.8, y0 = 2.2, r=0.1), colour = 'darkred', fill = 'darkred') +
  
  annotate('text', y=0.15, x=105.0, color='grey15', size=5, label='Singapore', fontface = 2) +
  annotate('text', y=7.1, x=98.0, color='grey15', size=5, label='Perlis', fontface = 2) +
  annotate('text', y=4.14, x=98.6, color='grey15', size=5, label='Perak', fontface = 2) +
  annotate('text', y=5.46, x=106.3, color='grey15', size=5, label='Terengganu', fontface = 2) +
  annotate('text', y= 3.41, x= 105.7, color='grey15', size=5, label='Pahang', fontface = 2) +
  annotate('text', y=1.82, x=106.0, color='grey15', size=5, label='Johor', fontface = 2) +
  annotate('text', y=9.7, x=93.4, color='grey15', size=5, label='Mergui\nArchipelago', fontface = 2, hjust = 0) +
  annotate('text', y=9.7, x=102, color='grey15', size=5, label='Gulf of\nThailand', fontface = 2) +
  annotate('text', y=2.6, x=96.7, color='grey15', size=5, label='Bagan Si Api Api', fontface = 2) +
  annotate('text', y=0.6, x=99.4, color='grey15', size=5, label='Malacca\nStraits', fontface = 2) +
  
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        plot.margin = grid::unit(c(0.5,0.5,0.5,0.5), "cm"))
base_map

world <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- ggplot(world)+ 
  geom_sf()+
  geom_rect(aes(xmin = 92, xmax = 123, ymin = -4, ymax = 11), colour = 'red', fill = NA) + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(20, 200), ylim = c(-50, 50), expand = FALSE) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color="black", linetype=1, linewidth=0.6),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = grid::unit(c(0,0,-1,-1), "mm"))
world_map

ggdraw()+
  draw_plot(base_map)+
  draw_plot(world_map, height=0.200, x=0.298, y=0.1855) 
# Image dimensions are 8.80 inches by 8.80 inches

ggpubr::ggarrange(base_map, world_map, nrow = 2)

######################################################################################################
######################################################################################################

save.image("2024_Historical_Ecology.RData") 

