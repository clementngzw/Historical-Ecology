### Doctor of Philosophy (Ph.D.)
## Chapter 1: Changes in the marine fishing industry of Malaysia and Singapore over the past century
## Clement Ng

#### Historical Ecology ####
## Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Load libraries
## Packages used for wrangling 
library(tidyverse)
library(data.table)

data <- fread('Capture_Quantity.csv')
country_code <- fread('CL_FI_COUNTRY_GROUPS.csv')
country_code %>% filter(GeoRegion_Group_En == "South-Eastern Asia")

theme_set(theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.02),
                axis.text = element_text(size=10),
                axis.title = element_text(size=10),
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
                plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")))

## Brunei
Brunei_data <- data %>% filter(COUNTRY.UN_CODE == "96")
Brunei_data <- Brunei_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p1 <- Brunei_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("A") +
  theme()
p1

## Myanmar
Myanmar_data <- data %>% filter(COUNTRY.UN_CODE == "104")
Myanmar_data <- Myanmar_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p2 <- Myanmar_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("B") +
  theme()
p2

## Indonesia
Indonesia_data <- data %>% filter(COUNTRY.UN_CODE == "360")
Indonesia_data <- Indonesia_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p3 <- Indonesia_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("C") +
  theme()
p3

## Cambodia
Cambodia_data <- data %>% filter(COUNTRY.UN_CODE == "116")
Cambodia_data <- Cambodia_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p4 <- Cambodia_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("D") +
  theme()
p4

## Laos
Laos_data <- data %>% filter(COUNTRY.UN_CODE == "418")
Laos_data <- Laos_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p5 <- Laos_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("E") +
  theme()
p5

## Malaysia
Malaysia_data <- data %>% filter(COUNTRY.UN_CODE == "458")
Malaysia_data <- Malaysia_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p6 <- Malaysia_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("F") +
  theme()
p6

## Philippines
Philippines_data <- data %>% filter(COUNTRY.UN_CODE == "608")
Philippines_data <- Philippines_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p7 <- Philippines_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("G") +
  theme()
p7

## Timor-Leste
Timor_data <- data %>% filter(COUNTRY.UN_CODE == "626")
Timor_data <- Timor_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p8 <- Timor_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("H") +
  theme()
p8

## Singapore
Singapore_data <- data %>% filter(COUNTRY.UN_CODE == "702")
Singapore_data <- Singapore_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p9 <- Singapore_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("I") +
  theme()
p9

## Thailand
Thailand_data <- data %>% filter(COUNTRY.UN_CODE == "764")
Thailand_data <- Thailand_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p10 <- Thailand_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("J") +
  theme()
p10

## Viet Nam
Vietnam_data <- data %>% filter(COUNTRY.UN_CODE == "704")
Vietnam_data <- Vietnam_data %>% group_by(PERIOD) %>% summarise(Harvest = sum(VALUE) / 1000) %>% rename(Year = PERIOD)
p11 <- Vietnam_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("K") +
  theme()
p11

## ASEAN
country_code %>% filter(GeoRegion_Group_En == "South-Eastern Asia") %>% distinct(UN_Code, Name_En)
asean_data <- data %>% 
  filter(COUNTRY.UN_CODE == "96" | COUNTRY.UN_CODE == "104" | COUNTRY.UN_CODE == "360" | COUNTRY.UN_CODE == "116" | COUNTRY.UN_CODE == "418" | COUNTRY.UN_CODE == "458" | COUNTRY.UN_CODE == "608" | COUNTRY.UN_CODE == "626" | COUNTRY.UN_CODE == "702" | COUNTRY.UN_CODE == "764" | COUNTRY.UN_CODE == "704")

asean_data <- asean_data %>% 
  group_by(PERIOD) %>%
  summarise(Harvest = sum(VALUE) / 1000) %>% 
  rename(Year = PERIOD)

p12 <- asean_data %>% 
  ggplot(aes(y = Harvest, x = Year)) +
  geom_point() +
  scale_x_continuous(breaks = c(1950,1970,1990,2010), labels = c(1950,1970,1990,2010)) +
  scale_y_continuous(name='Harvest (thousand tonnes)') +
  ggtitle("L") +
  theme() 
p12

plot <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
plot
