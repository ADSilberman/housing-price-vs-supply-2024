#clean and read in data ####
rm(list=ls())

knitr::opts_chunk$set(echo = TRUE,
                      root.dir = "C:\\Users\\alexa\\Code\\GW DATS\\6101 Intro to Data Science\\Project 1\\GitHub",
                      warning = F, message = F)
options(scientific = T, digits = 3)

library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library (tidyr)
library(janitor)
library(scales)
library(ggrepel)
library(corrplot)

getwd()
setwd("~/Documents/R/6101/Project_1/Data")
list.files()
housing = read.csv("state_county_data_download_2025.csv")
housing %>% slice_sample(n=5)

#filter for 2024, clean for analysis ####
housing_2024 = housing %>% filter(housing$Year == 2024) %>% na.omit
housing_2024$State = as.factor(housing_2024$State)
housing_2024$County = as.factor(housing_2024$County)
housing_2024$Tier = as.factor(housing_2024$Tier)
housing_2024 %>% slice_sample(n=5)

#summary stats table, to be investigated later
xkablesummary(housing_2024)

#clean variable names and data str
#tier def 
def= 
  {
    Low: all sales below the 40th percentile of FHA sales prices
    Low-medium: all sales at or below the 80th percentile of FHA sales prices
    Medium-high: all sales at or below 125% of the GSE loan limit
    High: all other sales
    entry-level: low and low-medium price tiers
    move-up: medium-high and high price tiers
  }
#list
names(housing_2024)
str(housing_2024)
#rename cols
housing_2024 <- housing_2024%>%
  rename(
    Median_Sale_Price_per_k = Median.Sale.Price..in.Thousands.,
    House_Price_Appreciation_yr_over_yr_percent = House.Price.Appreciation..Year.over.Year.,
    House_Price_Appreciation_since_2012_percent = House.Price.Appreciation.since.2012,
    Months_Supply = Months..Supply,
    New_Constr_by_share_of_sales_percent = New.Construction.Share.of.Sales,
    MortgageDefaultRate_percent = Mortgage.Default.Rate,
    County_Name = County,
    FIPS_County_Code = FIPS, 
    Affordability = Tier
  )
head(housing_2024)

     
#ditch '$' and '%' from values
housing_2024<- housing_2024%>%
  mutate(
    Median_Sale_Price_per_k = gsub("\\$", "", Median_Sale_Price_per_k),
    House_Price_Appreciation_since_2012_percent = gsub("%","",House_Price_Appreciation_since_2012_percent),
    House_Price_Appreciation_yr_over_yr_percent = gsub("%","",House_Price_Appreciation_yr_over_yr_percent),
    New_Constr_by_share_of_sales_percent = gsub("%","",New_Constr_by_share_of_sales_percent),
    MortgageDefaultRate_percent = gsub("%","",MortgageDefaultRate_percent)
  )

str(housing_2024)

#as int/num instead of chr
housing_2024$Median_Sale_Price_per_k = as.numeric(housing_2024$Median_Sale_Price_per_k)
housing_2024$House_Price_Appreciation_since_2012_percent = as.numeric(housing_2024$House_Price_Appreciation_since_2012_percent)
housing_2024$House_Price_Appreciation_yr_over_yr_percent= as.numeric(housing_2024$House_Price_Appreciation_yr_over_yr_percent)
housing_2024$New_Constr_by_share_of_sales_percent= as.numeric(housing_2024$New_Constr_by_share_of_sales_percent)
housing_2024$MortgageDefaultRate_percent= as.numeric(housing_2024$MortgageDefaultRate_percent)

str(housing_2024)
xkablesummary(housing_2024)

#run EDA####

#boxplot of median sale price by state####
ggplot(housing_2024, aes(x = reorder(State, -Median_Sale_Price_per_k, median), 
                         y = Median_Sale_Price_per_k)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Distribution of Median Sale Prices by State (2024)",
    x = "State",
    y = "Median Sale Price (in thousands)"
  ) +
  theme_minimal()
#too many to be particularly useful. You can see general trends, but I am going to run with a smaller state sample

#boxplot of top and bottom 10 states by housing count####
top_states <- housing_2024 %>%
  dplyr::count(State, sort = TRUE) %>%
  dplyr::slice_max(n, n = 5)

bottom_states <- housing_2024 %>%
  dplyr::count(State, sort = TRUE) %>%
  dplyr::slice_min(n, n = 5)

# merge top and bottom states
housing_compare <- housing_2024 %>%
  filter(State %in% c(top_states$State, bottom_states$State)) %>%
  mutate(StateGroup = case_when(
    State %in% top_states$State ~ "States with the Most Houses",
    State %in% bottom_states$State ~ "States with the Fewest Houses"
  ))%>%
  mutate(StateGroup = factor(StateGroup, levels = c(
    "States with the Most Houses",
    "States with the Fewest Houses"
  )))

#plot top bottom comparison ####
ggplot(housing_compare, aes(x = State, y = Median_Sale_Price_per_k, fill = StateGroup)) +
  geom_boxplot() +
  facet_wrap(~ StateGroup, scales = "free_x") +
  labs(
    title = "Median Sale Price in States with Most vs Least Housing Records",
    subtitle = "The Median Sale Price in States with a Larger Supply of Houses is Significantly Lower\nthan States with a Smaller Supply of Houses",
    x = "State",
    y = "Median Sale Price (in thousands)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("States with the Most Houses" = "skyblue", "States with the Fewest Houses" = "red"))


#hist of median sale price ####
ggplot(housing_2024, aes(x = Median_Sale_Price_per_k)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "k")) +
  labs(
    title = "Distribution of Median Sale Prices (2024)",
    x = "Median Sale Price (in thousands)",
    y = "Count"
  ) +
  theme_minimal()
#needs a subtitle with commentary, just remembered all need a caption with the source named, and we can prob add other elements to this, because otherwise it seems to obvi to me

#boxplot of sale price by affordability tier ####
ggplot(housing_2024, aes(x = Affordability, y = Median_Sale_Price_per_k, fill = Affordability)) +
  geom_boxplot() +
  labs(
    title = "Median Sale Price by Affordability Tier (2024)",
    x = "Affordability Tier",
    y = "Median Sale Price (in thousands)"
  ) +
  theme_minimal()
#no duh, there are more unpurchased expensive houses because people can't afford it 
#not sure how useful this is, but maybe as a starting baseline 

#scatterplot of months supply vs median price ####
ggplot(housing_2024, aes(x = Months_Supply, y = Median_Sale_Price_per_k)) +
  geom_point(aes(color = Affordability), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Housing Supply vs Median Sale Price",
    x = "Months of Supply",
    y = "Median Sale Price (in thousands)"
  ) +
  theme_minimal()
#this is a good one, needs a commentary subtitle and some cleaning 

#correlation heatmap of numeric variables ####
housing_numeric <- housing_2024 %>%
  select(where(is.numeric)) %>%
  drop_na()

cor_matrix <- cor(housing_numeric)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")
#interesting to see positive and inverse relationships between variables 
#did not scan for no relationships

#compare color scale####
# Create a vector of colors for top states (blue shades) and bottom states (red shades)
top_states_colors <- scales::seq_gradient_pal("skyblue", "navy")(seq(0, 1, length.out = length(unique(housing_compare$State[housing_compare$StateGroup == "States with the Most Houses"]))))
names(top_states_colors) <- unique(housing_compare$State[housing_compare$StateGroup == "States with the Most Houses"])

bottom_states_colors <- scales::seq_gradient_pal("pink", "red")(seq(0, 1, length.out = length(unique(housing_compare$State[housing_compare$StateGroup == "States with the Fewest Houses"]))))
names(bottom_states_colors) <- unique(housing_compare$State[housing_compare$StateGroup == "States with the Fewest Houses"])

#last point in line for state label 
label_points <- housing_compare %>%
  group_by(State) %>%
  filter(Months_Supply == max(Months_Supply, na.rm = TRUE)) %>%
  ungroup()

# Combine into one color vector
state_colors <- c(top_states_colors, bottom_states_colors)

#all in gray with faceted compare in color with states labeled ####
ggplot() +
  geom_point(data = housing_2024, aes(x = Months_Supply, y = Median_Sale_Price_per_k),
             color = "gray70", alpha = 0.3, size = 1) +
  geom_path(data = housing_compare,
            aes(x = Months_Supply, y = Median_Sale_Price_per_k, group = State, color = State),
            size = 1, alpha = 0.8) +
  geom_point(data = housing_compare,
             aes(x = Months_Supply, y = Median_Sale_Price_per_k, color = State),
             size = 2, alpha = 0.8) +
  geom_text_repel(data = label_points,
                  aes(x = Months_Supply, y = Median_Sale_Price_per_k, label = State, color = State),
                  size = 3, show.legend = FALSE) +
  facet_wrap(~ StateGroup) +
  scale_color_manual(values = state_colors) +
  labs(
    title = "Housing Supply vs Median Price by State with Grouped Colors",
    subtitle = "Some commentary here",
    x = "Months of Supply",
    y = "Median Sale Price (in thousands)",
    color = "State"
  ) +
  theme_minimal()
#can't read state names, maybe find a way to make it stand out 

#all together in gray with compare in color with states labeled ####
ggplot() +
  geom_point(data = housing_2024, 
             aes(x = Months_Supply, y = Median_Sale_Price_per_k), 
             color = "gray70", alpha = 0.3, size = 1) +
  geom_path(data = housing_compare %>% filter(StateGroup == "States with the Most Houses"),
            aes(x = Months_Supply, y = Median_Sale_Price_per_k, group = State, color = State),
            alpha = 0.8, size = 1) +
  geom_point(data = housing_compare %>% filter(StateGroup == "States with the Most Houses"),
             aes(x = Months_Supply, y = Median_Sale_Price_per_k, color = State),
             alpha = 0.8, size = 2) +
  geom_path(data = housing_compare %>% filter(StateGroup == "States with the Fewest Houses"),
            aes(x = Months_Supply, y = Median_Sale_Price_per_k, group = State, color = State),
            alpha = 0.8, size = 1) +
  geom_point(data = housing_compare %>% filter(StateGroup == "States with the Fewest Houses"),
             aes(x = Months_Supply, y = Median_Sale_Price_per_k, color = State),
             alpha = 0.8, size = 2) +
  scale_color_manual(values = state_colors) +
  labs(
    title = "Housing Supply vs Median Price: All Counties with Highlights",
    subtitle = "Gray points: All counties | Blue shades: States with Most Houses | Red shades: States with Fewest Houses",
    x = "Months of Supply",
    y = "Median Sale Price (in thousands)",
    color = "State"
  ) +
  theme_minimal()
