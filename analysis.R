# Assignment #3
# Brandon Wong
# November 18, 2020
# INFO 201

# Load the `dplyr` and `tidyr` libraries for data manipulation 
library("dplyr")
library("tidyr")
library("ggplot2")

# Load the Raw Data
# Read the big and messy data from GitHub repository
all_prison_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", 
                            stringsAsFactors = FALSE, header=TRUE)

# Select columns for analysis
my_prison_data <- all_prison_data %>% 
  select(year, state, county_name,
         total_pop_15to64,
         black_pop_15to64, 
         total_prison_pop,
         black_prison_pop, 
         white_prison_pop, 
         latinx_prison_pop,
         total_jail_pop,
         total_jail_from_prison,
         jail_rated_capacity)

# Add column of percent of jail capacity used
my_prison_data <- mutate(my_prison_data,
                         prison_jail_ratio = total_jail_pop/jail_rated_capacity,
                         prison_pop_black_ratio = black_prison_pop/total_prison_pop)


# ===============
# Functions

# Input: State prison data by county
# Output: Name of the county with the highest prison population
county_highest <- function(state_data) {
  state_data %>% 
    filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>% 
    pull(county_name)
}

# Input: State Dataframe, County of Interest, Column of Interest
# Output: The data from the latest year that matches the input query
latest_county_data <- function(state_data, county, pull_var) {
  state_data %>% 
    filter(county_name == county) %>% 
    filter(total_prison_pop > 0) %>% 
    filter(year == max(year)) %>% 
    pull(pull_var)
}

# Input: State Dataframe, County of Interest, Column of Interest
# Output: The data from the earliest year that matches the input query
earliest_county_data <- function(state_data, county, pull_var) {
  state_data %>% 
    filter(county_name == county) %>% 
    filter(black_pop_15to64 > 0, black_prison_pop > 0) %>% 
    filter(year == min(year)) %>% 
    pull(pull_var)
}

# ==================
# California

# Filter the data set to California only
ca_data <- my_prison_data %>% 
  filter(state == "CA") 


# ==================
# Values of Interest

# Value: What county in California had the highest number of Blacks
# in prison in the last year of the data
ca_highest_county <- county_highest(ca_data)

# Value: The number of Blacks reported in this prison facility
# according to the last year
black_ca_latest_prison_pop <- latest_county_data(ca_data, ca_highest_county, "black_prison_pop")

# Value: Total prison count at this facility in the last year
total_ca_latest_prison_pop <- latest_county_data(ca_data, ca_highest_county, "total_prison_pop")

# Value: What was the percent of Blacks living in this county?
black_ca_popul_ratio <-   
  (latest_county_data(ca_data, ca_highest_county, "black_pop_15to64")) /
  (latest_county_data(ca_data, ca_highest_county, "total_pop_15to64"))

# Value: What was the percent Black population change between earliest and
# latest year
black_ca_ratio_popul_change <- 
  ((latest_county_data(ca_data, ca_highest_county, "black_pop_15to64") /
      (latest_county_data(ca_data, ca_highest_county, "total_pop_15to64"))) -
     ((earliest_county_data(ca_data, ca_highest_county, "black_pop_15to64")) /
        (earliest_county_data(ca_data, ca_highest_county, "total_pop_15to64"))))

# Value: What was the percent Black prison population change between the
# earliest and latest year
black_ca_ratio_prison_change <- 
  ((latest_county_data(ca_data, ca_highest_county, "black_prison_pop") /
      (latest_county_data(ca_data, ca_highest_county, "total_prison_pop"))) -
     ((earliest_county_data(ca_data, ca_highest_county, "black_prison_pop")) /
        (earliest_county_data(ca_data, ca_highest_county, "total_prison_pop"))))

# Earliest year of data that can be used to 
# compare Black prison statistics with Black public statistics
min_prison_race_date <- ca_data %>% 
  filter(county_name == ca_highest_county) %>% 
  filter(black_pop_15to64 > 0, black_prison_pop > 0) %>% 
  filter(year == min(year)) %>% 
  pull(year)


# ===================================================
# Time Chart of prison counts
# Top 5 largest prisons in California 

# Filter the top 5 largest prisons by total prisoner population
top_5_prisons <- ca_data %>% 
  filter(black_pop_15to64 > 0, black_prison_pop > 0) %>% 
  filter(year == max(year)) %>%
  arrange(-total_prison_pop) %>% 
  top_n(5, total_prison_pop)

# Assemble the data for the top 5 prisons for plotting
top_5_prison_plot <- ca_data %>%
  filter(black_pop_15to64 > 0, black_prison_pop > 0) %>% 
  filter(county_name == top_5_prisons$county_name[1] |
           county_name == top_5_prisons$county_name[2] |
           county_name == top_5_prisons$county_name[3] |
           county_name == top_5_prisons$county_name[4] |
           county_name == top_5_prisons$county_name[5] )

# Plot the Black prison counts by year of the top 5 largest prisons
top5_prison_plot <- ggplot(data = top_5_prison_plot) +
  geom_point(mapping = aes(x = year, y = black_prison_pop, color = county_name)) +
  geom_smooth(mapping = aes(x = year, y = black_prison_pop, color = county_name),
              method = "loess", se = FALSE, fullrange = TRUE) +
  labs(
    title = "California Black Prisoner Count over Time",
    subtitle = "Top 5 County Jails",
    y = "Prison Population",
    x = "Year") +
  scale_color_discrete(name="County")

top5_prison_plot # display plot

# ==============================================
# Variable Comparison Chart
# Compare the capacity of the Jail to the prisoner counts

# Select variables to plot
# Compare White, Black, Latinx, and Jail Rated Capacity by year
prison_pop_by_year <- ca_data %>%
  filter(county_name == ca_highest_county) %>% 
  filter(year >= min_prison_race_date) %>% 
  select(year, white_prison_pop, black_prison_pop, latinx_prison_pop, jail_rated_capacity) %>% 
  gather(key = Race, value = Ratio, -year) # all columns except `year`

# Plot the income by year where each colored point is based on race
compare_prison_pop <- ggplot(data = prison_pop_by_year) +
  geom_smooth(mapping = aes(x = year, y = Ratio, color = Race),
              method = "loess", se = FALSE, fullrange = TRUE) +
  labs(
    title = "Prison Population vs Jail Capacity over Time",
    subtitle = "Los Angeles County, California",
    y = "Prison Population",
    x = "Year") +
  scale_color_discrete(name="Race &\nCapacity",
                       breaks=c("black_prison_pop", "white_prison_pop", 
                                "latinx_prison_pop","jail_rated_capacity"),
                       labels=c("Black", "White", "Latinx", "Capacity"))

compare_prison_pop # display plot

# ==============================
# Map
# Choropleth (heat) map Jails below, at, over capacity levels

library("ggmap")
library("maps")
library("mapdata")
library("stringr")
library("mapproj")

states <- map_data("state")

ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")


# Define a minimalist theme for maps
blank_theme <- theme(
  axis.line = element_blank(), 
  axis.text = element_blank(), 
  axis.ticks = element_blank(), 
  plot.background = element_blank(),  # remove gray background
  panel.grid = element_blank(), 
  panel.border = element_blank(),
  axis.title = element_blank(),       # remove axis titles
)

# Percent of Blacks in prison in each county

# Load latest prison data with density variable
prison_makeup <- ca_data %>%
  filter(year == 2016) %>% # keep only 2016 data
  mutate(county_name = tolower(county_name)) %>% # replace with lowercase for joining
  select(county_name, prison_pop_black_ratio) 

prison_makeup$county_name <- str_remove(prison_makeup$county_name, " county")
prison_makeup$prison_pop_black_ratio <- prison_makeup$prison_pop_black_ratio * 100
names(prison_makeup) <- c("subregion", "percent") # change header to match map 


# Join prison data to the county shapefile
county_makeup <- inner_join(ca_county, prison_makeup, by = "subregion")

# plot the state 
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_map() + 
  geom_polygon(color = "black", fill = "gray") 

# Draw the map setting the `fill` of each state using its eviction rate
ca_county_makeup_map <- ca_base + 
  geom_polygon(data = county_makeup, aes(fill = percent), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradientn(colours = rev(rainbow(7))) +
  labs(title="Percent of Blacks in the Prison Population",
       subtitle="California, 2016") +
  theme_bw() + blank_theme

ca_county_makeup_map #display map

