library("tidyverse")
library("ggplot2")
library("dplyr")
install.packages("maps")
library("maps")
library("mapproj")



data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

View(data)

# Highest Black Population in A County

highest_black_county <- data %>%
  group_by(county_name) %>%
  summarize(black_jail_pop = max(black_jail_pop)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

# Highest White Population in A County
highest_white_county <- data %>%
  group_by(county_name) %>%
  summarize(white_jail_pop = max(white_jail_pop)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)



# Highest White Population Year
white_highest_year <- data %>%
  group_by(year) %>%
  summarize(white_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_pop == max(white_pop, na.rm = TRUE)) %>%
  pull(year)



# Highest Black Population Year

black_highest_year <- data %>%
  group_by(year) %>%
  summarize(black_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_pop == max(black_pop, na.rm = TRUE)) %>%
  pull(year)




# Largest black jail population in WA countywise
highest_black_wa <- data %>%
  filter(state == "WA") %>%
  group_by(county_name) %>% 
  summarize(black_jail_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

# Largest white jail population in WA countywise
highest_white_wa <- data %>%
  filter(state == "WA") %>%
  group_by(county_name) %>% 
  summarize(white_jail_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(county_name)




# Lowest white jail population in WA countywise
lowest_white_wa <- data %>%
  filter(state == "WA") %>%
  group_by(county_name) %>% 
  summarize(white_jail_pop = min(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_jail_pop == min(white_jail_pop)) %>%
  pull(county_name)

# Lowest black jail population in WA countywise
lowest_black_wa <- data %>%
  filter(state == "WA") %>%
  group_by(county_name) %>% 
  summarize(black_jail_pop = min(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail_pop == min(black_jail_pop)) %>%
  pull(county_name)

# Black Jail population in Snohomish County
black_jail_pop_in_snohomish_county<- data %>%
  filter(county_name == "Snohomish County") %>%
  summarize(black_jail_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(black_jail_pop)
  
# White Jail population in Snohomish County  
white_jail_pop_in_snohomish_county<- data %>%
  filter(county_name == "Snohomish County") %>%
  summarize(white_jail_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(white_jail_pop)



# Time Trend Chart
                    
trend_over_time <- data %>%
  group_by(year) %>%
  summarize(
    total_black_population = sum(black_pop_15to64, na.rm = TRUE),  #Black people between 15-64 yrs old
    total_white_population = sum(white_pop_15to64, na.rm = TRUE), # White people between 15-64 yrs old
    total_latinx_population = sum(latinx_pop_15to64, na.rm = TRUE),  #Latinx people between 15-64 yrs old
    jailed_black_population = sum(black_jail_pop, na.rm = TRUE), # Black people in jail
    jailed_white_population = sum(white_jail_pop, na.rm = TRUE), # White people in jail
    jailed_latinx_population = sum(latinx_jail_pop, na.rm = TRUE), #  Latinx people in jail
) %>% 
  mutate(
      black_proportion = jailed_black_population / total_black_population,
      white_proportion = jailed_white_population / total_white_population,
      latinx_proportion = jailed_latinx_population / total_latinx_population,
)
trend_chart <- ggplot(data = trend_over_time) +
  geom_line(mapping = aes(
    x = year, y = black_proportion,
    color = "Black"
  ), size = 2) +
  geom_line(mapping = aes(
    x = year, y = white_proportion,
    color = "White"
  ), size = 2) +
  geom_line(mapping = aes(
    x = year, y = latinx_proportion,
    color = "Latinx"
  ), size = 2) +
  ggtitle("Proportions Of Jailed People With Respect to Race and Population
Over Years") +
  labs(x = "Years", y = "Proportions Jailed")



                       


# Variable Comparison Chart 
variable_comparsion_data <- data %>%
  group_by(year) %>%
  filter(state == "WA") %>% 
  summarize(
    white_prison_rate = sum(white_prison_pop_rate, na.rm = TRUE),
    black_prison_rate = sum(black_prison_pop_rate, na.rm = TRUE)
)




  
chart<- ggplot(data = variable_comparsion_data) +
  geom_col(mapping = aes(x = year, y = black_prison_rate), 
           color = "red",
           alpha = .3) +
  geom_col(mapping = aes(x = year, y = white_prison_rate), 
           color = "blue",
           alpha = .3) +
  labs(
    title = "The Prison Rate In Washington Comparing White and Blacks", # plot title
    x = "Years", # x-axis label
    y = "Number Of Population", # y-axis label
  ) 
              


    
# The blank theme  
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# The Map

jail_ratio <- data %>%
  select(
    county_name, year, state, fips, black_pop_15to64, white_pop_15to64,
    black_jail_pop, white_jail_pop
  ) %>%
  mutate(
    black_white_jail_ratio = (black_jail_pop / black_pop_15to64) /
      (white_jail_pop / white_pop_15to64) 
)


by_county <- map_data("county") %>%
  unite(polyname,
        region,
        subregion,
        sep = ","
  ) %>%
  left_join(county.fips, by = "polyname")

map_data <- by_county %>%
  left_join(jail_ratio, by = "fips") 

map_chart <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = black_white_jail_ratio
    ),
  ) +
  coord_map() +
  ggtitle("Black to White Jailed Ratio") +
  theme()


#lint("analysis.R")
# style_file("analysis.R")