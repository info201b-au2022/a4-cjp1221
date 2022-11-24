library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_df <- get_data()
#View(incarceration_df)


## Section 2  ---- 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

# Generate female race data
female_prison_data <- incarceration_df %>%
  summarize(
    white_female_pop = sum(white_female_prison_pop, na.rm = T),
    black_female_pop = sum(black_female_prison_pop, na.rm = T),
    total_female_pop = sum(female_prison_pop, na.rm = T),
    prop_black_female = black_female_pop / total_female_pop, 
    prop_white_female = white_female_pop / total_female_pop
  )
#View(female_prison_data)

prop_black_female <- female_prison_data %>% pull(prop_black_female)
prop_white_female <- female_prison_data %>% pull(prop_white_female)
percent_black_female <- "25%" #round down 0.253
percent_white_female <- "40%" #round up 0.396


# Generate urban vs non urban population data
jail_pop <- incarceration_df %>%
  summarize(jail_pop = sum(total_jail_pop, na.rm = T)) %>%
  pull(jail_pop)

urban_jail_pop <- incarceration_df %>%
  filter(urbanicity == "urban") %>%
  summarize(urban_jail_pop = sum(total_jail_pop, na.rm = T)) %>%
  pull(urban_jail_pop)

non_urban_jail_pop <- incarceration_df %>%
  filter(urbanicity != "urban") %>%
  summarize(non_urban_jail_pop = sum(total_jail_pop, na.rm = T)) %>%
  pull(non_urban_jail_pop)

prop_urban <- urban_jail_pop / jail_pop
prop_non_urban <- non_urban_jail_pop / jail_pop
percent_urban <- "34%" # round up from 0.337
percent_non_urban <- "66%" # round down from 0.662


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# Creates a dataframe with the total jail pop by year
get_year_jail_pop <- function() {
  df <- incarceration_df %>%
    group_by(year) %>%
    summarize(total_jail = sum(total_jail_pop, na.rm = T))
  return(df)
}

# Plot the jail population over time as a line graph
plot_jail_pop_for_us <- function() {
  df <- get_year_jail_pop()
  plot <- ggplot(df) +
    geom_col(mapping = aes(x = year, y = total_jail)) +
    labs(
      title = "Jail Population Over Time",
      x = "Years",
      y = "Population",
    )
  return(plot)
}

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# Creates a dataframe with the total jail pop by year for specific states.
# @param states The states for which jail population will be gathered.
get_jail_pop_by_states <- function(states) {
  df <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail = sum(total_jail_pop, na.rm = T))
  return(df)
}
df <- get_jail_pop_by_states(c("WA", "OR", "CA"))
#View(df)


# Plot the jail population over time as a line graph for each state in states
# @param states The states for which jail population will be plotted.
plot_jail_pop_by_states <- function(states) {
  df <- get_jail_pop_by_states(states)
  plot <- df %>%
    ggplot(mapping = aes(x = year, y = total_jail, group = state, color = state)) +
    geom_line() +
    labs(
      title = "Jail Population by State Over Time",
      x = "Years",
      y = "Population",
      color = "State"
    )
  return(plot)
}

plot_jail_pop_by_states(c("WA", "OR", "CA"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#

# Creates a dataframe with white and black female prison populations by county
black_white_jail <- function() {
  df <- incarceration_df %>%
    select(white_female_prison_pop, black_female_prison_pop, county_name) %>%
    filter(!is.na(black_female_prison_pop), !is.na(white_female_prison_pop),) %>%
    group_by(county_name) %>%
    summarize(
      white_pop = sum(white_female_prison_pop),
      black_pop = sum(black_female_prison_pop)
      ) %>%
    filter(black_pop < 15000, white_pop < 15000) # filter outliers
  return(df)
}


# Plots white and black female prison populations by county
black_white_jail_plot <- function() {
  df <- black_white_jail()
  plot <- ggplot(df) +
    geom_point(mapping = aes(x = white_pop, y = black_pop)) +
    labs(
      title = "Black versus White Female Prison Populations by County",
      x = "White Female",
      y = "Black Female"
    )
  return(plot)
}

black_white_jail_plot()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#

# Creates a counties vector that will replace the county_name column 
# in incarceration_df
counties <- incarceration_df$county_name
counties <- tolower(counties)
counties <- str_replace(counties, " county", "")

# Load in a data frame with state names and abreviations for easier merging
state_name <- read.csv("~/info201/data/state_abbr.csv", stringsAsFactors = F) %>%
  mutate(state = tolower(state))

# Creates a dataframe with the proportion of native prisoners in Washington
temp_df <- incarceration_df %>%
  mutate(county_name = counties) %>%
  rename(code = state) %>%
  filter(year == 2018) %>%
  filter(code == "WA") %>%
  left_join(state_name, by="code") %>%
  select(state, county_name, year, native_pop_15to64, total_pop_15to64) %>%
  group_by(year, county_name) %>%
  summarize(total_native = sum(native_pop_15to64, na.rm = T) / sum(total_pop_15to64, na.rm = T))
#View(temp_df)

# Joins the native proportion and county data with a mapping of county 
# boundries in Washington
state_shape <- map_data("county") %>%
  rename(county_name = subregion) %>%
  filter(region == "washington") %>% 
  right_join(temp_df, by="county_name")
#View(state_shape)

# Creates a map of percentages of natives in prison by county
wash_native_pop <- ggplot(state_shape) +
  geom_polygon( 
    mapping = aes(x = long, y= lat, group = group, fill = total_native),
    color = "black",
    size  = .1, 
  ) +
  coord_map() +
  scale_fill_continuous(low = "yellow", high = "red") +
  labs(
    title = "Percentage of Native Americans in Prison by Washington County",
    fill = "Prison Native Percentage",
    x = element_blank(),
    y = element_blank()
  ) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

wash_native_pop



#### Test county jail population change since 2010

# Creates a counties vector that will replace the county_name column 
# in incarceration_df
counties <- incarceration_df$county_name
counties <- tolower(counties)
counties <- str_replace(counties, " county", "")

# Load in a data frame with state names and abreviations for easier merging
state_name <- read.csv("~/info201/data/state_abbr.csv", stringsAsFactors = F) %>%
  mutate(state = tolower(state))

# Creates a dataframe with the proportion of native prisoners in Washington
county_df <- incarceration_df %>%
  mutate(county_name = counties) %>%
  rename(code = state) %>%
  filter(code == "WA", year > 2009) %>%
  left_join(state_name, by="code") %>%
  select(state, county_name, year, total_jail_pop) 
#View(county_df)

ten <- county_df %>%
  filter(year == 2010) %>%
  group_by(county_name) %>%
  summarize(pop_2010 = total_jail_pop)
#View(ten)

eighteen <- county_df %>%
  filter(year == 2018) %>%
  group_by(county_name) %>%
  summarize(pop_2018 = total_jail_pop)
#View(eighteen)

change_since_2010 <- left_join(ten, eighteen, by = "county_name") %>%
  group_by(county_name) %>%
  summarize(change = pop_2010 - pop_2018)
#View(change_since_2010)

county_df <- county_df %>%
  filter(year == 2018) %>%
  left_join(change_since_2010, by = "county_name")
#View(county_df)
  

# Joins the native proportion and county data with a mapping of county 
# boundries in Washington
state_shape_county <- map_data("county") %>%
  rename(county_name = subregion) %>%
  filter(region == "washington") %>% 
  right_join(county_df, by="county_name")
#View(state_shape_county)

# Creates a map of changes in jail population since 2010
county_jail_change <- ggplot(state_shape_county) +
  geom_polygon( 
    mapping = aes(x = long, y= lat, group = group, fill = change),
    color = "black",
    size  = .1, 
  ) +
  coord_map() +
  scale_fill_continuous(low = "yellow", high = "red") +
  labs(
    title = "Change in Jail Populations per Washington County Since 2010",
    fill = "Change in Jail Population",
    x = element_blank(),
    y = element_blank()
  ) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

county_jail_change

