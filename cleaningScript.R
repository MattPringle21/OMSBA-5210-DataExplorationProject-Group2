{
  library(tidyverse)
  library(vtable)
  library(scales)
  library(usmap)
  library(plotly)
  library(sf)
}

prices <- read_csv('Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv')
rents <- read_csv('Metro_zori_uc_sfrcondomfr_sm_month.csv')
vtable(prices, lush = TRUE)
vtable(rents)

#convert date columns into date values with pivot_longer
#first we'll do this to the homes data
prices_long <- prices %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "home_price",
    names_transform = list(date = as.Date)
  )

#now for the rents 
rents_long <- rents %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "rent",
    names_transform = list(date = as.Date)
  )

#join and clean the column names
price_rent <- prices_long %>%
  inner_join(rents_long, by = c("RegionID", "date")) %>% 
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))

#price to rent ratio mutate
price_rent <- price_rent %>% 
  mutate(price_to_rent = home_price/(rent*12), #multiply rent by 12 for annual rents
         RegionName = if_else(RegionName == "Ca-¦on City, CO", "Canon City, CO", RegionName)) #this was a weird RegionName so i fixed it since the coords were wonky    
  

vtable(price_rent)

ggplot(price_rent, aes(x = home_price, y = price_to_rent)) +
  geom_point()

ggplot(price_rent, aes(x = rent, y = price_to_rent)) +
  geom_point()

#get lat and long for cities ---Don't run this because it takes forever use the csv file
 # library(tidygeocoder)
 # region_lookup <- price_rent %>%
 #   distinct(RegionID, RegionName) %>%
 #   rename(RegionName = RegionName) %>%
 #   geocode(address = RegionName, method = "osm", lat = lat, long = lon)
 # vtable(region_lookup, lush = TRUE)

#save csv file with the encode geocodes
#write_csv(region_lookup, "zillow_region_geocodes.csv")

region_lookup <- read_csv("zillow_region_geocodes.csv")

price_rent <- price_rent %>%
  left_join(region_lookup, by = c("RegionID", "RegionName" = "RegionName"))
names(price_rent)

#plot map
select_date <- date('2025-01-31') #enter a date to filter data 

latest_city_us <- price_rent %>%
  group_by(RegionID) %>%
  filter(date == select_date) %>% #filter date variable
  ungroup() %>%
  filter(!is.na(lat), !is.na(lon), !is.na(price_to_rent)) %>%
  usmap_transform(input_names = c("lon", "lat")) %>%   # returns sf with geometry
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()


latest_city_us <- latest_city_us %>%
  mutate(
    ptr_bucket = case_when(
      price_to_rent >= 1  & price_to_rent <= 15 ~ "Better to buy (1–15)",
      price_to_rent >= 16 & price_to_rent <= 20 ~ "Middle (16–20)",
      price_to_rent >= 21                       ~ "Better to rent (21+)"
    )
  )

p <- plot_usmap(regions = "states", fill = "white") +
  geom_point(
    data = latest_city_us %>% filter(!is.na(ptr_bucket), !is.na(ptr_bucket),
                                     RegionType != 'country'),
    aes(
      x = x, y = y,
      size = price_to_rent,
      color = ptr_bucket,
      text = paste0(
        "<b>", RegionName, "</b><br>",
        "Price-to-rent: ", round(price_to_rent, 1), "<br>",
        "Home price: $", comma(round(home_price, 0)), "<br>",
        "Rent (monthly): $", comma(round(rent, 0)), "<br>",
        "Date: ", date
      )
    ),
    alpha = 0.7
  ) +
  scale_color_manual(
    values = c(
      "Better to buy (1–15)" = "red",
      "Middle (16–20)"       = "gray60",
      "Better to rent (21+)" = "blue"
    ),
    name = NULL
  ) +
  scale_size_continuous(name = "Price-to-rent\n(Price / annual rent)") +
  theme_void()

ggplotly(p, tooltip = "text")


##########################################
#Zillow rental Demand (ZORDI)
demand <- read_csv('Metro_zordi_uc_sfrcondomfr_month.csv')
demand_long <- demand %>% 
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "demand_index",
    names_transform = list(date = as.Date)
  ) %>% 
  mutate(RegionName = if_else(RegionName == "Ca-¦on City, CO", "Canon City, CO", RegionName)) %>% 
  left_join(region_lookup, by = c("RegionID", "RegionName" = "RegionName"))
vtable(demand_long, lush = TRUE)

 
#map index
select_date <- date('2025-01-31') #enter a date to filter data 

date_demand <- demand_long %>%
  group_by(RegionID) %>%
  filter(date == select_date) %>% #filter date variable
  ungroup() %>%
  filter(!is.na(lat), !is.na(lon), !is.na(demand_index)) %>%
  usmap_transform(input_names = c("lon", "lat")) %>%   # returns sf with geometry
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

# 4) plot: color = demand index (heat)
p2 <- plot_usmap(regions = "states", fill = "gray98") +
  geom_point(
    data = date_demand %>% filter(RegionType != "country"),
    aes(x = x, y = y, size = demand_index, color = demand_index),
    alpha = 0.6
  ) +
  scale_color_viridis_c(name = "Rental demand\nindex") +
  scale_size_continuous(name = "Rental demand\nindex", range = c(1, 10)) +
  theme_void()

p2

###############################################
#Zillow home market heat index (demand)
demand_home <- read_csv('Metro_market_temp_index_uc_sfrcondo_month.csv')
demand_home_long <- demand_home %>% 
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "date",
    values_to = "demand_home_index",
    names_transform = list(date = as.Date)
  ) %>% 
  mutate(RegionName = if_else(RegionName == "Ca-¦on City, CO", "Canon City, CO", RegionName)) %>% 
  left_join(region_lookup, by = c("RegionID", "RegionName" = "RegionName"))
vtable(demand_home_long, lush = TRUE)

date_demand_home <- demand_home_long %>%
  group_by(RegionID) %>%
  filter(date == select_date) %>% #filter date variable
  ungroup() %>%
  filter(!is.na(lat), !is.na(lon), !is.na(demand_home_index)) %>%
  usmap_transform(input_names = c("lon", "lat")) %>%   # returns sf with geometry
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

# 4) plot: color = demand index (heat)
p3 <- plot_usmap(regions = "states", fill = "gray98") +
  geom_point(
    data = date_demand_home %>% filter(RegionType != "country"),
    aes(x = x, y = y, size = demand_home_index, color = demand_home_index),
    alpha = 0.6
  ) +
  scale_color_viridis_c(name = "Home Market demand\nindex") +
  scale_size_continuous(name = "Home Market demand\nindex", range = c(1, 10)) +
  theme_void()

p3

