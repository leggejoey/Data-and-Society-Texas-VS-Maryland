library(tidyverse)
library(openintro)
library(alarmdata)
library(redist)
library(ggplot2)
library(ggredist)
library(sf)

plan2193 <- st_read("Redistricting/tx_cong_2021/PlanC2193.shp")
plans_texas <- alarm_50state_plans('TX')

plan2193_data <- left_join(plan2193 ,plans_texas, by = c("District" = "district"))

plan2193_data <- plan2193_data |> filter(draw == "cd_2020")

plan2193_data |> 
  ggplot() +
  geom_sf(aes(fill = comp_polsby)) +
  labs(fill = "Polsby compactness") +
  scale_fill_gradient(
    trans = scales::pseudo_log_trans(sigma = 0.01),
    low = "blue", 
    high = "red",
    breaks = c(0,0.05,0.15,0.35,0.65,1),
    labels = c("0: Lowest", "0.05: Very Low", "0.15: Low", "0.35: Low Mid", "0.65: Mid High or Higher", "1: Highest"),
    limits = c(0,1)) +
    theme_map()

MDadopt2022 <- st_read("Redistricting/md_cong_adopted_2022/SB1012-cong-shape-032822.shp")
plans_maryland <- alarm_50state_plans('MD')

MDadopt2022 <- MDadopt2022 |> mutate(DISTRICT = as.numeric(DISTRICT))

plans_maryland <- left_join(MDadopt2022, plans_maryland, by = c("DISTRICT" = "district"))

plans_maryland <- plans_maryland |> filter(draw == "cd_2020")

plans_maryland |> 
  ggplot() +
  geom_sf(aes(fill = comp_polsby)) +
  labs(fill = "Polsby compactness") +
  scale_fill_gradient(
    #trans = scales::pseudo_log_trans(sigma = 0.01),
    low = "blue", 
    high = "red",
    #breaks = c(0,0.05,0.15,0.35,0.65,1),
    labels = c("0: Lowest", "0.25: Mid Low", "0.5: Mid", "0.75: High Mid", "1: Highest"),
    limits = c(0,1)) +
  theme_map()

#tmap example:
  #library(tmap)
  #map_wa <- alarm_50state_map("WA")
  #tmap_mode("view") #once per r session
  #my_map <- map_wa |> 
  #  mutate(prop_hisp = pop_hisp/pop) |>
  #  tm_shape() + 
  #    tm_polygons("prop_hisp", hover = "prop_hisp")
  #tmap_leaflet(my_map) #converts it to leaflet object
  #my_map
