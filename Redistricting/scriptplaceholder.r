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
    low = "blue", 
    high = "red",
    #breaks = c(0,0.05,0.15,0.35,0.65,1),
    labels = c("0: Lowest", "0.25: Mid Low", "0.5: Mid", "0.75: High Mid", "1: Highest"),
    limits = c(0,1)) +
  theme_map()

new_texas_map <- redist_map(plan2193_data, ndists = 38, pop_tol = 0.03)

#original map
new_texas_map |> 
  ggplot() +
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

#breaking up the map geometry for an easier calculation for my computer
texas_q1 <- new_texas_map |> filter(District %in% c(13,19,11,25,12,26,31,24))
texas_q2 <- new_texas_map |> filter(District %in% c(1,3,4,5,6,30,32,33,17))
texas_q3 <- new_texas_map |> filter(District %in% c(38,29,7,9,18,36,14,22,2,8))
texas_q4 <- new_texas_map |> filter(District %in% c(10,35,27,37,31,21,20))
texas_q5 <- new_texas_map |> filter(District %in% c(15,34,28,16,23))

texas_q1 |>
  ggplot() + 
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

texas_q2 |>
  ggplot() + 
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

texas_q3 |>
  ggplot() + 
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

texas_q4 |>
  ggplot() + 
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

texas_q5 |>
  ggplot() + 
  geom_sf(aes(fill = ndv / (nrv + ndv)), linetype = "dashed") +
  scale_fill_party_c() +
  theme_map()

#new map still broken :\
new_texas_map_sim <- redist_smc(texas_q1, nsims = 5, ncores = 5)

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
