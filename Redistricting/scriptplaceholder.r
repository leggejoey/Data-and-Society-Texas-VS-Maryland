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
  scale_fill_gradient(limits = c(0, 1)) +
  #scale_fill_party_c() +
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
  scale_fill_gradient(limits = c(0, 1)) +
  #scale_fill_party_c() +
  theme_map()
