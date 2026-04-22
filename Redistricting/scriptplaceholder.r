library(tidyverse)
library(openintro)
library(alarmdata)
library(redist)
library(ggplot2)
library(ggredist)
library(sf)

plan2333 <- st_read("Redistricting/PLANC2333/PLANC2333.shp")
plans_texas <- alarm_50state_plans('TX')

plan2333_data <- left_join(plan2333 ,plans_texas, by = c("District" = "district"))

plan2333_data <- plan2333_data |> filter(draw == "cd_2020")

plan2333_data |> 
  ggplot() +
  geom_sf(aes(fill = comp_polsby)) +
  labs(fill = "Polsby compactness") +
  scale_fill_gradient(limits = c(0, 1)) +
  #scale_fill_party_c() +
  theme_map()

