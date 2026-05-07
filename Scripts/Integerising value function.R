# integerising value function
library(tidyverse)

source("Scripts\\load_weca_tables.R")
ground_flora_lookup <- data.frame(per_spp = 0:100) %>% 
  mutate(value = approx(
    x = ground_flora_lookup$per_spp,
    y = ground_flora_lookup$value,
    xout = per_spp
  )$y)

write.csv(ground_flora_lookup, "outputs\\lookup_tables\\Ground flora_vf_lookup - EMcH MU edit01_2026-05-06_integerised.csv", row.names = F)