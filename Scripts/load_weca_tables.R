# Load the lookup tables
age_lookup <- read.csv("outputs\\lookup_tables\\Tree age distribution_vf_lookup.csv")
tree_spp_lookup <- read.csv("outputs\\lookup_tables\\N tree & shrub spp._vf_lookup.csv")
antropogenic_damage_lookup <- read.csv("outputs\\lookup_tables\\Anthropogenic damage_vf_lookup_domin.csv")
deadwood_lookup <- read.csv("outputs\\lookup_tables\\Deadwood_vf_lookup.csv")
ground_flora_lookup <- read.csv("outputs\\lookup_tables\\Ground flora_vf_lookup.csv")
Horizontal_complexity_lookup <- read.csv("outputs\\lookup_tables\\Horizontal complexity_vf_lookup.csv")
invasives_lookup <- read.csv("outputs\\lookup_tables\\Invasive plants % cover_vf_lookup_domin.csv")
microhabitats_lookup <- read.csv("outputs\\lookup_tables\\Microhabitats_vf_lookup.csv")
native_canopy_lookup <- read.csv("outputs\\lookup_tables\\Native canopy percentage _vf_lookup.csv")
regen_lookup <- read.csv("outputs\\lookup_tables\\Regen_vf_lookup.csv")
tree_health_lookup <- read.csv("outputs\\lookup_tables\\Tree health_vf_lookup.csv")
vertical_structure_lookup <- read.csv("outputs\\lookup_tables\\Vertical structure_vf_lookup.csv")
veteran_trees_lookup <- read.csv("outputs\\lookup_tables\\Veteran trees_vf_lookup.csv")
Herbivore_impact_lookup <- read.csv("outputs\\lookup_tables\\Herbivore damage_vf_lookup.csv")%>%
  mutate(
    herbivore_impact_class = case_when(
      str_detect(Damage.class..potential.12.month.growth.removed, "^Negligable") ~ "Negligable damage",
      str_detect(Damage.class..potential.12.month.growth.removed, "^Low damage") ~ "Low damage",
      str_detect(Damage.class..potential.12.month.growth.removed, "^Moderate") ~ "Moderate damage",
      str_detect(Damage.class..potential.12.month.growth.removed, "^High damage") ~ "High damage",
      str_detect(Damage.class..potential.12.month.growth.removed, "^Very high") ~ "Very high damage",
      TRUE ~ NA_character_
    )
  ) %>%
  select(herbivore_impact_class, value) %>% 
  mutate(herbivore_impact_class = factor(herbivore_impact_class, 
                                         levels = c("Negligable damage", "Low damage", "Moderate damage", "High damage", "Very high damage")))

weights_lookup <- read.csv("outputs\\lookup_tables\\weights_lookup.csv")

#load domin range table
domin_transformer <- read.csv("Data\\domin transformer.csv") %>% 
  mutate(domin = as.character(antropogenic_damage_lookup$domin))
domin_transformer$mid = domin_transformer$min + (domin_transformer$max - domin_transformer$min) / 2
