##------ Wed Dec 20 11:02:52 2023 ------##
# curation of expert opinion data extracted from the excel respondant files
# Ewan McHenry

# forms.direct <- "Data\\Delphi round 1\\response sheets\\"
# extraction.location <- "Data\\Delphi round 1\\"


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(Polychrome)

#  LOAD DATA ----------------------------------------------
load(paste0(extraction.location,"sheets_data.RData"))


# Tree speceis data tree_spp_list -------

tree_spp_list <- list(
  habitat = rep(c("Acidic Upland", "Base-rich Upland", "Wet Upland", "Acidic Lowland", "Base-rich Lowland", "Wet Lowland"), each = 3),
  structure = rep(c("Groves (>70% canopy cover)", "Open wooded habtiats (20–70% canopy cover)", "Glades (<20% canopy cover)"), times = 6)
  # 
  # tree_spp = rep(list(character(100)), 18)
  )

## Acidic Upland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("sessile oak", "downy birch", "hazel", "holly"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("Scots pine", "small-leaved lime"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("bluebell", "wood sorrel", "wood anemone", "honeysuckle", "bugle", "yellow pimpernel", "ivy", "creeping soft grass", "bilberry", "scaly male fern", "male fern", "broad buckler fern", "hard fern"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Acidic Upland" &
                          tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("Mnium horum", "Leucobryum glaucum", "Plagiothecium undulatum", "Hylocomium splendens", "Thuidium tamariscinum"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(NA)
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("sessile oak", "downy birch", "hawthorn", "rowan", "holly", "aspen", "hazel", "goat willow", "grey willow"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("bird cherry", "Scots pine", "smallleaved lime"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Acidic Upland" &
                          tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("pignut", "common dog violet", "betony", "bitter vetch", "honeysuckle", "bluebell", "raspberry", "slender St. John’s wort", "heather", "bilberry", "cowberry", "chickweed wintergreen", "goldenrod", "heath cudweed", "foxglove", "heath speedwell", "common cow-wheat", "climbing corydalis", "greater stitchwort", "red campion", "broom", "wild roses", "brambles", "greater wood rush", "hairy wood rush", "wavy hair grass", "tufted hair grass", "bracken"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("Plagiothecium undulatum", "Rhytidiadelphus loreus", "Hylocomium splendens", "Psuedoscleropodium purum", "Pleurozium schreberi"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Acidic Upland" &
                           tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(NA)
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("hawthorn", "rowan", "holly", "sessile oak", "downy birch", "aspen", "hazel", "goat willow", "grey willow"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Upland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("bird cherry", "juniper", "Scots pine"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Acidic Upland" &
                          tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("heath bedstraw", "tormentil", "heather", "bilberry", "gorse", "wild roses", "heath speedwell", "mat grass", "sheep’s sorrel", "betony", "common bird’s-foot trefoil", "crowberry", "changing forget-me-not", "bitter vetch", "common dog violet", "harebell", "sheep’s fescue", "common bent grass", "sweet vernal grass", "wavy hair grass", "early hair grass"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Acidic Upland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("Psuedoscleropodium purum", "Pleurozium schreberi", "Rhytidiadelphus squarrosus"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Acidic Upland" &
                           tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list("Cladonia lichens")

## Base-rich Upland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("ash", "wych elm", "downy birch", "aspen", "sessile oak", "hazel", "holly"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("pedunculate oak", "small-leaved lime", "silver birch", "yew"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Base-rich Upland" &
                          tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("pignut", "common dog violet", "betony", "bitter vetch", "honeysuckle", "bluebell", "raspberry", "slender St. John’s wort", "heather", "bilberry", "cowberry", "chickweed wintergreen", "goldenrod", "heath cudweed", "foxglove", "heath speedwell", "common cow-wheat", "climbing corydalis", "greater stitchwort", "red campion", "broom", "wild roses", "brambles", "greater wood rush", "hairy wood rush", "wavy hair grass", "tufted hair grass", "bracken"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("Plagiothecium undulatum", "Rhytidiadelphus loreus", "Hylocomium splendens", "Psuedoscleropodium purum", "Pleurozium schreberi"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Base-rich Upland" &
                           tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(NA)
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("ash", "wych elm", "downy birch", "aspen", "rowan", "holly", "hazel", " sessile oak"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("alder (damp areas)", "bird cherry", "wild service", "silver birch", "pedunculate oak", "small-leaved lime", "sycamore", "Scots pine", "crab apple", "juniper", "yew"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Base-rich Upland" &
                          tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("pignut", "common dog violet", "betony", "bitter vetch", "honeysuckle", "bluebell", "raspberry", "slender St. John’s wort", "heather", "bilberry", "cowberry", "chickweed wintergreen", "goldenrod", "heath cudweed", "foxglove", "heath speedwell", "common cow-wheat", "climbing corydalis", "greater stitchwort", "red campion", "broom", "wild roses", "brambles", "greater wood rush", "hairy wood rush", "wavy hair grass", "tufted hair grass", "bracken"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("Plagiothecium undulatum", "Rhytidiadelphus loreus", "Hylocomium splendens", "Psuedoscleropodium purum", "Pleurozium schreberi"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Base-rich Upland" &
                           tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(NA)
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("downy birch", "hawthorn", "blackthorn", "grey willow", "goat willow", "aspen", "rowan",  "holly", "sessile oak", "ash", "elder", "wych elm"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Upland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("alder (damp areas)", "bird cherry", "silver birch", "wild service" , "sycamore", "Scots pine", "crab apple", "juniper"))
tree_spp_list$plant_spp[tree_spp_list$habitat == "Base-rich Upland" &
                          tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("pignut", "common dog violet", "betony", "bitter vetch", "honeysuckle", "bluebell", "raspberry", "slender St. John’s wort", "heather", "bilberry", "cowberry", "chickweed wintergreen", "goldenrod", "heath cudweed", "foxglove", "heath speedwell", "common cow-wheat", "climbing corydalis", "greater stitchwort", "red campion", "broom", "wild roses", "brambles", "greater wood rush", "hairy wood rush", "wavy hair grass", "tufted hair grass", "bracken"))
tree_spp_list$moss_spp[tree_spp_list$habitat == "Base-rich Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("Plagiothecium undulatum", "Rhytidiadelphus loreus", "Hylocomium splendens", "Psuedoscleropodium purum", "Pleurozium schreberi"))
tree_spp_list$lichen_spp[tree_spp_list$habitat == "Base-rich Upland" &
                           tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(NA)

## Wet Upland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Upland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("alder", "downy birch", "sessile oak", "ash", "holly", "wych elm"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Upland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("Scots pine", "crack willow", "white willow (local, on some alluvial or riparian areas)"))
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Upland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("alder", "downy birch", "grey willow", "goat willow", "aspen", "holly", "elder", "sessile oak", "ash"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Upland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("bird cherry", "guelder rose", "bay willow", "eared willow", "alder buckthorn", "purple willow", "dark-leaved willow (local, riparian)"))
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Upland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("grey willow", "goat willow", "holly", "elder", "aspen", "alder", "sessile oak", "downy birch"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Upland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("bay willow", "eared willow", "guelder rose", "alder buckthorn", "bird cherry", "purple willow", "darkleaved willow (local, riparian)"))

## Acidic Lowland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("pedunculate oak", "silver birch", "hazel", "holly"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("sessile oak", "downy birch", "beech", "hornbeam"))
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("pedunculate oak", "silver birch", "downy birch", "hawthorn", "rowan", "holly", "aspen", "hazel", "goat willow", "grey willow", "crab apple", "wild cherry"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("sessile oak, beech, hornbeam"))
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("pedunculate oak", "silver birch", "downy birch", "hawthorn", "rowan", "holly", "aspen", "hazel", "goat willow", "grey willow", "crab apple", "wild cherry", "blackthorn", "elder", "guelder rose"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Acidic Lowland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("sessile oak", "beech", "hornbeam"))

## Base-rich Lowland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("ash", "wych elm", "field maple", "silver birch", "pedunculate oak", "hazel", "yew"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("downy birch (damper)", "beech", "hornbeam", "small-leaved lime", "large-leaved lime", "sessile oak", "Midland hawthorn"))
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("downy birch", "silver birch", "holly", "crab apple", "rowan", "aspen", "grey willow", "pedunculate oak", "ash", "wych elm", "field maple", "hazel", "yew", "spindle, privet", "hawthorn", "guelder rose", "dogwood"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("Midland hawthorn", "wild cherry", "common whitebeam", "hornbeam", "beech", "sycamore", "smallleaved lime", "wild service", "sessile oak", "purging buckthorn", "wild service"))
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("blackthorn", "elder", "silver birch", "downy birch", "goat willow", "holly", "crab apple", "rowan", "aspen", "grey willow", "pedunculate oak", "ash", "wych elm", "field maple", "hazel", "hawthorn", "yew"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Base-rich Lowland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("guelder rose", "privet", "dogwood", "spindle", "wayfaring tree", "purging buckthorn", "wild cherry", "wild service", "common whitebeam", "beech", "sycamore", "common barberry", "hornbeam", "small-leaved lime"))

## Wet Lowland ----
### Groves
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                         tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("alder", "downy birch", "ash", "holly", "crack willow", "pedunculate oak", "wych elm"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                               tree_spp_list$structure == "Groves (>70% canopy cover)" ] <- list(c("white willow (where  inundation frequency is lower)", "black poplar", "small-leaved lime", "hornbeam"))
### Open wooded habtiats
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                         tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("alder", "downy birch", "grey willow", "pedunculate oak", "ash", "goat willow", "aspen", "holly", "elder", "crack willow", "white willow", "osier willow", "almond willow"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                               tree_spp_list$structure == "Open wooded habtiats (20–70% canopy cover)" ] <- list(c("guelder rose", "alder buckthorn", "black poplar", "small-leaved lime", "hornbeam", "purple willow", "bay willow"))
### glades
tree_spp_list$tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                         tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("goat willow", "grey willow", "aspen", "holly", "elder", "osier willow", "almond willow", "crack willow", "white willow", "alder", "downy birch", "pedunculate oak", "ash"))
tree_spp_list$local_tree_spp[tree_spp_list$habitat == "Wet Lowland" &
                               tree_spp_list$structure == "Glades (<20% canopy cover)" ] <- list(c("guelder rose", "alder buckthorn", "purple willow", "bay willow", "black poplar", "smallleaved lime"))

## add length of tree_spp and local_tree_spp ----
tree_spp_list$tree_spp_length <- lengths(tree_spp_list$tree_spp)
tree_spp_list$local_tree_spp_length <- lengths(tree_spp_list$local_tree_spp)


## Create a data frame of respondant data ------------
# Step 1: Extract data from expert.data list
respondent_names <- expert.data$respondant_name %>% unlist
indicator_nums <- expert.data$indicator_num %>% unlist
indicator_names <- expert.data$indicator_name %>% unlist
cert_val_funct <- expert.data$value_confidence_score %>% unlist
measure <- unlist(lapply(expert.data$value.points, `[[`, "measure"))
value <- unlist(lapply(expert.data$value.points, `[[`, "value"))
weight <- expert.data$weight_indicy_score %>% unlist()
cert_weight <- expert.data$weight_indicy_confidence %>% unlist()
vf.sentance <- expert.data$descriptive_sentance %>% unlist()

# Create a data frame by combining the data, scale and add variables
# 
df <- data.frame(
  respondant_name = rep(respondent_names, sapply(expert.data$value.points, nrow)),
  indicator_num = rep(indicator_nums, sapply(expert.data$value.points, nrow)),
  indicator_name = rep(indicator_names, sapply(expert.data$value.points, nrow)),
  
  measure = measure,
  value = value,
  cert_val_funct = rep(cert_val_funct, sapply(expert.data$value.points, nrow)),
  vf.sentance = rep(vf.sentance, sapply(expert.data$value.points, nrow)),
  
  weight = rep(weight, sapply(expert.data$value.points, nrow)),
  cert_weight = rep(cert_weight, sapply(expert.data$value.points, nrow)),
  
  stringsAsFactors = FALSE
) %>% merge(., ind.matcher.df, by.x = "indicator_name", by.y = "indicator_name_for_extraction", all.x = T) %>%   # this is wrong -- its sheet name.. but its fine
  # remove indicator_name.y
  dplyr::select(-indicator_name.y)

  
# working here - need to get sheet name formatted right for the weights plots
df <- df %>% 
  mutate(sheet_name = factor(sheet_name, levels = unique(sheet_name)[ order(unique(indicator_num ))] ))

## scale and augment ----

df.scaled <- df %>% 
  group_by(indicator_name, respondant_name) %>% 
  mutate(value = ((value - min(value)) * 100)/ (max(value) - min(value)) # scale by min and max value for each respondant 
  ) 
df.scaled <- df.scaled %>% 
  group_by(respondant_name) %>% 
  mutate(weight = weight * (100 / max(weight)) # scale by biggest value for each respondant 
  )

which.scaled <- list( vf = paste(df$respondant_name, "-", df$indicator_name)[df.scaled$value != df$value] %>% 
  sort() %>%   unique(),
  wt = paste(df$respondant_name)[df.scaled$weight != df$weight] %>% 
  sort() %>%   unique()
)

df$value <- df.scaled$value %>% round(0)
df$weight <- df.scaled$weight %>% round(0)

weight.rank <-  df[!duplicated(paste0(df$indicator_name, df$respondant_name)),] %>% arrange(respondant_name, weight) %>%
  group_by(respondant_name) %>%
  mutate(weight_rank = rank(-weight)) %>% 
  dplyr::select(c("indicator_name", "respondant_name", "weight_rank"))

df <- df %>% right_join(weight.rank, by = c("indicator_name", "respondant_name"))

df$value.dec = df$value/100 # a decimal version of value to work in the later binomial gams


## version with sinlge entry for each indicator:respondant ----
just.one.df <- df[!duplicated(paste0(df$sheet_name, df$respondant_name)),
                  c("indicator_name", "respondant_name", "cert_val_funct", "cert_weight", "sheet_name", "weight","indicator_num")] %>% 
  mutate(respondant_name = as.factor(respondant_name),
         indicator_name = as.factor(indicator_name),
         sheet_name = as.factor(sheet_name)
  )
just.one.df$mean_repond_cert_vf <- ave(just.one.df$cert_val_funct, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))
just.one.df$mean_repond_cert_wt <- ave(just.one.df$cert_weight, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))

# add reverce rank of wieghts by each respondant to just.one.df
just.one.df <- just.one.df %>% 
  group_by(respondant_name) %>% 
  mutate(rank_weight = rank(-weight, ties.method = "min")) %>% 
  ungroup()


#-----------------------------------------------------

# completed df ----
has_completed <- data.frame(respondent_names = respondent_names,
                            indicator_name = indicator_names,
                            wt = !is.na(weight),
                            wt_cert = !is.na(cert_weight),
                            vf = sapply(expert.data$value.points, nrow)>1,
                            vf_cert = !is.na(as.numeric(cert_val_funct))
) %>% 
  left_join(., ind.matcher.df[, c("sheet_name", "indicator_name_for_extraction")], by = c("indicator_name" =  "indicator_name_for_extraction"))

# create df of group by(indicator, respondant) %>% sum(completed) for each
completed_summary <- has_completed %>% 
  group_by(respondent_names) %>% 
  summarize(vf = sum(vf),
            vf_cert = sum(vf_cert),
            wt = sum(wt),
            wt_cert = sum(wt_cert))

respondants_with_something <- completed_summary$respondent_names[
  completed_summary %>%
    select_if(is.numeric) %>%
    rowSums()>0] %>% 
  as.factor()


respondant_colours <- glasbey.colors(nlevels(respondants_with_something)+1)[-1] %>% as.character() %>% 
  setNames(., respondants_with_something)


# test and throw up error issue ---------
if (length(unique(paste0(df$sheet_name,df$indicator_num )))!=length(unique(df$sheet_name)) ){
  print("Warning, someones response sheet has slipped into the wrong orger! -- this WILL cause issues")
}

save(df,
     which.scaled,
     just.one.df, 
     ind.matcher.df,
     has_completed,
     completed_summary, 
     respondants_with_something,
     respondant_colours, tree_spp_list, 
     file = paste0(extraction.location,"curated.RData"))
