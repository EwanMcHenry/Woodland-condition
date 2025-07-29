# zone 1 and 21 harrisons and beechy wood and ladypark
#ladypark for starter

# Libraries ----
library(readxl)
library(tidyverse)
library(lubridate)
library(vegan)

# Config
extract_plot_survey_data <- function(
    excel_sheet_folderpath = "Data\\Field Test\\Ladypark Wood, Wye valley - Non-intervention 70yrs broadleaf SSSI ASNW\\",
    excel_sheet_filename = "WCA - Lady park wood - Field test - 12.06.2025.xlsx",
    write_plot.csv = F,
    verticle_dont_contribute = c("Absent", "< 4%", "4 - 10%"),
    n.deadwood_types = 4, # total number of deadwood categories - hard coded
    last.micohabitat.listed = "Heavy resinosis", # last microhabitat listed in the sheet, used to find the last row of microhabitats
    list_appropriate_ground_flora = read.csv("Data\\Appropriate_groundflora_spp_list.csv")$species ,
    list_appropriate_tree_spp = read.csv("Data\\Appropriate_tree_spp_list.csv")$species ,
    list_high_threat_invasives = read.csv("Data\\high_threat_invasives_list.csv")$species, # list of high threat invasive species, used to check if any are present in the plot data
    herbivory_category_names = c("Low damage", "Moderate damage", "High damage", "Very high damage", "Extreme damage"), # names of the herbivory categories
    domin.absent = "0% Absent", # value of absent domin, used to replace NAs in tree species counts
    avt_search_radius_m = 10
){
  # load data ----
  excel_sheet_path <- paste0(excel_sheet_folderpath, excel_sheet_filename)
  sheets <- excel_sheets(excel_sheet_path) 
  
  # create a data frame for each plot that has a number
  plot_sheets <- sheets[grepl("Plot", sheets)] %>% 
    # remove that doesnt have number in it
    .[!grepl("Blank", .)]
  
  # Read data from each plot sheet into a list, no header
  plot_data <- lapply(plot_sheets, function(sheet) {
    read_excel(excel_sheet_path, sheet = sheet, col_names = FALSE)
  })
  
  # Loop over all plots wanted ----
  these.plots <- 1:length(plot_sheets) # all plots
  survey_data <- list() # create a list to store the survey data for each plot
  for(this.plot in these.plots){
    # write plot data if wanted ---- 
    if(write_plot.csv == T){
      write.csv(plot_data[[this.plot]], paste0(excel_sheet_folderpath, "plot", this.plot, ".csv"), row.names = FALSE)
    }
    
    # survey info ----
    wood_name = plot_data[[this.plot]][2,2][[1]]
    date_surveyed = dmy(plot_data[[this.plot]][1, 4][[1]])
    zone = plot_data[[this.plot]][2, 4][[1]]
    plot = plot_data[[this.plot]][2, 6][[1]]
    surveyor = plot_data[[this.plot]][1, 6][[1]]
    time_start = plot_data[[this.plot]][1, 8] %>% as.numeric() 
    time_end = plot_data[[this.plot]][2, 8]%>% as.numeric() 
    time_break = plot_data[[this.plot]][3, 8] %>% as.numeric() # if NA make 0
    if (is.na(time_break)) {
      time_break = 0
    } 
    time_taken = time_end - time_start - time_break
    
    # positions ----
    position.tree_age.first_spp <- which(plot_data[[this.plot]] == "1a. Tree and Shrub Composition", arr.ind = TRUE) %>% 
      as.numeric() + c(3, 0) 
    position.invasive.first_spp <- which(plot_data[[this.plot]] == "4. Invasive non-native species", arr.ind = TRUE) %>% 
      as.numeric() + c(2, 0) 
    position.invasive.cover <- position.invasive.first_spp + c(0, 5)
    position.vertical_structure.first_strata <- which(plot_data[[this.plot]] == "5. Vertical Structure", arr.ind = TRUE) %>% 
      as.numeric() + c(3, 0) 
    position.nativeness.measure <- which(plot_data[[this.plot]] == "6. Canopy Nativeness", arr.ind = TRUE) %>% 
      as.numeric() + c(3, 1) 
    position.tree_health.dieback.measure <- which(plot_data[[this.plot]] == "7. Tree Health", arr.ind = TRUE) %>% 
      as.numeric() + c(3, 1) 
    position.tree_health.dieback.affected_species <- position.tree_health.dieback.measure + c(0, 5)
    position.tree_health.mortality.measure <- position.tree_health.dieback.measure + c(2, 0)
    position.tree_health.mortality.affected_species <- position.tree_health.mortality.measure + c(0, 5)
    position.anthropogenic_damage.measure <- which(plot_data[[this.plot]] == "8. Anthropogenic Damage", arr.ind = TRUE) %>% 
      as.numeric() + c(3, 1)
    position.deadwood.first_type <- which(plot_data[[this.plot]] == "9. Deacaying Wood Habitats", arr.ind = TRUE) %>% 
      as.numeric() + c(3,0 )
    position.microhabitat.first_type <- which(plot_data[[this.plot]] == "10. Microhabitats (presence)", arr.ind = TRUE) %>% 
      as.numeric() + c(2,0 )
    position.microhabitat.last_type <- c(grep(last.micohabitat.listed, plot_data[[this.plot]][[position.microhabitat.first_type[2]]]), position.microhabitat.first_type[2])
    position.groundflora.first_species <- which(plot_data[[this.plot]] == "11. Flora", arr.ind = TRUE) %>%
      as.numeric() + c(3,0 )
    position.herbivore_impact.first_pal <- which(plot_data[[this.plot]] == "12. Herbivore impact", arr.ind = TRUE) %>% 
      as.numeric() + c(4,0 )
    position.horizontal_complexity.first_stop_number <- which(plot_data[[this.plot]] == "13. Horizontal complexity", arr.ind = TRUE) %>% 
      as.numeric() + c(2,0 )
    position.avt.number <- which(plot_data[[this.plot]] == "Total veterans", arr.ind = TRUE) %>% 
      as.numeric() + c(1,0 )
    position.supp.first_tree_spp <- which(plot_data[[this.plot]] == "SUPPLEMENTARY PLOT", arr.ind = TRUE) %>% 
      as.numeric() + c(5,0 )
    
    # this.position <- position.invasive.cover # this is the position of the first invasive species in the plot data
    # plot_data[[this.plot]][this.position[1], this.position[2]]
    
    # tree species and age ----
    ## dataframe
    ### main plot
    n.tree_spp.listed <- which(is.na(plot_data[[this.plot]][position.tree_age.first_spp[1]:(position.tree_age.first_spp[1]+30), position.tree_age.first_spp[2]]))[1]-1 
    if(n.tree_spp.listed == 0){
      rows4trees = NA # if no tree species listed, set to 1
    }else{rows4trees <- position.tree_age.first_spp[1]:(position.tree_age.first_spp[1]+n.tree_spp.listed-1)
    }
    trees_main_df <- tibble(
      tree_species = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2]][[1]],
      young = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 1][[1]],
      semi_mature = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 2][[1]],
      early_mature = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 3][[1]],
      mature = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 4][[1]],
      seedlings = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 5][[1]],
      saplings = plot_data[[this.plot]][rows4trees, position.tree_age.first_spp[2] + 6][[1]]
    ) %>%
      mutate(across(c(young, semi_mature, early_mature, mature), ~ ifelse(is.na(.), 0, .))) %>% 
      mutate(across(c(young, semi_mature, early_mature, mature), ~ ifelse(is.na(.), 0, .))) %>% 
      mutate(across(c(seedlings, saplings), ~ ifelse(is.na(.), domin.absent, .))) %>%
      mutate(across(c(young, semi_mature, early_mature, mature), as.numeric)) %>%
      # remvoe rows with all 0s
      filter(!(young == 0 & semi_mature == 0 & early_mature == 0 & mature == 0 & seedlings == domin.absent & saplings == domin.absent)) %>% 
      mutate(spp_total_trees = young + semi_mature + early_mature + mature) 
    ### supplementary plot
    n.supp.tree_spp.listed <- which(is.na(plot_data[[this.plot]][position.supp.first_tree_spp[1]:(position.supp.first_tree_spp[1]+30), position.supp.first_tree_spp[2]]))[1]-1
    rows4supp_trees <- position.supp.first_tree_spp[1]:(position.supp.first_tree_spp[1]+n.supp.tree_spp.listed-1)
    if(n.supp.tree_spp.listed == 0){
      rows4supp_trees = NA}else{
        rows4supp_trees <- position.supp.first_tree_spp[1]:(position.supp.first_tree_spp[1]+n.supp.tree_spp.listed-1)
      }
    
    trees_supp_df <- tibble(
      tree_species = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2]][[1]],
      young = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 1][[1]],
      semi_mature = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 2][[1]],
      early_mature = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 3][[1]],
      mature = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 4][[1]],
      seedlings = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 5][[1]],
      saplings = plot_data[[this.plot]][rows4supp_trees, position.supp.first_tree_spp[2] + 6][[1]]
    ) %>%
      mutate(across(c(young, semi_mature, early_mature, mature), ~ ifelse(is.na(.), 0, .))) %>% 
      mutate(across(c(young, semi_mature, early_mature, mature), ~ ifelse(is.na(.), 0, .))) %>% 
      mutate(across(c(seedlings, saplings), ~ ifelse(is.na(.), domin.absent, .))) %>%
      mutate(across(c(young, semi_mature, early_mature, mature), as.numeric)) %>%
      # remvoe rows with all 0s
      filter(!(young == 0 & semi_mature == 0 & early_mature == 0 & mature == 0 & seedlings == domin.absent & saplings == domin.absent)) %>% 
      mutate(spp_total_trees = young + semi_mature + early_mature + mature)
    
    ## tree species present by name and count
    trees_main_spp_present <- trees_main_df %>%
      filter(spp_total_trees > 0) %>%
      select(tree_species, spp_total_trees)
    trees_supp_spp_present <- trees_supp_df %>%
      filter(spp_total_trees > 0) %>%
      select(tree_species, spp_total_trees)
    
    ## Calculate indicator measurments
    ### n species
    ind.tree_spp_n.main <- sum(trees_main_df$spp_total_trees>0) 
    ind.tree_spp_n.supp <- sum(trees_supp_df$spp_total_trees>0)
    ### shannon diversity index for each plot - tree species
    ind.tree_spp_shannon.main <- vegan::diversity(trees_main_df$spp_total_trees, index = "shannon")
    ind.tree_spp_shannon.supp <- vegan::diversity(trees_supp_df$spp_total_trees, index = "shannon")
    ### proportion of appropriate tree species
    ind.tree_spp_prop_appropriate.main <- sum(tolower(trees_main_df$tree_species) %in% tolower(list_appropriate_tree_spp)) / length(list_appropriate_tree_spp)
    ind.tree_spp_prop_appropriate.supp <- sum(tolower(trees_supp_df$tree_species) %in% tolower(list_appropriate_tree_spp)) / length(list_appropriate_tree_spp)
    
    ### age classes
    main_ages <- trees_main_df %>% 
      select(young, semi_mature, early_mature, mature) %>%
      summarise(across(everything(), ~ sum(.)))
    supp_ages <- trees_supp_df %>%
      select(young, semi_mature, early_mature, mature) %>%
      summarise(across(everything(), ~ sum(.)))
    ### n age classes
    ind.age_classes_n.main <- sum(main_ages>0) 
    ind.age_classes_n.supp <- sum(supp_ages>0)
    ### shannon diversity index - age classes
    ind.tree_age_classes_shannon.main <- vegan::diversity(main_ages, index = "shannon")
    ind.tree_age_classes_shannon.supp <- vegan::diversity(supp_ages, index = "shannon")
    
    # regen ----
    
    # presence of differertn regen classes for spp and row for any species that has regen
    regen_species_classes.main <- trees_main_df %>%
      select(tree_species, young, seedlings, saplings) %>%
      mutate(across(c(young, seedlings, saplings), ~ .x != 0 & .x != domin.absent)) %>%  # Convert counts to TRUE/FALSE
      bind_rows(
        summarise(
          .,
          tree_species = "Any species",
          across(c(young, seedlings, saplings), ~ any(.x, na.rm = TRUE))
        )
      )
    
    regen_species_classes.supp <- trees_supp_df %>%
      select(tree_species, young, seedlings, saplings) %>%
      mutate(across(c(young, seedlings, saplings), ~ .x != 0 & .x != domin.absent)) %>%  # Convert counts to TRUE/FALSE
      bind_rows(
        summarise(
          .,
          tree_species = "Any species",
          across(c(young, seedlings, saplings), ~ any(.x, na.rm = TRUE))
        )
      )
    # sum of any species classes
    ind.regen_classes_n.main <- sum(regen_species_classes.main %>% filter(tree_species == "Any species") %>%  select("young", "seedlings", "saplings") )
    ind.regen_classes_n.supp <-  sum(regen_species_classes.supp %>% filter(tree_species == "Any species") %>%  select("young", "seedlings", "saplings") )
    
    # invasive species ----
    n.invasive_spp.listed <- which(is.na(plot_data[[this.plot]][position.invasive.first_spp[1]:(position.invasive.first_spp[1]+30), position.invasive.first_spp[2]]))[1]-1
    if(n.invasive_spp.listed == 0){
      rows4invasive = 0 # if no invasive species listed, set to 1
    }else{
      rows4invasive <- position.invasive.first_spp[1]:(position.invasive.first_spp[1]+n.invasive_spp.listed-1)
    }
    invasive_spp <- tibble(
      invasive_species = plot_data[[this.plot]][rows4invasive, position.invasive.first_spp[2]],
      cover = plot_data[[this.plot]][rows4invasive, position.invasive.first_spp[2] + 1][[1]]
    ) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
      mutate(across(c(cover), as.numeric)) %>%
      filter(invasive_species != "")
    if(n.invasive_spp.listed > 0){
      ind.invasive_list = plot_data[[this.plot]][rows4invasive, position.invasive.first_spp[2]]
    }else{
      ind.invasive_list = NA
    }
    
    ind.invasive.total_domin <- plot_data[[this.plot]][position.invasive.cover[1], position.invasive.cover[2]][[1]]
    ind.invasive.high_threat_present <- any(tolower(invasive_spp$invasive_species) %in% tolower(list_high_threat_invasives)) # check if any high threat invasive species are present
    
    
    
    # vertical structure ----
    ## df of cover at each vertical stratum
    n_strata = which(is.na(plot_data[[this.plot]][position.vertical_structure.first_strata[1]:(position.vertical_structure.first_strata[1]+30), position.vertical_structure.first_strata[2]]))[1]-1
    strata_rows <- position.vertical_structure.first_strata[1]:(position.vertical_structure.first_strata[1]+n_strata-1)
    
    vert_structure <- data.frame(
      stratum = plot_data[[this.plot]][strata_rows, position.vertical_structure.first_strata[2]][[1]],
      cover = plot_data[[this.plot]][strata_rows, position.vertical_structure.first_strata[2] + 1][[1]])
    ## N verticle strata past threshold in cover
    # which vert_structure$cover matching string in verticle_dont_contribute
    ## verticle_dont_contribute elements only have part of the string, so we need to check if any of the elements in verticle_dont_contribute are in the cover
    vert_structures_counted = vert_structure$stratum[!grepl(paste(verticle_dont_contribute, collapse = "|"), vert_structure$cover)] # find which cover values match the dont contribute strings
    ind.vert_structure_n <- sum(!is.na(vert_structures_counted))
    
    # nativeness ----
    ind.nativeness.measure <- plot_data[[this.plot]][position.nativeness.measure[1], position.nativeness.measure[2]][[1]] %>% as.numeric()*100
    
    
    # tree health ----
    ## dieback
    ind.tree_health.dieback.measure <- plot_data[[this.plot]][position.tree_health.dieback.measure[1], position.tree_health.dieback.measure[2]][[1]] %>% as.numeric()
    ind.tree_health.mortality.measure <- plot_data[[this.plot]][position.tree_health.mortality.measure[1], position.tree_health.mortality.measure[2]][[1]] %>% as.numeric()
    ind.tree_health.max.dieback_mortality <- max(ind.tree_health.dieback.measure, ind.tree_health.mortality.measure, na.rm = TRUE) # max of dieback and mortality measures
    
    ## affected species
    # TODO
    
    # anthropogenic damage ----
    ind.anthropogenic_damage.measure <- plot_data[[this.plot]][position.anthropogenic_damage.measure[1], position.anthropogenic_damage.measure[2]]
    
    # deadwood ----
    deadwood_rows <- position.deadwood.first_type[1]:(position.deadwood.first_type[1]+n.deadwood_types-1)
    ## df of deadwood types, plot quarters and counts
    deadwood_df <- data.frame(
      deadwood_type = plot_data[[this.plot]][deadwood_rows, position.deadwood.first_type[2]],
      q1 = plot_data[[this.plot]][deadwood_rows, position.deadwood.first_type[2] + 1] ,
      q2 = plot_data[[this.plot]][deadwood_rows, position.deadwood.first_type[2] + 2],
      q3 = plot_data[[this.plot]][deadwood_rows, position.deadwood.first_type[2] + 3],
      q4 = plot_data[[this.plot]][deadwood_rows, position.deadwood.first_type[2] + 4]
    ) 
    names(deadwood_df) <- c("deadwood_type", "Q1", "Q2", "Q3", "Q4")
    # not actually T/F data, needs coverted
    ## test if q1, q2, q3, q4 == "TRUE"
    deadwood_df <- deadwood_df %>%
      mutate(across(c(Q1, Q2, Q3, Q4), ~ ifelse(. == "TRUE", TRUE, FALSE))) %>%
      mutate(across(c(Q1, Q2, Q3, Q4), as.logical)) # convert to logical
    
    ind.deadwood_type_quaters <- sum(c(deadwood_df$Q1, deadwood_df$Q2, deadwood_df$Q3, deadwood_df$Q4) %>% as.numeric()) # sum of all deadwood types across all quarters
    
    # microhabitats ----
    microhab_rows <- position.microhabitat.first_type[1]:position.microhabitat.last_type[1]
    microhabs_all <- plot_data[[this.plot]][microhab_rows, position.microhabitat.first_type[2]] %>% 
      unlist() %>% 
      as.character() %>% 
      na.omit() # remove NAs
    microhab_logical <- plot_data[[this.plot]][microhab_rows, position.microhabitat.first_type[2] + 1] %>% unname() %>%  unlist()
    microhabitats_present <- microhabs_all[!is.na(microhab_logical) & microhab_logical== "TRUE"]
    ind.microhabitats_n <- length(microhabitats_present)
    ind.microhabitats_prop <- ind.microhabitats_n / length(microhabs_all)
    
    # ground flora ----
    n.groundflora_spp.listed <- which(is.na(plot_data[[this.plot]][position.groundflora.first_species[1]:(position.groundflora.first_species[1]+50), position.groundflora.first_species[2]]))[1]-1
    all_groundflora_rows <- position.groundflora.first_species[1]:(position.groundflora.first_species[1]+n.groundflora_spp.listed-1)
    ground_flora_df <- data.frame(
      groundflora_species = plot_data[[this.plot]][all_groundflora_rows, position.groundflora.first_species[2]][[1]],
      cover = plot_data[[this.plot]][all_groundflora_rows, position.groundflora.first_species[2] + 1][[1]]
    ) %>% 
      filter(!is.na(cover) & !is.na(groundflora_species) & groundflora_species != "" )
    ind.groundflora_n <- nrow(ground_flora_df) # number of ground flora species
    ind.groundflora_prop_listed <- sum(
      tolower(ground_flora_df$groundflora_species) %in% tolower(list_appropriate_ground_flora)
    ) / length(list_appropriate_ground_flora)  
    ground_flora.not_counting <- ground_flora_df$groundflora_species[!tolower(ground_flora_df$groundflora_species) %in% tolower(list_appropriate_ground_flora)] # ground flora species not in the appropriate list
    
    # herbivore impact ----
    n.herbivore_pal.listed <- which(is.na(plot_data[[this.plot]][position.herbivore_impact.first_pal[1]:(position.herbivore_impact.first_pal[1]+50), position.herbivore_impact.first_pal[2]]))[1]-1
    herbivore_pal_rows <- position.herbivore_impact.first_pal[1]:(position.herbivore_impact.first_pal[1]+n.herbivore_pal.listed-1)
    herbivore_pal_cols <- which(!is.na(unlist(plot_data[[this.plot]][herbivore_pal_rows[1]-1, ])))
    herbivore_amounts <- plot_data[[this.plot]][herbivore_pal_rows[1]-1, (position.herbivore_impact.first_pal[2] + 1):(position.herbivore_impact.first_pal[2] + 5)] %>% 
      unlist() %>% 
      as.character() %>% 
      na.omit() # remove NAs
    
    
    herbivore_impact_df <- data.frame(
      palatability = plot_data[[this.plot]][herbivore_pal_rows, position.herbivore_impact.first_pal[2]][[1]],
      per_removed = NA)
    herbivore_impact_df$per_removed[1] <- herbivore_amounts[ plot_data[[this.plot]][herbivore_pal_rows[1], herbivore_pal_cols[-1]] %>% unlist() == "TRUE" ]
    herbivore_impact_df$per_removed[2] <- herbivore_amounts[ plot_data[[this.plot]][herbivore_pal_rows[2], herbivore_pal_cols[-1]] %>% unlist() == "TRUE" ]
    
    per.palatable_removed <-  herbivore_impact_df$per_removed[herbivore_impact_df$palatability == "Palatatble species"] %>% 
      as.character() %>% 
      na.omit() # remove NAs
    per.unpalatable_removed <-  herbivore_impact_df$per_removed[herbivore_impact_df$palatability == "Un-palatatble species"] %>% 
      as.character() %>% 
      na.omit() # remove NAs
    
    low_impact <- c("0%", "<25%") #
    moderate_impact <- c("25-75%") 
    high_impact <- c("75-90%", ">90%") 
    
    ind.herbivore_impact.class <- ifelse(per.palatable_removed %in% low_impact, 
                                         herbivory_category_names[1],
                                         ifelse(per.palatable_removed %in% moderate_impact, 
                                                herbivory_category_names[2],
                                                ifelse( per.palatable_removed %in% high_impact & per.unpalatable_removed %in% low_impact, 
                                                        herbivory_category_names[3],
                                                        ifelse(per.palatable_removed %in% high_impact & per.unpalatable_removed %in% moderate_impact, 
                                                               herbivory_category_names[4],
                                                               ifelse(per.palatable_removed %in% high_impact & per.unpalatable_removed %in% high_impact, 
                                                                      herbivory_category_names[5], 
                                                                      NA)))))
    
    # horizontal complexity ----
    n.horizontal_stops <- which(is.na(plot_data[[this.plot]][position.horizontal_complexity.first_stop_number[1]:(position.horizontal_complexity.first_stop_number[1]+50), position.horizontal_complexity.first_stop_number[2]]))[1]-1
    horizontal_stops_rows <- position.horizontal_complexity.first_stop_number[1]:(position.horizontal_complexity.first_stop_number[1]+n.horizontal_stops-1)
    top_heights_recoreded <- plot_data[[this.plot]][horizontal_stops_rows, position.horizontal_complexity.first_stop_number[2]+1][[1]]
    top_heights_df <- data.frame(
      top_height = unique(top_heights_recoreded) %>% sort(),
      count = NA)
    top_heights_df$count <- sapply(top_heights_df$top_height, function(x) {
      sum(top_heights_recoreded == x)
    })
    ind.horizontal_complexity_n <- nrow(top_heights_df) # number of top heights recorded
    ind.horizontal_complexity_shannon <- vegan::diversity(top_heights_df$count, index = "shannon") # shannon diversity index of top heights
    
    # ancient/veteran trees ----
    ind.avt_number <- plot_data[[this.plot]][position.avt.number[1], position.avt.number[2]] %>% 
      as.numeric() %>% 
      ifelse(is.na(.), 0, .) # if NA, set to 0
    
    transect_area_ha <- (avt_search_radius_m*2 *100 + (pi * avt_search_radius_m^2))/10000 # area of the transect in ha
    
    ind.avt_density <- ind.avt_number / transect_area_ha # density of ancient/veteran trees per 
    
    
    # Create a list to store all the indicators ----
    survey_data[[which(these.plots==this.plot)]] <- list(
      survey_info = list(
        wood_name = wood_name,
        date_surveyed = date_surveyed,
        zone = zone,
        plot = plot,
        surveyor = surveyor,
        time_taken = time_taken
      ),
      
      trees_df = list(
        trees_main_df = trees_main_df,
        trees_supp_df = trees_supp_df
      ),
      
      indicators = list(
        
        tree_spp = list(
          ind.tree_spp_n.main = ind.tree_spp_n.main,
          ind.tree_spp_n.supp = ind.tree_spp_n.supp,
          ind.tree_spp_shannon.main = ind.tree_spp_shannon.main,
          ind.tree_spp_shannon.supp = ind.tree_spp_shannon.supp,
          ind.tree_spp_prop_appropriate.main = ind.tree_spp_prop_appropriate.main,
          ind.tree_spp_prop_appropriate.supp = ind.tree_spp_prop_appropriate.supp,
          trees_main_spp_present = trees_main_spp_present,
          trees_supp_spp_present = trees_supp_spp_present
        ),
        tree_age_classes = list(
          ind.age_classes_n.main = ind.age_classes_n.main,
          ind.age_classes_n.supp = ind.age_classes_n.supp,
          ind.tree_age_classes_shannon.main = ind.tree_age_classes_shannon.main,
          ind.tree_age_classes_shannon.supp = ind.tree_age_classes_shannon.supp,
          main_ages = main_ages,
          supp_ages = supp_ages
        ),
        regen = list(
          ind.regen_classes_n.main = ind.regen_classes_n.main,
          ind.regen_classes_n.supp = ind.regen_classes_n.supp,
          regen_species_classes.main = regen_species_classes.main,
          regen_species_classes.supp = regen_species_classes.supp
        ),
        
        invasives = list(
          ind.invasive_list = ind.invasive_list,
          ind.invasive.total_domin = ind.invasive.total_domin,
          invasive_spp = invasive_spp, # df of invasive species and cover
          ind.invasive.high_threat_present = ind.invasive.high_threat_present # if any high threat invasive species are present
        ),
        vert_structure = list(
          vert_structure = vert_structure, # df of vertical structure
          ind.vert_structure_n = ind.vert_structure_n # number of vertical strata past threshold
        ),
        
        canopy_nativness = list(
          ind.nativeness.measure = ind.nativeness.measure # nativeness measure
        ),
        
        tree_health = list(
          ind.tree_health.dieback.measure = ind.tree_health.dieback.measure, # tree health dieback measure
          ind.tree_health.mortality.measure = ind.tree_health.mortality.measure, # tree health mortality measure
          ind.tree_health.max.dieback_mortality = ind.tree_health.max.dieback_mortality # max of dieback and mortality measures
        ),
        
        anthropogenic_damage = list(
          ind.anthropogenic_damage.measure = ind.anthropogenic_damage.measure # anthropogenic damage measure
        ),
        
        deadwood = list(
          deadwood_df = deadwood_df, # df of deadwood types and counts
          ind.deadwood_type_quaters = ind.deadwood_type_quaters # number of deadwood types across all quarters
        ),
        
        microhabitats = list(
          ind.microhabitats_n = ind.microhabitats_n, # number of microhabitats present
          ind.microhabitats_prop = ind.microhabitats_prop, # proportion of microhabitats present
          microhabitats_present = microhabitats_present # vector of microhabitats present
        ),
        
        ground_flora = list(
          ground_flora_df = ground_flora_df,
          ind.groundflora_n = ind.groundflora_n, # number of ground flora species in plot
          ground_flora.not_counting = ground_flora.not_counting, # ground flora species not in the appropriate list
          ind.groundflora_prop_listed = ind.groundflora_prop_listed # proportion of ground flora species listed in the appropriate list
        ),
        
        herbivore_impact = list(
          herbivore_impact_df = herbivore_impact_df,
          ind.herbivore_impact.class = ind.herbivore_impact.class # herbivore impact class
        ),
        
        horizontal_complexity = list(
          ind.horizontal_complexity_n = ind.horizontal_complexity_n, # number of top heights recorded
          ind.horizontal_complexity_shannon = ind.horizontal_complexity_shannon, # shannon diversity index of top heights
          top_heights_df = top_heights_df # df of top heights and counts
        ),
        
        avts = list(
          ind.avt_number = ind.avt_number, # number of ancient/veteran trees
          ind.avt_density = ind.avt_density # density of ancient/veteran trees per hectare
        )
      )
    )
  }
  
  # Return the survey data ----
  return(survey_data)
} 

