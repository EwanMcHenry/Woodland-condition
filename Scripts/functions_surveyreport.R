# summary plot  plot_weca_contributions -----
# An "equilizer" style plot
## Horizontal bars for each indicator, to to weighting "filled in" up to their weighted value

plot_weca_contributions <- function(survey_data, summary_score, zone_summary, use_weighted = TRUE) {
  # Decide which values to use
  value_col <- if (use_weighted) "weighted_value" else "value"
  # Convert column name to symbol for tidy evaluation
  value_sym <- sym(value_col)
  
  # Create summary row ----
  summary_row <- tibble(
    indicator_name = "WECA summary score",
    Weighting = 100,
    weighted_value = summary_score$weighted,
    value = summary_score$unweighted
  )
  
  if (use_weighted) {
    summary_row <- summary_row %>%
      mutate(value = NA_real_)  # Prevent fill colour on summary bar
  }
  # Combine with original data
  zone_summary_ext <- bind_rows(summary_row, zone_summary) %>% 
    mutate(background_height = if (use_weighted) Weighting else 100)
  
  # plot values for each indicator ----
  indicator_plot_lookup <- c(
    "Tree age structure" = "age_summary",
    "Tree species composition" = "tree_spp_summary",
    "Regeneration" = "regen_summary",
    "Native canopy cover" = "native_canopy_summary",
    "Vertical structure" = "vertical_structure_summary",
    "Invasive plants" = "invasives_summary",
    "Microhabitats" = "microhabitats_summary",
    "Horizontal complexity" = "horizontal_complexity_summary",
    "Veteran trees" = "veteran_trees_summary",
    "Dead and decaying wood" = "dead_decaying_wood_summary",
    "Herbivore impact" = "herbivore_impact_summary",
    "Tree health" = "tree_health_summary",
    "Anthropogenic damage" = "anthropogenic_damage_summary",
    "Ground flora" = "ground_flora_summary"
  )  
  individual_values <- purrr::map_dfr(
    zone_summary$indicator_name,
    function(ind_name) {
      summary_name <- indicator_plot_lookup[[ind_name]]
      values <- all_summaries[[summary_name]]$value
      plot <- all_summaries[[summary_name]]$plot
      tibble(indicator_name = ind_name, value = values, 
             plot = plot, 
             plot_type = all_summaries[[summary_name]]$plot_type)
    }
  ) %>% 
    left_join(
      zone_summary %>% select(indicator_name, Weighting),
      by = "indicator_name"
    )
  individual_values$plot <- as.factor(individual_values$plot)
  
  individual_values$plot_type[is.na(individual_values$plot_type)] <- "main"
  individual_values$plot_value <- individual_values$value
  
  # Get individual values for each indicator
  if(use_weighted) {
    individual_values$plot_value <- individual_values$value * individual_values$Weighting / 100
  }
  
  # order indicators ----
  # Separate indicators and summary row
  indicators_only <- zone_summary_ext %>%
    filter(indicator_name != "WECA summary score") %>%
    arrange(value, weighted_value )
  
  ordered_levels <- indicators_only$indicator_name
  
  # Rebind with summary row and set levels
  zone_summary_ext <- bind_rows(
    indicators_only,
    filter(zone_summary_ext, indicator_name == "WECA summary score")
  ) %>%
    mutate(indicator_name = factor(indicator_name, levels = c(ordered_levels, "WECA summary score")))

    # plot text ----
  subtitle <- if (use_weighted) {
    paste("WECA summary score (weighted) =", round(summary_score$weighted,0))
  } else {
    paste("WECA summary score (unweighted) =", round(summary_score$unweighted,0))
  }
  title <- paste(
    survey_data[[1]]$survey_info$wood_name,
    "Zone", survey_data[[1]]$survey_info$zone
  )
  

  
  # plot ----
  
  
  
  ggplot(zone_summary_ext, aes(x = indicator_name)) +
    # Background bars (weighting or fixed 100)
    geom_col(aes(y = background_height), fill = "grey40", width = 0.6) +
    
    # Summary bar (red) ----
  geom_col(
    data = filter(zone_summary_ext, indicator_name == "WECA summary score"),
    aes(y = weighted_value),
    fill = "red", width = 0.4
  ) +
    
    # Indicator bars ----
  geom_col(
    data = filter(zone_summary_ext, indicator_name != "WECA summary score"),
    aes(y = !!value_sym, fill = value),
    width = 0.4
  ) +
    
    # Indicator score points ----
  geom_point(
    data = filter(zone_summary_ext, indicator_name != "WECA summary score"),
    aes(y = !!value_sym, fill = value),
    size = 4, shape = 21, colour = "black"
  ) +
    # Continuous fill scale for bar and point value scores
    scale_fill_viridis_c(
      limits = c(0, 100),
      name = "Indicator\nscore"
    ) +
    
    # NEW FILL SCALE before reusing fill for discrete `plot` values
    new_scale_fill() +
    
    # points - individual plot values ----
  geom_point(
    data = individual_values,
    aes(y = plot_value, fill = plot, shape = plot_type),
    size = 2, shape = 21, colour = "black", alpha = 0.8,
    position = position_jitter(width = 0.2, height = 0)  # jitter on x only
  ) +
    scale_fill_manual(values= wt_palette[-c(1,2,4)], name = "Plot") +
    scale_shape_manual(values = c(21, 24), name = "Plot type") + # 21 for main plots, 24 for supplementary plots
    
    # Axes, scales and titles ----
  coord_flip() +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20)
    ) +
    labs(
      x = NULL,
      y = "WECA score",
      title = paste(
        survey_data[[1]]$survey_info$wood_name,
        "Zone", survey_data[[1]]$survey_info$zone
      ),
      subtitle = subtitle
    ) +
    theme_pubr() +
    theme(
      legend.position = "right",
      axis.text.y = element_text(
        colour = c(rep("black", nrow(zone_summary)), "steelblue")
      )
    )
}

# circle_badge----
# a badge with a circle shape to embed in the report
circle_badge <- function(x, color = "#4CAF50", size = "2em") {
  glue::glue('<span style="
    display: inline-block;
    background-color: {color};
    color: white;
    border-radius: 50%;
    width: {size};
    height: {size};
    line-height: {size};
    text-align: center;
    font-weight: bold;
    font-family: sans-serif;">{round(x)}</span>')
}


# create summaries ----


create_age_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators
    tibble(
      plot = i,
      plot_type = c("main", "supp"),
      age_classes_n = c(ind$tree_age_classes$ind.age_classes_n.main, ind$tree_age_classes$ind.age_classes_n.supp),
      shannon = c(ind$tree_age_classes$ind.tree_age_classes_shannon.main, ind$tree_age_classes$ind.tree_age_classes_shannon.supp) %>% round(2),
    ) %>%
      bind_cols(
        bind_rows(ind$tree_age_classes$main_ages, ind$tree_age_classes$supp_ages)
      )
  }) %>% 
    # add corresponding value from lookup table
    left_join(age_lookup, by = c("age_classes_n" = "N.Tree.Age.Categories")) %>% 
    #reorder
    select(plot, plot_type, value, age_classes_n, shannon, everything()) %>% 
    mutate(plot_type = factor(plot_type, levels = c("main", "supp")),
           plot = factor(plot)) }

create_tree_species_summary <- function(){
  list(combined_table = map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$tree_spp
    
    spp_both_t <- ind$trees_both_spp_present %>%
      mutate(plot = i) %>% 
      pivot_wider(
        names_from = tree_species,
        values_from = spp_total_trees,
        values_fill = 0
      )
    
    meta_both <- tibble(
      plot = i,
      per_of_appropriate_species = ind$ind.tree_spp_prop_appropriate.both * 100,
      N_appropriate_species = ind$ind.tree_spp_N_appropriate.both,
      spp_n = ind$ind.tree_spp_n.both,
      shannon_index_appropriate = round(ind$ind.tree_spp_shannon_appropriate.both, 2)
    )
    
    # Right join ensures meta is kept even if spp is empty
    right_join(spp_both_t, meta_both, by = c("plot"))
  }) %>%
    # Closest match join using fuzzyjoin
    fuzzyjoin::difference_inner_join(tree_spp_lookup,
                                     by = c("per_of_appropriate_species" = "Proportion.of.appropriate.Tree...Shrub.Species"),
                                     max_dist = Inf,
                                     distance_col = "dist"
    ) %>%
    group_by(plot) %>%
    slice_min(order_by = dist, n = 1) %>%  # Keep only the closest match
    ungroup()  %>% 
    # replace nas with 0 for everything
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    select(-dist, -Proportion.of.appropriate.Tree...Shrub.Species) %>%
    select(plot, value, per_of_appropriate_species,N_appropriate_species, spp_n, shannon_index_appropriate, everything())  %>%
    relocate(
      # Move alphabetically sorted species columns to the end, after metadata
      sort(setdiff(names(.), c("plot", "value", "per_of_appropriate_species", "N_appropriate_species", "spp_n", "shannon_index_appropriate"))),
      .after = shannon_index_appropriate
    ) %>% 
    mutate(
      plot = factor(plot)
    ),
    
    separate_table = map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$tree_spp
    
    # Add plot and type columns to each tibble
    main <- ind$trees_main_spp_present %>%
      mutate(plot = i, plot_type = "main")
    
    supp <- ind$trees_supp_spp_present %>%
      mutate(plot = i, plot_type = "supp")
    

    # Combine, pivot wider
    spp_separate_t <- bind_rows(main, supp) %>%
      pivot_wider(
        names_from = tree_species,
        values_from = spp_total_trees,
        values_fill = 0
      )

    # Force rows to exist for both plot types
    meta_separate <- tibble(
      plot = i,
      plot_type = c("main", "supp"),
      per_of_appropriate_species = c(ind$ind.tree_spp_prop_appropriate.main, ind$ind.tree_spp_prop_appropriate.supp) * 100,
      N_appropriate_species = c(ind$ind.tree_spp_N_appropriate.main, ind$ind.tree_spp_N_appropriate.supp),
      spp_n = c(ind$ind.tree_spp_n.main, ind$ind.tree_spp_n.supp),
      shannon_index_appropriate = round(
        c(ind$ind.tree_spp_shannon_appropriate.main, ind$ind.tree_spp_shannon_appropriate.supp), 
        2
      )
    )
    
    # Right join ensures meta is kept even if spp is empty
    right_join(spp_separate_t, meta_separate, by = c("plot", "plot_type"))
  }) %>% 
    # Closest match join using fuzzyjoin
    fuzzyjoin::difference_inner_join(tree_spp_lookup,
                                     by = c("per_of_appropriate_species" = "Proportion.of.appropriate.Tree...Shrub.Species"),
                                     max_dist = Inf,
                                     distance_col = "dist"
    ) %>%
    group_by(plot, plot_type) %>%
    slice_min(order_by = dist, n = 1) %>%  # Keep only the closest match
    ungroup()  %>% 
    # replace nas with 0 for everything
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
    select(-dist, -Proportion.of.appropriate.Tree...Shrub.Species) %>%
    select(plot, plot_type, value, per_of_appropriate_species,N_appropriate_species, spp_n, shannon_index_appropriate, everything())  %>%
    relocate(
      # Move alphabetically sorted species columns to the end, after metadata
      sort(setdiff(names(.), c("plot", "plot_type", "value", "per_of_appropriate_species", "N_appropriate_species", "spp_n", "shannon_index_appropriate"))),
      .after = shannon_index_appropriate
    ) %>% 
    mutate(
      plot = factor(plot),
      plot_type = factor(plot_type, levels = c("main", "supp"))
    )
  )
} 

create_regen_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$regen
    
    main_classes_present <- ind$regen_species_classes.main %>%
      select(-tree_species) %>%
      colSums()>0
    
    supp_classes_present <- ind$regen_species_classes.supp %>%
      select(-tree_species) %>%
      colSums()>0
    
    # Add plot and type columns to each tibble
    main <- main_classes_present %>%
      tibble::enframe(name = "age_class", value = "present") %>%
      mutate(plot = i, plot_type = "main")
    
    supp <- supp_classes_present %>%
      tibble::enframe(name = "age_class", value = "present") %>%
      mutate(plot = i, plot_type = "supp")
    
    # Combine and pivot wider (species as columns)
    bind_rows(main, supp) %>%
      pivot_wider(
        names_from = age_class,      
        values_from = present,
        values_fill = FALSE         
      ) %>%
      left_join(
        tibble(
          plot = i,
          plot_type = c("main", "supp"),
          regen_classes_n = c(ind$ind.regen_classes_n.main, ind$ind.regen_classes_n.supp)
        ),
        by = c("plot", "plot_type")
      )
  }) %>% 
    # join value
    left_join(regen_lookup, by = c("regen_classes_n" = "N.Regen.classes.present")) %>% 
    #reorder
    select(plot, plot_type, value, regen_classes_n, everything()) %>% 
    mutate(plot_type = factor(plot_type, levels = c("main", "supp")),
           plot = factor(plot)) 
  
}

create_native_canopy_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$canopy_nativness
    
    tibble(
      plot = i,
      native_canopy_cover = ind$ind.nativeness.measure
    )
  }) %>% 
    # join value
    left_join(native_canopy_lookup, by = c("native_canopy_cover" = "Native.Canopy.Percentage")) %>% 
    #reorder
    select(plot, value, native_canopy_cover) %>% 
    mutate(plot = factor(plot)) 
}

create_vertical_structure_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$vert_structure
    
    # Extract stratum cover as wide format
    cover_df <- ind$vert_structure %>%
      as_tibble() %>%
      # mutate(stratum = str_replace_all(stratum, " \\(.*\\)", ""),  # Remove ranges
      #        stratum = str_replace_all(stratum, "\\s+", "_")) %>%  # Replace spaces with underscores
      select(stratum, cover) %>%
      pivot_wider(names_from = stratum, values_from = cover)
    
    # Add plot number and count of strata
    cover_df %>%
      mutate(
        plot = i,
        vertical_structure_n = survey_data[[i]]$indicators$vert_structure$ind.vert_structure_n
      ) %>%
      select(plot, vertical_structure_n, everything())
  }) %>% 
    # join value
    left_join(vertical_structure_lookup, by = c("vertical_structure_n" = "N.Vertical.Structure.Categories")) %>% 
    #reorder
    select(plot, value, vertical_structure_n, everything()) %>% 
    mutate(plot = factor(plot))
}

create_invasives_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$invasives
    
    # Handle the invasive_spp tibble (could be empty)
    inv_spp_wide <- ind$invasive_spp %>%
      # If empty, create empty tibble with columns for consistent pivoting
      { if (nrow(.) == 0) tibble(invasive_species = character(), cover = numeric()) else . } %>%
      mutate(invasive_species = str_replace_all(invasive_species, "\\s+", "_")) %>% # clean species names
      select(invasive_species, cover) %>%
      pivot_wider(names_from = invasive_species, values_from = cover, values_fill = 0)
    
    invasive_metadata <- tibble(
      plot = i,
      total_invasive_domin_cover = ind$ind.invasive.total_domin,
      high_threat_spp_present = ind$ind.invasive.high_threat_present
    )
    
    if (ncol(inv_spp_wide) > 0) {
      result <- bind_cols(invasive_metadata, inv_spp_wide)
    } else {
      result <- invasive_metadata
    }
    
    result %>% 
      left_join(invasives_lookup, by = c("total_invasive_domin_cover" = "domin")) %>% 
      fuzzyjoin::difference_left_join(
        invasive_plants_predictions,
        by = "value",
        max_dist = Inf,
        distance_col = "dist"
      ) %>%
      # Keep only the closest match per plot
      group_by(plot) %>%  
      slice_min(order_by = dist, n = 1, with_ties = FALSE) %>% 
      ungroup()  %>%
      # Keep only one value column, here we keep value.x and drop value.y
      select(-value.y, -dist) %>%
      rename(value = value.x,
             dummy_measure = measure ) %>%
      # if high threat present, overwrite value with 0
      mutate(value = ifelse(high_threat_spp_present, 0, value)) %>%
      select(plot, value, total_invasive_domin_cover ,high_threat_spp_present , everything()) %>% 
      mutate(plot = factor(plot))
  })
}

create_microhabitats_summary <- function(){
  
  # Microhabitats present list - cleaned unique
  all_microhabitats <- survey_data %>%
    map(~ .x$indicators$microhabitats$microhabitats_present) %>%
    unlist() %>%
    str_replace_all("\\r\\n", " ") %>%
    str_squish() %>%
    unique() %>% 
    sort()
  
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$microhabitats
    
    # Clean current plot's microhabitats present
    present_clean <- str_replace_all(ind$microhabitats_present, "\\r\\n", " ") %>% str_squish()
    
    # Create a logical named vector of presence/absence for all microhabitats
    presence_logical <- setNames(all_microhabitats %in% present_clean, all_microhabitats)
    
    # Build summary tibble
    base_tbl <- tibble(
      plot = i,
      microhabitats_prop = 100 * ind$ind.microhabitats_n  / length(microhabitats_list$microhabitat_name),
      microhabitats_n = ind$ind.microhabitats_n
    ) %>%
      # Join fuzzy lookup as before
      fuzzyjoin::difference_left_join(
        microhabitats_lookup,
        by = c("microhabitats_prop" = "Proportion.of.microhabitats.present"),
        max_dist = Inf,
        distance_col = "dist"
      ) %>%
      group_by(plot) %>%
      slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-dist, -Proportion.of.microhabitats.present) %>%
      select(plot, value, microhabitats_prop, microhabitats_n, everything()) %>%
      mutate(plot = factor(plot))
    
    # Bind presence/absence columns
    bind_cols(base_tbl, as_tibble(t(presence_logical)))
  })
}

create_horizontal_complexity_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$horizontal_complexity
    
    # N stops of each top hegith catgegory
    topheights_df <- vert_structure_categories %>%
      left_join(ind$top_heights_df %>% mutate(top_height = as.numeric(top_height)), 
                by = c("index" = "top_height")) %>%
      mutate(count = replace_na(count, 0)) %>%  # Replace missing counts with 0
      select(strata, count) %>%
      pivot_wider(names_from = strata, values_from = count)
    
    # summary table
    tibble(
      plot = i,
      N_topheight_cats = ind$ind.horizontal_complexity_n,
      shannon_topheight_cats = round(ind$ind.horizontal_complexity_shannon, 2)
    ) %>% 
      # add col for each top height and the count for each plot
      bind_cols(topheights_df %>% 
                  mutate(across(everything(), as.numeric))  # Ensure all counts are numeric
      )
  }) %>% 
    # join value
    left_join(Horizontal_complexity_lookup, by = c("N_topheight_cats" = "Horizontal.Complexity.Classes")) %>% 
    #reorder
    select(plot, value, N_topheight_cats, shannon_topheight_cats, everything() ) %>% 
    mutate(plot = factor(plot))
}

create_veteran_trees_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$avts
    
    tibble(
      plot = i,
      N_avt_n = ind$ind.avt_number,
      dens_avt = ind$ind.avt_density
    ) %>% 
      #reorder
      select(plot, N_avt_n, dens_avt) %>%
      mutate(plot = factor(plot))
  })
}

create_dead_decaying_wood_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$deadwood
    
    # Base row: plot number and number of type-quarters
    base <- tibble(
      plot = i,
      N_type_quaters = ind$ind.deadwood_type_quaters
    )
    
    # Count presence per deadwood type across quarters
    type_counts <- ind$deadwood_df %>%
      rowwise() %>%
      mutate(n_quarters = sum(c_across(starts_with("Q")), na.rm = TRUE)) %>%
      ungroup() %>%
      select(deadwood_type, n_quarters) %>%
      pivot_wider(
        names_from = deadwood_type,
        values_from = n_quarters,
        values_fill = 0
      )
    
    # Combine base and type counts
    bind_cols(base, type_counts) %>%
      mutate(plot = factor(plot))
  }) %>%
    # Join the value score using the volume-based lookup
    left_join(deadwood_lookup, by = c("N_type_quaters" = "N.Deadwood.Categories")) %>%
    # add average Ntype per quarter
    mutate(average_N_type_quaters = N_type_quaters / 4) %>%
    # Reorder for clarity
    select(plot, value, average_N_type_quaters, N_type_quaters, everything()) %>%
    mutate(plot = factor(plot))
}

create_herbivore_impact_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$herbivore_impact
    tibble(
      plot = i,
      herbivore_impact_class = ind$ind.herbivore_impact.class,
      palatable_removed = ind$herbivore_impact_df$per_removed[ind$herbivore_impact_df$palatability == "Palatatble species"],
      unpalatable_removed = ind$herbivore_impact_df$per_removed[ind$herbivore_impact_df$palatability == "Un-palatatble species"],
    ) %>%
      left_join(Herbivore_impact_lookup, by = "herbivore_impact_class") %>%
      select(plot, value, herbivore_impact_class, palatable_removed, unpalatable_removed) %>%
      mutate(plot = factor(plot),
             herbivore_impact_class = factor(herbivore_impact_class, 
                                             levels = c("Negligable damage", "Low damage", "Moderate damage", "High damage", "Very high damage"))
      ) 
  }) 
} 

create_tree_health_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$tree_health
    
    tibble(
      plot = i,
      dieback_per = ind$ind.tree_health.dieback.measure*100,
      suddenmortality_per = ind$ind.tree_health.mortality.measure*100,
      worst_indicator = ind$ind.tree_health.max.dieback_mortality*100
    ) %>%
      fuzzyjoin::difference_left_join(
        tree_health_lookup,
        by = c("worst_indicator" = "Dieback.or.sudden.mortality...."),
        max_dist = Inf,
        distance_col = "dist"
      ) %>%
      select(plot, value, dieback_per, suddenmortality_per, worst_indicator, dist) %>%
      mutate(plot = factor(plot))
  })%>%
    group_by(plot) %>%
    slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-dist) 
} 

create_anthropogenic_damage_summary <- function(){
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$anthropogenic_damage
    
    tibble(
      plot = i,
      damage_cover = as.character(ind$ind.anthropogenic_damage.measure)
    ) %>%
      left_join(antropogenic_damage_lookup, by = c("damage_cover" = "domin")) %>%
      select(plot, value, damage_cover) %>%
      mutate(plot = factor(plot))
  }) 
} 

create_ground_flora_summary <- function(){
  # Get the full list of all species across all plots
  all_species <- unique(unlist(
    lapply(survey_data, function(x) x$indicators$ground_flora$ground_flora_df$groundflora_species)
  ))
  
  map_dfr(seq_along(survey_data), function(i) {
    ind <- survey_data[[i]]$indicators$ground_flora
    
    
    # Species cover wide format
    if (nrow(ind$ground_flora_df) == 0) {
      wide_cover <- tibble(plot = i)
    } else {
      wide_cover <- ind$ground_flora_df %>%
        transmute(
          species = groundflora_species,
          cover = cover,
          plot = i
        ) %>%
        tidyr::pivot_wider(
          names_from = species,
          values_from = cover
        )
    }
    
    # Ensure all species columns are present
    missing_species <- setdiff(all_species, names(wide_cover))
    if (length(missing_species) > 0) {
      wide_cover[missing_species] <- NA_character_
    }
    
    # Join in summary data
    wide_cover <- wide_cover %>%
      left_join(
        tibble(
          plot = i,
          N_plant_spp = nrow(ind$ground_flora_df),
          N_appropriate_ground_flora = length(ind$ind.approp_groundflora_names_listed),
          perc_of_appropriate_ground_flora = ind$ind.groundflora_prop_listed * 100
        ),
        by = "plot"
      )
    
    # Join in value from lookup table using fuzzy match
    wide_cover %>%
      fuzzyjoin::difference_left_join(
        ground_flora_lookup,
        by = c("perc_of_appropriate_ground_flora" = "X..Appropraite.ground.flora.species"),
        max_dist = Inf,
        distance_col = "dist"
      ) %>%
      group_by(plot) %>%
      slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-dist, -`X..Appropraite.ground.flora.species`) %>%
      
      # Fill NA values in species columns with "0% Absent"
      mutate(across(
        .cols = -c(plot, value, perc_of_appropriate_ground_flora, N_appropriate_ground_flora, N_plant_spp),
        .fns = ~replace_na(.x, domin_transformer$domin[1])
      )) %>%
      
      # Reorder columns
      select(plot, value, perc_of_appropriate_ground_flora, N_appropriate_ground_flora, N_plant_spp, everything()) %>%
      mutate(plot = factor(plot))
  })
}

create_all_summaries <- function() {
  list(
    age_summary = create_age_summary(),
    tree_spp_summary = create_tree_species_summary(),
    regen_summary = create_regen_summary(),
    native_canopy_summary = create_native_canopy_summary(),
    vertical_structure_summary = create_vertical_structure_summary(),
    invasives_summary = create_invasives_summary(),
    microhabitats_summary = create_microhabitats_summary(),
    horizontal_complexity_summary = create_horizontal_complexity_summary(),
    veteran_trees_summary = create_veteran_trees_summary(),
    dead_decaying_wood_summary = create_dead_decaying_wood_summary(),
    herbivore_impact_summary = create_herbivore_impact_summary(),
    tree_health_summary = create_tree_health_summary(),
    anthropogenic_damage_summary = create_anthropogenic_damage_summary(),
    ground_flora_summary = create_ground_flora_summary()
  )
}

