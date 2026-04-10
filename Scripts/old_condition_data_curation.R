# Curation of old condition data prior to indicator calculation



overall0 <- overall
long_plots0 <- long_plots

# some basic descriptive info about the data before curation -----
results$plot_data_description <- list(NA)
results$plot_data_description$nplots_og = nrow(long_plots0)
results$plot_data_description$n_surveys_og <- length(unique(long_plots0$id))
results$plot_data_description$nplot_sites_og <- length(unique(long_plots0$Site.Name))

results$overall_data_description <- list(NA)
results$overall_data_description$n_surveys_og <- length(unique(overall0$id))
results$overall_data_description$nplot_sites_og <- length(unique(overall0$Site.Name))

curation_notes_plots <- character()
curation_notes_overall <- character()

curation_notes_plots <- c(
  curation_notes_plots,
  "### Plot data curation",
  paste0( "The initial plot data contained information from ", results$plot_data_description$nplots_og, " plots, covering ", results$plot_data_description$n_surveys_og, " surveys over ",  results$plot_data_description$nplot_sites_og, " sites." )
)

curation_notes_overall <- c(
  curation_notes_overall,
  "### Overall data curation",
  paste0( "The initial overall data contained information from ", results$overall_data_description$n_surveys_og, " surveys over ",  results$overall_data_description$nplot_sites_og, " sites." )
)

# functions ----
fill_assumed_indicators <- function(df, indicators, indicator_types) {
  
  for (ind in indicators) {
    
    ind_type <- indicator_types$type[indicator_types$indicator == ind]
    
    # logical vector: was *any* indicator in this set measured at this plot?
    any_measured <- df %>%
      select(all_of(indicators)) %>%
      apply(1, function(x) any(!is.na(x)))
    
    if (ind_type == "dafor") {
      
      df[[ind]] <- ifelse(
        is.na(df[[ind]]) & any_measured,
        "Absent",
        as.character(df[[ind]])
      )
      
      df[[ind]] <- factor(
        df[[ind]],
        levels  = dafor_levels,
        ordered = TRUE
      )
      
    } else if (ind_type %in% c("numeric", "richness")) {
      
      df[[ind]] <- ifelse(
        is.na(df[[ind]]) & any_measured,
        0,
        as.numeric(df[[ind]])
      )
      
    } else if (ind_type == "logical") {
      
      df[[ind]] <- ifelse(
        is.na(df[[ind]]) & any_measured,
        FALSE,
        as.logical(df[[ind]])
      )
    }
  }
  
  df
}
# function to sum rows, returning NA if all values are NA
row_sum_na_if_all_na <- function(...) {
  vals <- cbind(...)
  if_else(
    rowSums(!is.na(vals)) == 0,   # NA where all NA in that row
    NA_real_,
    rowSums(vals, na.rm = TRUE)
  )
}


# is there dafor in those
has_indicator <- function(data, cols) {
  
  rowSums(
    as.matrix(data[, cols, drop = FALSE]) > "Absent",
    na.rm = TRUE
  ) > 0
}
# helpers ----

tree.ageclass.indicators <- indicator_types$indicator [indicator_types$theme == "tree_age"]
shrub.cover.indicators <- indicator_types$indicator [indicator_types$theme == "shrub_cover"]

tree.regen.level.indicators <- c("LTR.Seedlings.Less.Than.10cm", "LTR.Seedlings.10.100cm", "LTR.Saplings.Greater.Than.100cm",
                                 "LTR.Coppice.Regrowth.or.Suckering")
shrub.regen.level.indicators <- c("LSR.Seedlings.Less.Than.10cm", "LSR.Seedlings.10.100cm", "LSR.Saplings.Greater.Than.100cm",
                                  "LSR.Coppice.Regrowth.or.Suckering")
regen.level.indicators <- c(tree.regen.level.indicators, shrub.regen.level.indicators)

tree.regen.sr.indicators <- c("RTS.Native.richness",     "RTS.Non.Native.richness" )
shrub.regen.sr.indicators <- c("RSS.Native.richness",     "RSS.Non.Native.richness" )

tree_sr.indicators <- c("TS.Native.richness", "TS.Non.Native.richness")
shrub_sr.indicators <- c("SS.Native.richness", "SS.Non.Native.richness")

tree_shrub_sr.indicators <- indicator_types$indicator[indicator_types$theme %in% c( "tree_shrub_species_richness") &
                                                                      indicator_types$type %in% c("numeric", "richness")]
richness_indicators <- indicator_types$indicator[indicator_types$theme %in% c( "tree_shrub_species_richness",
                                                                               "regeneration_species_richness")]


# 1. overall info from plots
## 1.1 sometimes overall species richness was the total of the relevent species richnesses gathered across plots ----
# where this was the case but not == the max of plots, it was recorded and the overall replaced with the max of plot entries

# find sum of each tree/shrub species richness across plots for each survey
plot_sr_sum_max <- long_plots0 %>%
  group_by(id) %>%
  summarise(
    plot_tot_TS.Native.richness = sum(TS.Native.richness, na.rm = TRUE),
    plot_tot_TS.Non.Native.richness = sum(TS.Non.Native.richness, na.rm = TRUE),
    plot_tot_SS.Native.richness = sum(SS.Native.richness, na.rm = TRUE),
    plot_tot_SS.Non.Native.richness = sum(SS.Non.Native.richness, na.rm = TRUE),
    plot_tot_RTS.Native.richness = sum(RTS.Native.richness, na.rm = TRUE),
    plot_tot_RTS.Non.Native.richness = sum(RTS.Non.Native.richness, na.rm = TRUE),
    plot_tot_RSS.Native.richness = sum(RSS.Native.richness, na.rm = TRUE),
    plot_tot_RSS.Non.Native.richness = sum(RSS.Non.Native.richness, na.rm = TRUE),
    
    plot_max_TS.Native.richness = max(TS.Native.richness, na.rm = TRUE),
    plot_max_TS.Non.Native.richness = max(TS.Non.Native.richness, na.rm = TRUE),
    plot_max_SS.Native.richness = max(SS.Native.richness, na.rm = TRUE),
    plot_max_SS.Non.Native.richness = max(SS.Non.Native.richness, na.rm = TRUE),
    plot_max_RTS.Native.richness = max(RTS.Native.richness, na.rm = TRUE),
    plot_max_RTS.Non.Native.richness = max(RTS.Non.Native.richness, na.rm = TRUE),
    plot_max_RSS.Native.richness = max(RSS.Native.richness, na.rm = TRUE),
    plot_max_RSS.Non.Native.richness = max(RSS.Non.Native.richness, na.rm = TRUE)
  )
# replace all -Inf with NA (which happens when all plot entries are NA for a survey)
plot_sr_sum_max <- plot_sr_sum_max %>%
  mutate(across(starts_with("plot_max"), ~ if_else(is.infinite(.), NA_real_, .)))

# join that to overall data and compare to overall species richness entries
overall0 <- overall0 %>%
  left_join(plot_sr_sum_max, by = "id")

# add a T/F label for if (overall = plot total for them, but not the max of them) and overall is > suspicious
suspicious.thresh <- 0

# Add the T/F “suspicious” flags
# Apply the logic indicator-by-indicator to avoid tidy-eval issues
overall0 <- reduce(
  richness_indicators,
  
  function(df, ind) {
    
    # Construct related column names for this indicator
    flag_col <- paste0("overall_", ind, ".matches_plot_sum_not_max")
    plot_tot <- paste0("plot_tot_", ind)
    plot_max <- paste0("plot_max_", ind)
    
    df %>%
      mutate(
        # ---------------------------------------------------------
        # Flag cases where:
        #  - overall value equals the sum across plots
        #  - but does NOT equal the maximum plot value
        #  - and exceeds the suspicious threshold
        # These are likely summed incorrectly rather than summarised
        # ---------------------------------------------------------
        !!flag_col := if_else(
          is.na(.data[[ind]]) | is.na(.data[[plot_tot]]),
          NA,  # propagate NA if either value is missing
          .data[[ind]] == .data[[plot_tot]] &
            .data[[ind]] != .data[[plot_max]] &
            .data[[ind]] > suspicious.thresh
        ),
        
        # ---------------------------------------------------------
        # Where flagged as suspicious, replace the overall value
        # with the maximum plot value
        # (a crude but safer approximation than the sum)
        # ---------------------------------------------------------
        !!ind := if_else(
          .data[[flag_col]] == TRUE,
          .data[[plot_max]],
          .data[[ind]]
        )
      )
  },
  
  # Start the reduction from the existing overall dataset
  .init = overall0
)


# N surveys with at least one overall species richness that matches the sum of plot species richness but not the max of plot species richness 
survey_rows_summing_plot_sr <- which(
  overall0$overall_TS.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_TS.Non.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_SS.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_SS.Non.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_RTS.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_RTS.Non.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_RSS.Native.richness.matches_plot_sum_not_max == TRUE |
    overall0$overall_RSS.Non.Native.richness.matches_plot_sum_not_max == TRUE)

curation_notes_overall <- c(
  curation_notes_overall,
  "#### Where 'overall' species richness was the sum of species richness across plots",
  "Some suspiciously high overall species richness entries were found in the original data and investigated further.",
  "The 'overall' indicators were intended to characterise conditions across the site in general, however, consulting with site managers and investigating these suspiciously high entries it appeared that sometimes 'overall' was interperated as e.g. the sum of species across plots (Notably, this interpretation was not consistent across the surveys of individual surveyors).",
  paste0("- Within ", length(survey_rows_summing_plot_sr), " surveys at least one overall species richness indicator matched the sum of its respective plot-level entries, but not the maxium of plots. These were changed so that each overall species richness indicator was equal to the maximum of plot-level entries. This was a *significant and crude method of aproximation*, but judged better than throwing out those entries, summing was much worse as it biased any comparision with the rest of the dataset. Notably, it was not carried out across entire surveys: only on the indicators that suggested suspcion. This affected the following overall species richness indicators:"),
    paste0(" - ", sum ( overall0$overall_TS.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for native trees."),
    paste0(" - ", sum ( overall0$overall_TS.Non.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for non-native trees."),
    paste0(" - ", sum ( overall0$overall_SS.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for native shrubs."),
    paste0(" - ", sum ( overall0$overall_SS.Non.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for non-native shrubs."),
    paste0(" - ", sum ( overall0$overall_RTS.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for native tree regeneration."),
    paste0(" - ", sum ( overall0$overall_RTS.Non.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for non-native tree regeneration."),
    paste0(" - ", sum ( overall0$overall_RSS.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for native shrub regeneration."),
    paste0(" - ", sum ( overall0$overall_RSS.Non.Native.richness.matches_plot_sum_not_max, na.rm = T), " surveys for non-native shrub regeneration.")
)
  

## 1.2 sometimes overall species richness was less than the max of the plots, which is impossible - change to the max of the plots ----
survey_rows_overall_sr_less_than_plot_max <- which(
  (overall0$TS.Native.richness < overall0$plot_max_TS.Native.richness) |
    (overall0$TS.Non.Native.richness < overall0$plot_max_TS.Non.Native.richness) |
    (overall0$SS.Native.richness < overall0$plot_max_SS.Native.richness) |
    (overall0$SS.Non.Native.richness < overall0$plot_max_SS.Non.Native.richness) |
    (overall0$RTS.Native.richness < overall0$plot_max_RTS.Native.richness) |
    (overall0$RTS.Non.Native.richness < overall0$plot_max_RTS.Non.Native.richness) |
    (overall0$RSS.Native.richness < overall0$plot_max_RSS.Native.richness) |
    (overall0$RSS.Non.Native.richness < overall0$plot_max_RSS.Non.Native.richness)
)

# aa = cnd[cnd$id %in% overall0$id[survey_rows_overall_sr_less_than_plot_max],]


## id those overall species richness entries less than the max of the plots
sr_less_than_plot_max <- sapply(
  richness_indicators,
  function(ind) {
    plot_max <- overall0[[paste0("plot_max_", ind)]]
    overall  <- overall0[[ind]]
    
    !is.na(overall) &
      !is.na(plot_max) &
      overall < plot_max
  }
)

# Convert to tibble for easier handling
sr_less_than_plot_max <- as_tibble(sr_less_than_plot_max)

# Count corrections per indicator
sr_correction_counts <- colSums(sr_less_than_plot_max)

# apply correction
for (ind in richness_indicators) {
  overall0[[ind]] <- pmax(
    overall0[[ind]],
    overall0[[paste0("plot_max_", ind)]],
    na.rm = TRUE
  )
}

curation_notes_overall <- c(
  curation_notes_overall,
  "#### Overall species richness less than plot maximum",
  paste0(
    "In ",
    sum(rowSums(sr_less_than_plot_max) > 0),
    " surveys at least one overall species richness indicator was less than the maximum of its respective plot-level entries, which is impossible. ",
    "These values were corrected so that overall richness equalled the maximum plot-level richness for the affected indicators:"
  ),
  paste0(
    "- Native tree richness: ", sr_correction_counts["TS.Native.richness"], " surveys."
  ),
  paste0(
    "- Non-native tree richness: ", sr_correction_counts["TS.Non.Native.richness"], " surveys."
  ),
  paste0(
    "- Native shrub richness: ", sr_correction_counts["SS.Native.richness"], " surveys."
  ),
  paste0(
    "- Non-native shrub richness: ", sr_correction_counts["SS.Non.Native.richness"], " surveys."
  ),
  paste0(
    "- Native tree regeneration richness: ", sr_correction_counts["RTS.Native.richness"], " surveys."
  ),
  paste0(
    "- Non-native tree regeneration richness: ", sr_correction_counts["RTS.Non.Native.richness"], " surveys."
  ),
  paste0(
    "- Native shrub regeneration richness: ", sr_correction_counts["RSS.Native.richness"], " surveys."
  ),
  paste0(
    "- Non-native shrub regeneration richness: ", sr_correction_counts["RSS.Non.Native.richness"], " surveys."
  )
)



# 2. remove rows with NA across all tree related information (age class cover, regeneration, species count) ----
## identify tree rows
tree_cols <- indicator_types$indicator[indicator_types$theme %in% c("tree_age", "tree_shrub_species_richness", 
                                                                         "shrub_cover", 
                                                                         "regeneration_level", "regeneration_species_richness")]
tree_cols <- tree_cols[!grepl("Dominated", tree_cols)] # remove those containing "Dominated"
## identify rows where all NA
plot_na_tree_id = long_plots0$id[ which(
  long_plots0[,tree_cols] %>% is.na() %>% rowSums() == length(tree_cols)
)]
long_plots0 <- long_plots0[!long_plots0$id %in% plot_na_tree_id, ]

overall_na_tree_id = overall0$id[which(
  overall0[,tree_cols] %>% is.na() %>% rowSums() == length(tree_cols)
)]

# which ids have tree info at plots but not overall level
plot_but_no_overall_treeinfo_id <- long_plots0$id[long_plots0$id %in% overall_na_tree_id] %>% 
  unique()
# which ids have tree info at overall or plot level
overall_or_plot_tree_info_id <- unique(c(overall0$id[!overall0$id %in% overall_na_tree_id], 
                                         long_plots0$id[!long_plots0$id %in% plot_na_tree_id]))

# remove overall surveys where no tree id at overall or plot level
overall0 <- overall0[overall0$id %in% overall_or_plot_tree_info_id, ]

# which ids have overall but not plot tree info
overall_but_no_plot_treeinfo_id <- overall0$id[overall0$id %in% plot_na_tree_id] %>% 
  unique()

# remove na_tree_rows

results$plot_data_description$nplots_no_tree_info = length(plot_na_tree_id)
results$plot_data_description$nplots_tree_info = nrow(long_plots0)
results$plot_data_description$n_surveys_tree_info <- length(unique(long_plots0$id))
results$plot_data_description$n_strata_tree_info <- length(unique(paste(long_plots0$Stratum, long_plots0$Site.Name)))
results$plot_data_description$nplot_sites_tree_info <- length(unique(long_plots0$Site.Name))


results$overall_data_description$n_surveys_no_tree_info <- length(overall_na_tree_id)
results$overall_data_description$n_surveys_tree_info <- length(unique(overall0$id))

results$overall_data_description$n_strata_tree_info <- length(unique(paste(overall0$Stratum, overall0$Site.Name)))
results$overall_data_description$n_sites_tree_info <- length(unique(overall0$Site.Name))

curation_notes_plots <- c(
  curation_notes_plots,
  "#### Removing plots with no tree/shrub information",
  paste0( "- Of the original dataset (", nrow(long_plots), " plots), ", 
          results$plot_data_description$nplots_no_tree_info, " plots contained no information on trees or shrubs (i.e. tree age, species richness, shrub cover and regneration info was all missing).", 
          " Those ", results$plot_data_description$nplots_no_tree_info ," plots were removed, leaving data from ", 
          results$plot_data_description$nplots_tree_info, 
          " plots, covering ", results$plot_data_description$n_surveys_tree_info, 
          " surveys over ",  results$plot_data_description$n_strata_tree_info,
          " strata, over ", results$plot_data_description$nplot_sites_tree_info, " sites." ),
  paste0("- Notably, ", length(overall_but_no_plot_treeinfo_id), " of those surveys removed with no plot-level tree/shrub information had tree/shrub information at overall-level (these plots were not considered hereafter). It is expected that these are mainly surveys where a surveyor chose not to employ plots (the methodology did not make them compulsory)")
)

curation_notes_overall <- c(
  curation_notes_overall,
  "#### Removing surveys with no tree/shrub information",
  paste0( "- Of the original dataset (", results$overall_data_description$n_surveys_og, 
          " surveys), ", results$overall_data_description$n_surveys_no_tree_info, 
          " surveys contained no overall information on trees or shrubs (i.e. tree age, species richness, shrub cover and regneration info was all missing)."),
  paste0( "-  Of those, ", length(plot_but_no_overall_treeinfo_id), " had tree/shrub information at the plot level"),
  " - Consulting with site managers, this seems to reflect the 'primarily-a-reference-note' nature of the dataset: an overall summary may not have been considered useful for them and their decision making.",
  paste0( "- The ", results$overall_data_description$n_surveys_no_tree_info - length(plot_but_no_overall_treeinfo_id), " surveys with no tree/shrub information at either overall or plot level were removed."),
  paste0("The remaining data contained ", dim(overall0)[1], " surveys, covering ", 
          length(unique(paste(overall0$Stratum, overall0$Site.Name))), "strata, over ", 
          length(unique(overall0$Site.Name)), " sites." )
  )


# 3. Manipulations where data entered wrong ----
## 3.2 sometimes species richness was confused as % canopy nativness for natives and Non.Natives - really high sr values given ----

# 
# aa = long_plots0[, c(richness_indicators,
#                      "tot_tree_SR", "tot_shrub_SR", "tot_tree_regn_SR", "tot_shrub_regen_SR")]
# aa = overall0[, c(richness_indicators)]

plot_sr_thought_to_be_percent_error_rows = which(
  long_plots0$TS.Native.richness >= 20 | # inspected aa manuely to find that threshold
    long_plots0$TS.Non.Native.richness >= 20|
    long_plots0$SS.Native.richness >= 20 |
    long_plots0$SS.Non.Native.richness >= 20 |
    long_plots0$RSS.Native.richness >= 20 |
    long_plots0$RSS.Non.Native.richness >= 20|
    long_plots0$RTS.Native.richness >= 20 |
    long_plots0$RTS.Non.Native.richness >= 20
)
long_plots0[plot_sr_thought_to_be_percent_error_rows, 
  richness_indicators] <- NA

overall_na_sr_thought_to_be_percent_error_rows = which(
  overall0$TS.Native.richness >= 20 | # inspected aa manuely to find that threshold
    overall0$TS.Non.Native.richness >= 20|
    overall0$SS.Native.richness >= 20 |
    overall0$SS.Non.Native.richness >= 20 |
    overall0$RSS.Native.richness >= 20 |
    overall0$RSS.Non.Native.richness >= 20|
    overall0$RTS.Native.richness >= 20 |
    overall0$RTS.Non.Native.richness >= 20
)
overall0[overall_na_sr_thought_to_be_percent_error_rows, 
         richness_indicators] <- NA


curation_notes_plots <- c(
  curation_notes_plots,
  "#### Unusually high species richness values - likely data entry errors",
  paste0("- For ", length(plot_sr_thought_to_be_percent_error_rows) , " plots very high (>20) tree/shrub species richness values were entered that were likely data entry errors (confused with % canopy cover).",
         "  All species richness entries were replaced with NA in those plots."
  )
)

curation_notes_overall <- c(
  curation_notes_overall,
  "#### Unusually high species richness values - likely data entry errors",
  paste0("- For ", length(overall_na_sr_thought_to_be_percent_error_rows) , " surveys very high (>20) tree/shrub species richness values were entered that were likely data entry errors (confused with % canopy cover).",
         "  All species richness entries were replaced with NA in those surveys."
  ),
  paste0("Noteably, some high species richness values remained in the dataset, particuarlly for native tree species where ", sum(overall0$TS.Native.richness > 10, na.rm = T), " surveys had values > 10. These were not changed as they were not considered implausible, but should be interpreted with caution.")
)

## 3.3 0 species richness was recorded, but the presence of trees/shrubs was indicated elsewhere (eg tree age class cover, shrub cover, regeneration level) ----

# plots
plot_wrong_zero_ts <- which(
  # if 0 sr recorded
  row_sum_na_if_all_na(long_plots0$TS.Native.richness,  long_plots0$TS.Non.Native.richness) == 0 &
    # and some tree age class cover recorded (which implies presence of trees, and therefore sr > 0)
    has_indicator(long_plots0, indicator_types$indicator [indicator_types$theme == "tree_age"])
)
long_plots0$TS.Native.richness[plot_wrong_zero_ts] <- NA
long_plots0$TS.Non.Native.richness[plot_wrong_zero_ts] <- NA

plot_wrong_zero_ss <- which(
  row_sum_na_if_all_na(long_plots0$SS.Native.richness,  long_plots0$SS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, indicator_types$indicator [indicator_types$theme == "shrub_cover"])
)
long_plots0$SS.Non.Native.richness[plot_wrong_zero_ss] <- NA
long_plots0$SS.Native.richness[plot_wrong_zero_ss] <- NA

plot_wrong_zero_ts.regen <- which(
  row_sum_na_if_all_na(long_plots0$RTS.Native.richness,  long_plots0$RTS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, tree.regen.level.indicators)
)
long_plots0$RTS.Native.richness[plot_wrong_zero_ts.regen] <- NA
long_plots0$RTS.Non.Native.richness[plot_wrong_zero_ts.regen] <- NA

plot_wrong_zero_ss.regen <- which(
  row_sum_na_if_all_na(long_plots0$RSS.Native.richness,  long_plots0$RSS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, shrub.regen.level.indicators)
)
long_plots0$RSS.Native.richness[plot_wrong_zero_ss.regen] <- NA
long_plots0$RSS.Non.Native.richness[plot_wrong_zero_ss.regen] <- NA

curation_notes_plots <- c(
  curation_notes_plots,
  "#### Impossible species richness",
  paste0(
    "- For ",
    length(unique(c(
      plot_wrong_zero_ts,
      plot_wrong_zero_ss,
      plot_wrong_zero_ts.regen,
      plot_wrong_zero_ss.regen
    ))),
    " plots tree/shrub/regen species richness was recorded as 0, but the presence of recorded cover for the corresponding vegetation suggested this was an error. These 0s were replaced with NAs at:\n",
    "  - ", length(plot_wrong_zero_ts), " plots for tree species richness\n",
    "  - ", length(plot_wrong_zero_ss), " plots for shrub species richness\n",
    "  - ", length(plot_wrong_zero_ts.regen), " plots for tree regeneration species richness\n",
    "  - ", length(plot_wrong_zero_ss.regen), " plots for shrub regeneration species richness"
  )
  )

# overall
## where cover suggested
overall_wrong_zero_ts <- which(
  row_sum_na_if_all_na(overall0$TS.Native.richness,  overall0$TS.Non.Native.richness) == 0 &
    has_indicator(overall0, indicator_types$indicator [indicator_types$theme == "tree_age"])
)
overall0$TS.Native.richness[overall_wrong_zero_ts] <- NA
overall0$TS.Non.Native.richness[overall_wrong_zero_ts] <- NA

overall_wrong_zero_ss <- which(
  row_sum_na_if_all_na(overall0$SS.Native.richness,  overall0$SS.Non.Native.richness) == 0 &
    has_indicator(overall0, indicator_types$indicator [indicator_types$theme == "shrub_cover"])
)
overall0$SS.Non.Native.richness[overall_wrong_zero_ss] <- NA
overall0$SS.Native.richness[overall_wrong_zero_ss] <- NA

overall_wrong_zero_ts.regen <- which(
  row_sum_na_if_all_na(overall0$RTS.Native.richness,  overall0$RTS.Non.Native.richness) == 0 &
    has_indicator(overall0, tree.regen.level.indicators)
)
overall0$RTS.Native.richness[overall_wrong_zero_ts.regen] <- NA
overall0$RTS.Non.Native.richness[overall_wrong_zero_ts.regen] <- NA

overall_wrong_zero_ss.regen <- which(
  row_sum_na_if_all_na(overall0$RSS.Native.richness,  overall0$RSS.Non.Native.richness) == 0 &
    has_indicator(overall0, shrub.regen.level.indicators)
)
overall0$RSS.Native.richness[overall_wrong_zero_ss.regen] <- NA
overall0$RSS.Non.Native.richness[overall_wrong_zero_ss.regen] <- NA


curation_notes_overall <- c(
  curation_notes_overall,
  "#### Impossible species richness",
  paste0(
    "- For ",
    length(unique(c(
      overall_wrong_zero_ts,
      overall_wrong_zero_ss,
      overall_wrong_zero_ts.regen,
      overall_wrong_zero_ss.regen
    ))),
    " surveys overall tree/shrub/regen species richness was recorded as 0, but the presence of recorded cover for the corresponding vegetation in that survey suggested this was an error. These 0s were replaced with NAs at:\n",
    "  - ", length(overall_wrong_zero_ts), " surveys for tree species richness\n",
    "  - ", length(overall_wrong_zero_ss), " surveys for shrub species richness\n",
    "  - ", length(overall_wrong_zero_ts.regen), " surveys for tree regeneration species richness\n",
    "  - ", length(overall_wrong_zero_ss.regen), " surveys for shrub regeneration species richness"
  )
)

# 4. filling accidentally missing indicator values ----
## implied observations where plot exists but indicator is NA
### DAFOR "Absent"
### Logical FALSE
### Numeric/Richness 0 (no filling)
##### 4.1 Tree/shrub species richness indicators:  ----

results$plot_data_description$n_plots_missing_richness_any_before <- sum(
  rowSums(is.na(long_plots0[tree_shrub_sr.indicators])) > 0
)

results$overall_data_description$n_surveys_missing_richness_any_before <- sum(
  rowSums(is.na(overall0[tree_shrub_sr.indicators])) > 0
)


###### 4.1.1 - where all tree/shrub species richness info absent, and corresponding  tree/shrub cover also absent ----
# plots
## trees
na_tree_sr_rows_no_tree_info = which(
  # na in all tree species richness cols
  long_plots0[,tree_sr.indicators] %>% is.na() %>% rowSums() == length(tree_sr.indicators) &
    # na in all tree age cols
    !has_indicator(long_plots0, tree.ageclass.indicators)
)
long_plots0[na_tree_sr_rows_no_tree_info, tree_sr.indicators] <- 0

## shrubs
na_shrub_sr_rows_no_shrub_info = which(
  # all shrub species richness missing
  rowSums(is.na(long_plots0[, shrub_sr.indicators, drop = FALSE])) == length(shrub_sr.indicators) &
    
    # all shrub cover missing
    !has_indicator(long_plots0, shrub.cover.indicators) &
    
    # but some tree age or SR info present - support that something WAS measured
    (has_indicator(long_plots0, tree.ageclass.indicators) |
     rowSums(is.na(long_plots0[, tree_sr.indicators, drop = FALSE])) < length(tree_sr.indicators)
    )
)
long_plots0[na_shrub_sr_rows_no_shrub_info, shrub_sr.indicators] <- 0

# overall
## trees
overall_na_tree_sr_rows_no_tree_info = which(
  # na in all tree species richness cols
  overall0[,tree_sr.indicators] %>% is.na() %>%
    rowSums() == length(tree_sr.indicators) &
    # na in all tree age cols
    !has_indicator(overall0, tree.ageclass.indicators)
)
overall0[overall_na_tree_sr_rows_no_tree_info, tree_sr.indicators] <- 0

## shrubs
overall_na_shrub_sr_rows_no_shrub_info = which(
  # all shrub species richness missing
  rowSums(is.na(overall0[, shrub_sr.indicators, drop= FALSE])) == length(shrub_sr.indicators) &
    
    # all shrub cover missing
    !has_indicator(overall0, shrub.cover.indicators) &
    
    # but some tree age or SR info present - support that something WAS measured
    (has_indicator(overall0, tree.ageclass.indicators) |
     rowSums(is.na(overall0[, tree_sr.indicators, drop = FALSE])) < length(tree_sr.indicators)
    )
)
overall0[overall_na_shrub_sr_rows_no_shrub_info, shrub_sr.indicators] <- 0


###### 4.1.2 - where only one of native/Non.Native filled for species richness ----
#plots
## trees
na_tree_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  long_plots0[,tree_sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree_sr.indicators] %>% is.na() %>% rowSums() < length(tree_sr.indicators)
)
# replace those nas with 0
long_plots0[na_tree_sr_rows_some, tree_sr.indicators] <-
  long_plots0[na_tree_sr_rows_some, tree_sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

## shrubs
na_shrub_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  long_plots0[,shrub_sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,shrub_sr.indicators] %>% is.na() %>% rowSums() < length(shrub_sr.indicators)
)
# replace those nas with 0
long_plots0[na_shrub_sr_rows_some, shrub_sr.indicators] <-
  long_plots0[na_shrub_sr_rows_some, shrub_sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

# overall
## trees
overall_na_tree_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  overall0[,tree_sr.indicators] %>% is.na() %>% rowSums
  () > 0 &
    # but data present for at least one of them
    overall0[,tree_sr.indicators] %>% is.na() %>% rowSums() < length(tree_sr.indicators)
)
overall0[overall_na_tree_sr_rows_some, tree_sr.indicators] <- overall0[overall_na_tree_sr_rows_some, tree_sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
## shrubs
overall_na_shrub_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  overall0[,shrub_sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,shrub_sr.indicators] %>% is.na() %>% rowSums() < length(shrub_sr.indicators)
)
overall0[overall_na_shrub_sr_rows_some, shrub_sr.indicators] <- overall0[overall_na_shrub_sr_rows_some, shrub_sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))


###### 4.1.XX - curation notes etc ----

# plots
plot_na_remaining_tree_sr_rows = which(
  # na in all tree species richness cols
  long_plots0[,tree_sr.indicators] %>% is.na() %>% rowSums() > 0
)
results$plot_data_description$n_plots_missing_tree_richness_any_after <- length(plot_na_remaining_tree_sr_rows)

plot_na_remaining_shrub_sr_rows = which(
  # na in all shrub species richness cols
  long_plots0[,shrub_sr.indicators] %>% is.na() %>% rowSums() > 0
)
results$plot_data_description$n_plots_missing_shrub_richness_any_after <- length(plot_na_remaining_shrub_sr_rows)

plot_na_remaining_tree_shrub_sr_rows = which(
  # na in all tree/shrub species richness cols
  long_plots0[,tree_shrub_sr.indicators] %>% is.na() %>% rowSums() > length(tree_shrub_sr.indicators)
)
results$plot_data_description$n_plots_missing_treeandshrub_richness_any_after <- length(plot_na_remaining_tree_shrub_sr_rows)

curation_notes_plots <- c(
  curation_notes_plots,
  "#### Missing data",
  "Missing data were dealt with differently depending on the indicator",
  "##### Tree/shrub species richness",
  paste0(
    "For ", results$plot_data_description$n_plots_missing_richness_any_before,
    " plots at least one tree/shrub species richness datum was missing. ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing tree species richness at ",
    length(na_tree_sr_rows_no_tree_info),
    " plots with entirely missing tree age class information.\n",
    "- Missing shrub species richness at ",
    length(na_shrub_sr_rows_no_shrub_info),
    " plots with missing shrub cover information, provided at least some data was recorded for tree age or tree species richness.\n",
    "- The further missing tree species richness at ",
    length(na_tree_sr_rows_some),
    " plots that recorded one of native or non-native tree species richness.\n",
    "- The further missing shrub species richness at ",
    length(na_shrub_sr_rows_some),
    " plots that recorded one of native or non-native shrub species richness.\n\n",
    "This left ",
    results$plot_data_description$n_plots_missing_tree_richness_any_after,
    " plots with no tree species richness information, ",
    results$plot_data_description$n_plots_missing_shrub_richness_any_after,
    " plots with no shrub species richness information and ",
    results$plot_data_description$n_plots_missing_treeandshrub_richness_any_after,
    " plots missing both).\n",
    "- Notably, all surveys missing tree/shrub species richness information after this process recorded some measurement of corresponding tree age class/shrub cover. ",
    
  )
)

# overall
overall_na_remaining_tree_sr_rows = which(
  # na in all tree species richness cols
  overall0[,tree_sr.indicators] %>% is.na() %>% rowSums() > 0
)
results$overall_data_description$n_surveys_missing_tree_richness_any_after <- length(overall_na_remaining_tree_sr_rows)
overall_na_remaining_shrub_sr_rows = which(
  # na in all shrub species richness cols
  overall0[,shrub_sr.indicators] %>% is.na() %>% rowSums() > 0
)
results$overall_data_description$n_surveys_missing_shrub_richness_any_after <- length(overall_na_remaining_shrub_sr_rows)
overall_na_remaining_tree_shrub_sr_rows = which(
  # na in all tree/shrub species richness cols
  overall0[,tree_shrub_sr.indicators] %>% is.na() %>% rowSums() > length(tree_shrub_sr.indicators)
)
# aa = overall0[overall_na_remaining_tree_shrub_sr_rows, ]
results$overall_data_description$n_surveys_missing_treeandshrub_richness_any_after <- length(overall_na_remaining_tree_shrub_sr_rows)

curation_notes_overall <- c(
  curation_notes_overall,
  "#### Missing data",
  "Missing data were dealt with differently depending on the indicator",
  "##### Tree/shrub species richness",
  paste0(
    "For ", results$overall_data_description$n_surveys_missing_richness_any_before,
    " surveys at least one tree/shrub species richness datum was missing. ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing tree species richness at ",
    length(overall_na_tree_sr_rows_no_tree_info),
    " surveys with entirely missing tree age class information.\n",
    "- Missing shrub species richness at ",
    length(overall_na_shrub_sr_rows_no_shrub_info),
    " surveys with missing shrub cover information, provided at least some data was recorded for tree age or tree species richness.\n",
    "- The further missing tree species richness at ",
    length(overall_na_tree_sr_rows_some),
    " surveys that recorded one of native or non-native tree species richness.\n",
    "- The further missing shrub species richness at ",
    length(overall_na_shrub_sr_rows_some),
    " surveys that recorded one of native or non-native shrub species richness.\n\n",
    "This left ",
    results$overall_data_description$n_surveys_missing_tree_richness_any_after,
    " surveys with no tree species richness information, ",
    results$overall_data_description$n_surveys_missing_shrub_richness_any_after,
    " surveys with no shrub species richness information and ",
    results$overall_data_description$n_surveys_missing_treeandshrub_richness_any_after,
    " surveys missing both).\n",
    "- Notably, all surveys missing tree/shrub species richness information after this process recorded some measurement of corresponding tree age class/shrub cover. ",
  )
)





# do the species richness totals
##plots
long_plots0 <- long_plots0 %>%
  mutate(
    tot_tree_SR = row_sum_na_if_all_na(
      TS.Native.richness,
      TS.Non.Native.richness
    ),
    
    tot_shrub_SR = row_sum_na_if_all_na(
      SS.Native.richness,
      SS.Non.Native.richness
    )
  )

## overall
overall0 <- overall0 %>%
  mutate(
    tot_tree_SR = row_sum_na_if_all_na(
      TS.Native.richness,
      TS.Non.Native.richness
    ),
    tot_shrub_SR = row_sum_na_if_all_na(
      SS.Native.richness,
      SS.Non.Native.richness
    ))

##### 4.2 Tree age indicators ----
###### 4.2.1 - where some age measurements present ----
results$plot_data_description$n_plots_missing_ta_any <- sum(
  rowSums(is.na(long_plots0[tree.ageclass.indicators])) > 0
)
results$plot_data_description$n_plots_missing_ta_all <- sum(
  rowSums(is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "tree_age"]])) > 0 &
    rowSums(!is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "tree_age"]])) == 0
)

long_plots0 <- long_plots0 %>% 
  fill_assumed_indicators(
    .,
    indicator_types$indicator [indicator_types$theme == "tree_age"],
    indicator_types
  ) 


###### 4.2.2 - where all age class info absent, but tree sr is 0 ----
na_ta_rows_0sr = which(
  # na in all tree age cols & total tree species richness is 0
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() == length(indicator_types$indicator [indicator_types$theme == "tree_age"]) &
    long_plots0$tot_tree_SR == 0
)

long_plots0[na_ta_rows_0sr, indicator_types$indicator [indicator_types$theme == "tree_age"]] <- "Absent"


###### 4.2.3 - where all age class info absent, but shrub cover or richness present ----
na_ta_rows_shrub = which(
  # na in all tree age cols & any shrub cover or shrub species richness info present
  long_plots0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0  &
    # tree sr not recorded as >0
    
    
    (
      (long_plots0[,  shrub.cover.indicators] %>% is.na() ==F) |
        (long_plots0$tot_shrub_SR %>% is.na() == F)
    )
)
long_plots0[na_ta_rows_shrub, indicator_types$indicator [indicator_types$theme == "tree_age"]] <- "Absent"

###### 4.2.4 - where all age class info absent, but regeneration level or regeneration species info present ----
na_ta_rows_regen = which(
  # na in all tree age cols & any regeneration level or regeneration species richness info present
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() == sum(indicator_types$theme == "tree_age")  &
    ((long_plots0[,indicator_types$indicator [indicator_types$theme == "regeneration_level"]] %>% is.na() %>% rowSums() < sum(indicator_types$theme == "regeneration_level") ) |
       (long_plots0[,indicator_types$indicator [indicator_types$theme == "regeneration_species_richness"]] %>% is.na() %>% rowSums() < sum(indicator_types$theme == "regeneration_species_richness") ))
)

long_plots0[na_ta_rows_regen, indicator_types$indicator [indicator_types$theme == "tree_age"]] <- "Absent" 

###### 4.2.XX - curation notes etc ----

na_remaining_tree_age_rows = which(
  # na in all tree age cols
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() > 0
)

results$plot_data_description$n_plots_missing_ta_after <- length(na_remaining_tree_age_rows)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Tree age class information",
  paste0(
    "For ", results$plot_data_description$n_plots_missing_ta_any,
    " plots at least one tree age datum was missing. ",
    "Implied absences were assumed in the following cases:\n\n",
    "- For ",
    results$plot_data_description$n_plots_missing_ta_any - results$plot_data_description$n_plots_missing_ta_all,
    " plots with at least one tree age class indicator recorded.\n",
    "- The further ", length(na_ta_rows_0sr),
    " plots with tree species richness as 0.\n",
    "- The further ", length(na_ta_rows_shrub),
    " plots containing shrub cover or shrub species richness information.\n",
    "- The further ", length(na_ta_rows_regen),
    " plots containing information on the level or species richness of regenerating trees or shrubs.\n\n",
    "This left ",
    results$plot_data_description$n_plots_missing_ta_after,
    " plots with no tree age information."
  )
)


##### 4.3 Shrub cover  ----
####### 4.3.1 - shrub cover absent but shrub sr 0 ----

results$plot_data_description$n_plots_missing_shrub_cover_any <- sum(
  is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "shrub_cover"]]))

na_shrub_cover_rows_0sr = which(
  # na in all shrub cover cols & total shrub species richness is 0
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()  &
    long_plots0$tot_shrub_SR == 0
)
long_plots0[na_shrub_cover_rows_0sr, indicator_types$indicator [indicator_types$theme == "shrub_cover"]] <- "Absent"


####### 4.3.2 - shrub cover absent but tree age or species info present ----
na_shrub_cover_rows_ta_sr = which(
  # na in all shrub cover cols & any tree age class or tree species info present
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()  &
    (!is.na(long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] ) %>% rowSums() > 0 |
       !is.na(long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_shrub_species_richness"]]) %>% rowSums() > 0
    )
)
long_plots0[na_shrub_cover_rows_ta_sr, indicator_types$indicator [indicator_types$theme == "shrub_cover"]] <- "Absent"


###### 4.3.XX - curation notes etc ----
na_remaining_shrub_cover_rows = which(
  # na in all shrub cover cols
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()
)
results$plot_data_description$n_plots_missing_shrub_cover_after <- length(na_remaining_shrub_cover_rows)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Shrub cover information",
  paste0(
    "For ", results$plot_data_description$n_plots_missing_shrub_cover_any,
    " plots shrub cover information was missing. ",
    "Implied absences were assumed in the following cases:\n\n",
    "- For ", length(na_shrub_cover_rows_0sr),
    " plots where total shrub species richness was recorded as 0.\n",
    "- The further ", length(na_shrub_cover_rows_ta_sr),
    " plots containing tree age class or tree species richness information.\n\n",
    "This left ",
    results$plot_data_description$n_plots_missing_shrub_cover_after,
    " plots with no shrub cover information."
  )
)


##### 4.4 Regeneration species richness  ----
regen.species.indicators <- indicator_types$indicator[indicator_types$theme == "regeneration_species_richness" &
                                                        indicator_types$type %in% c("numeric", "richness")] 

results$plot_data_description$n.missing.regen.species_before <- sum(
  rowSums(is.na(long_plots0[regen.species.indicators])) > 0
)

###### 4.4.1 - where all regen tree/shrub species richness info absent, and corresponding  tree/shrub regen level absent ----

# trees
na_treeregen_sr_rows_no_treeregen_level <- which(
  # na in all tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() == length(tree.regen.sr.indicators) &
    # na in all tree regen level cols
    !has_indicator(long_plots0, tree.regen.level.indicators)
)
long_plots0[na_treeregen_sr_rows_no_treeregen_level, tree.regen.sr.indicators] <- 0

# shrubs
na_shrubregen_sr_rows_no_shrubregen_level <- which(
  # na in all shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.sr.indicators) &
    # na in all shrub regen level cols
    !has_indicator(long_plots0, shrub.regen.level.indicators) 
)
long_plots0[na_shrubregen_sr_rows_no_shrubregen_level, shrub.regen.sr.indicators] <- 0


###### 4.4.2 - where only one of native/Non.Native filled for regen species richness ---- ----

# trees
na_treeregen_sr_rows_some = which(
  # na in some tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() < length(tree.regen.sr.indicators))
# replace those nas with 0
long_plots0[na_treeregen_sr_rows_some, tree.regen.sr.indicators] <-
  long_plots0[na_treeregen_sr_rows_some, tree.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

# shrubs
na_shrubregen_sr_rows_some = which(
  # na in some shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() < length(shrub.regen.sr.indicators))
# replace those nas with 0
long_plots0[na_shrubregen_sr_rows_some, shrub.regen.sr.indicators] <-
  long_plots0[na_shrubregen_sr_rows_some, shrub.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
         

###### 4.4.XX - curation notes etc ----

na_remainging_tree_regen_sr_rows = which(
  # na in all tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() == length(tree.regen.sr.indicators)
)
na_remainging_shrub_regen_sr_rows = which(
  # na in all shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.sr.indicators)
)

na_remaining_regen_sr_rows = which(
  # na in all regen species richness cols
  long_plots0[,regen.species.indicators] %>% is.na() %>% rowSums() == length(regen.species.indicators)
)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Regeneration species richness",
  paste0(
    "For ", results$plot_data_description$n.missing.regen.species_before,
    " plots at least one regeneration species richness datum was missing. ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing regenerating tree species richness at ",
    length(na_treeregen_sr_rows_no_treeregen_level),
    " plots where tree regeneration level was entirely missing.\n",
    "- The further missing regenerating tree species richness at ",
    length(na_treeregen_sr_rows_some),
    " plots that recorded only one of native or non-native tree regeneration species richness.\n",
    "- Missing regenerating shrub species richness at ",
    length(na_shrubregen_sr_rows_no_shrubregen_level),
    " plots where shrub regeneration level was entirely missing.\n",
    "- The further missing regenerating shrub species richness at ",
    length(na_shrubregen_sr_rows_some),
    " plots that recorded only one of native or non-native shrub regeneration species richness.\n\n",
    "This left ",
    length(na_remainging_tree_regen_sr_rows),
    " plots with no tree regeneration species richness information, ",
    length(na_remainging_shrub_regen_sr_rows),
    " plots with no shrub regeneration species richness information and ",
    length(na_remaining_regen_sr_rows),
    " plots with neither."
  )
)


long_plots0 <- long_plots0 %>%
  mutate(
    tot_tree_regn_SR = row_sum_na_if_all_na(
      RTS.Native.richness,
      RTS.Non.Native.richness
    ),
    
    tot_shrub_regen_SR = row_sum_na_if_all_na(
      RSS.Native.richness,
      RSS.Non.Native.richness
    )
  )

##### 4.5 regen level ----

results$plot_data_description$n.missing.regen.level_before <- sum(
  rowSums(is.na(long_plots0[regen.level.indicators])) > 0
)
results$plot_data_description$n.missing.treeregen.level_before <- sum(
  rowSums(is.na(long_plots0[tree.regen.level.indicators])) > 0
)
results$plot_data_description$n.missing.shrubregen.level_before <- sum(
  rowSums(is.na(long_plots0[shrub.regen.level.indicators])) > 0
)


###### 4.5.1 - where some regen level measurements present ----

na_regen_level_rows_some = which(
  # na in some regen level cols
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators)
)

na_treeregen_level_rows_some = which(
  # na in some regen level cols
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() < length(tree.regen.level.indicators)
)

na_shrubregen_level_rows_some = which(
  # na in some regen level cols
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() < length(shrub.regen.level.indicators)
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    tree.regen.level.indicators,
    indicator_types
  ) %>%
  fill_assumed_indicators(
    .,
    shrub.regen.level.indicators,
    indicator_types
  )

# long_plots0 <- long_plots0 %>%
#   fill_assumed_indicators(
#     .,
#     regen.level.indicators,
#     indicator_types
#   )


###### 4.5.2 - where regen level info absent, but tree/shrub regen sr is 0  -----
# note where both were originally missing, sr will now be 0 


# either
na_regen_level_rows_0regensr_either = which(
  # na in all regen level cols & either tree or shrub regen species richness is 0
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators) &
    (long_plots0$tot_tree_regn_SR == 0 | long_plots0$tot_shrub_regen_SR == 0)
)
# tree
na_treeregen_level_rows_0treeregen_sr = which(
  # na in all tree regen level cols & tree regen species richness is 0
  (long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() == 
    length(tree.regen.level.indicators)) &
    long_plots0$tot_tree_regn_SR == 0 
)
long_plots0[na_treeregen_level_rows_0treeregen_sr, tree.regen.level.indicators] <- "Absent"

# shrub
na_shrubregen_level_rows_0shrubregen_sr = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.level.indicators) &
    long_plots0$tot_shrub_regen_SR == 0
)
long_plots0[na_shrubregen_level_rows_0shrubregen_sr, shrub.regen.level.indicators] <- "Absent"
  

###### 4.5.XX  - curation notes etc ----

na_treeregen_level_after = which(
  # na in any tree regen level cols & tree regen species richness is 0
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_treeregen_level_after_sr <- long_plots0$tot_tree_regn_SR[na_treeregen_level_after]

na_shrubregen_level_after = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_shrubregen_level_after_sr <- long_plots0$tot_shrub_regen_SR[na_shrubregen_level_after]

na_anyregen_level_after = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0
)

na_allregen_level_after = which(
  # na in all regen level cols
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators)
)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Regeneration level information",
  paste0(
    "For ", results$plot_data_description$n.missing.regen.level_before,
    " plots at least one regeneration level datum was missing (",
    results$plot_data_description$n.missing.treeregen.level_before,
    " missing tree regeneration level and ",
    results$plot_data_description$n.missing.shrubregen.level_before,
    " missing shrub regeneration level). ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing tree regeneration level at ",
    length(na_treeregen_level_rows_some),
    " plots that recorded some (but not all) tree regeneration levels.\n",
    "- The further missing tree regeneration level at ",
    length(na_treeregen_level_rows_0treeregen_sr),
    " plots that recorded zero total species richness of regenerating trees.\n",
    "- Missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_some),
    " plots that recorded some (but not all) shrub regeneration levels.\n",
    "- The further missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_0shrubregen_sr),
    " plots that recorded zero total species richness of regenerating shrubs.\n\n",
    "This left ",
    length(na_anyregen_level_after),
    " plots with some missing regeneration level information (",
    length(na_treeregen_level_after),
    " missing tree regeneration level and ",
    length(na_shrubregen_level_after),
    " missing shrub regeneration level).\n",
    "Note: Of the plots with missing tree regeneration level, ",
    length(na_treeregen_level_after_sr[na_treeregen_level_after_sr > 0]),
    " recorded tree regeneration species richness >1, and of the plots with missing shrub regeneration level, ",
    length(na_shrubregen_level_after_sr),
    " recorded shrub regeneration species richness >1."
  )
)


##### 4.6 Flora and Deadwood indicators ----

flora.indicators <- indicator_types$indicator[indicator_types$theme == "flora"]
deadwood.indicators <- indicator_types$indicator[indicator_types$theme == "deadwood"]

flora.deadwood.indicators <- c(flora.indicators, deadwood.indicators)

results$plot_data_description$n.missing.flora_before <- sum(
  rowSums(is.na(long_plots0[flora.indicators])) > 0
)
results$plot_data_description$n.missing.deadwood_before <- sum(
  rowSums(is.na(long_plots0[deadwood.indicators])) > 0
)
results$plot_data_description$n.missing.flora_deadwood_before <- sum(
  rowSums(is.na(long_plots0[flora.deadwood.indicators])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    flora.deadwood.indicators,
    indicator_types
  )

results$plot_data_description$n.missing.flora_after <- sum(
  rowSums(is.na(long_plots0[flora.indicators])) > 0
)
results$plot_data_description$n.missing.deadwood_after <- sum(
  rowSums(is.na(long_plots0[deadwood.indicators])) > 0
)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Features information (flora and deadwood)",
  paste0(
    "For ", results$plot_data_description$n.missing.flora_deadwood_before,
    " plots at least one datum was missing from flora (",
    results$plot_data_description$n.missing.flora_before,
    " plots) or deadwood (",
    results$plot_data_description$n.missing.deadwood_before,
    " plots). ",
    "Implied absences were assumed in the following cases:\n\n",
    "- ", results$plot_data_description$n.missing.flora_deadwood_before - results$plot_data_description$n.missing.flora_after,
    " plots with data entered for at least one of these 'features' indicators (",
    results$plot_data_description$n.missing.flora_before - results$plot_data_description$n.missing.flora_after,
    " plots filled for flora and ",
    results$plot_data_description$n.missing.deadwood_before - results$plot_data_description$n.missing.flora_after,
    " for deadwood).\n\n",
    "This left ",
    results$plot_data_description$n.missing.flora_after,
    " plots with no flora or deadwood information."
  )
)

##### 4.7 Invasives, Animal damage, Human impact or Tree health: contains "Invasives.", "Animal.Damage.", "Human.Impacts.", "Tree.Health." ----
invasives.indicators <- indicator_types$indicator[indicator_types$theme == "invasives"]
animal_damage.indicators <- indicator_types$indicator[indicator_types$theme == "animal_damage"]
human_impact.indicators <- indicator_types$indicator[indicator_types$theme == "human_impact"]
tree_health.indicators <- indicator_types$indicator[indicator_types$theme == "tree_health"]

threats.indicators <- c(
  invasives.indicators,
  animal_damage.indicators,
  human_impact.indicators,
  tree_health.indicators
)

results$plot_data_description$n.missing.invasives_before <- sum(
  rowSums(is.na(long_plots0[invasives.indicators])) > 0
)
results$plot_data_description$n.missing.animal_damage_before <- sum(
  rowSums(is.na(long_plots0[animal_damage.indicators])) > 0
)
results$plot_data_description$n.missing.human_impact_before <- sum(
  rowSums(is.na(long_plots0[human_impact.indicators])) > 0
)
results$plot_data_description$n.missing.tree_health_before <- sum(
  rowSums(is.na(long_plots0[tree_health.indicators])) > 0
)
results$plot_data_description$n.missing.threats_before <- sum(
  rowSums(is.na(long_plots0[threats.indicators])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    threats.indicators,
    indicator_types
  )

results$plot_data_description$n.missing.invasives_after <- sum(
  rowSums(is.na(long_plots0[invasives.indicators])) > 0
)
results$plot_data_description$n.missing.animal_damage_after <- sum(
  rowSums(is.na(long_plots0[animal_damage.indicators])) > 0
)
results$plot_data_description$n.missing.human_impact_after <- sum(
  rowSums(is.na(long_plots0[human_impact.indicators])) > 0
)
results$plot_data_description$n.missing.tree_health_after <- sum(
  rowSums(is.na(long_plots0[tree_health.indicators])) > 0
)

results$plot_data_description$n.missing.threats_after <- sum(
  rowSums(is.na(long_plots0[threats.indicators])) > 0
)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Threats information (invasives, animal damage, human impact and tree health)",
  paste0(
    "For ", results$plot_data_description$n.missing.threats_before,
    " plots at least one datum was missing across threats, including: invasives (",
    results$plot_data_description$n.missing.invasives_before,
    " plots), animal damage (",
    results$plot_data_description$n.missing.animal_damage_before,
    " plots), human impact (",
    results$plot_data_description$n.missing.human_impact_before,
    " plots) and tree health (",
    results$plot_data_description$n.missing.tree_health_before,
    " plots). ",
    "Implied absences were assumed in the following cases:\n\n",
    "- ", results$plot_data_description$n.missing.threats_before - results$plot_data_description$n.missing.human_impact_after,
    " plots with data entered for at least one of these 'threats' indicators (",
    results$plot_data_description$n.missing.invasives_before - results$plot_data_description$n.missing.invasives_after,
    " for invasives; ",
    results$plot_data_description$n.missing.animal_damage_before - results$plot_data_description$n.missing.animal_damage_after,
    " for animal damage; ",
    results$plot_data_description$n.missing.human_impact_before - results$plot_data_description$n.missing.human_impact_after,
    " for human impact; ",
    results$plot_data_description$n.missing.tree_health_before - results$plot_data_description$n.missing.tree_health_after,
    " for tree health).\n\n",
    "This left ",
    results$plot_data_description$n.missing.invasives_after,
    " plots with no information on threats."
  )
)

##### 4.8 remaining empty feature or threat indicators ----

results$plot_data_description$n.missing.features_threats_before <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    c(flora.deadwood.indicators, threats.indicators),
    indicator_types
  )

results$plot_data_description$n.missing.features_threats_after <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)
curation_notes_plots <- c(
  curation_notes_plots,
  "##### Remaining features and threats information",
  paste0(
    "Of the ", results$plot_data_description$n.missing.features_threats_before,
    " plots remaining that contained missing data across features (N = ",
    results$plot_data_description$n.missing.deadwood_after,
    ") or threats (N = ",
    results$plot_data_description$n.missing.invasives_after,
    "), ",
    results$plot_data_description$n.missing.features_threats_before - results$plot_data_description$n.missing.features_threats_after,
    " plots contained information on at least one of those indicators. ",
    "These were assumed to be implied absences. \n",
    "This left ",
    results$plot_data_description$n.missing.features_threats_after,
    " plots with no information on features or threats."
  )
)


##### 
# 5. fix "Dominated.By.One.Or.Two.SPP" info ----
## "Dominated.By.One.Or.Two.SPP" is never NA - where blank it is "no", even where no corresponeding species are recorded (eitehr 0 or NA)
##### set to NA where corresponding species richness is 0 or NA

results$plot_data_description$n.fixed_dominated_rts <- sum(
  long_plots0$RTS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_regn_SR == 0 |
                                                is.na(long_plots0$tot_tree_regn_SR)] == FALSE,
  na.rm = TRUE
)
results$plot_data_description$n.fixed_dominated_rss <- sum(
  long_plots0$RSS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_shrub_regen_SR == 0 |
                                                is.na(long_plots0$tot_shrub_regen_SR)] == FALSE,
  na.rm = TRUE
)
results$plot_data_description$n.fixed_dominated_ts <- sum(
  long_plots0$TS.Canopy.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_SR == 0 |
                                                      is.na(long_plots0$tot_tree_SR)] == FALSE,
  na.rm = TRUE
)
results$plot_data_description$n.fixed_dominated_ss <- sum(
  long_plots0$SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP[long_plots0$tot_shrub_SR == 0 |
                                                           is.na(long_plots0$tot_shrub_SR)] == FALSE,
  na.rm = TRUE
)
long_plots0$RTS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_regn_SR == 0 |
                                              is.na(long_plots0$tot_tree_regn_SR)] <- NA
long_plots0$RSS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_shrub_regen_SR == 0 |
                                              is.na(long_plots0$tot_shrub_regen_SR)] <- NA
long_plots0$TS.Canopy.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_SR == 0 |
                                                    is.na(long_plots0$tot_tree_SR)] <- NA
long_plots0$SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP[long_plots0$tot_shrub_SR == 0 |
                                                         is.na(long_plots0$tot_shrub_SR)] <- NA

curation_notes_plots <- c(
  curation_notes_plots,
  "#### 'Dominated by one or two species' indicators - inappropraite default value",
  paste0(
    "Orignally 'Dominated by one or two species', indicators were FALSE by default and manually overwritten to TRUE when needed.\n ",
    "- This left FALSE values even when records of zeo or NA species richness implied the indicator should be NA.", 
    " In those cases (",
    results$plot_data_description$n.fixed_dominated_rts, " for renerating trees, ", 
    results$plot_data_description$n.fixed_dominated_rss, " for regenerating shrubs, ", 
    results$plot_data_description$n.fixed_dominated_ts, " for tree species, ",
    results$plot_data_description$n.fixed_dominated_ss, " for shrub species",
    "), the indicator was changed to 'NA'"
  )
)


# 6. Understand what missing data remains ----

# sum missing indicators (except for dominated indicators)
long_plots0$na_count <- rowSums(is.na(long_plots0[indicator_types$indicator[!indicator_types$indicator %in% c(
  "RTS.Dominated.By.One.Or.Two.SPP",
  "RSS.Dominated.By.One.Or.Two.SPP",
  "TS.Canopy.Dominated.By.One.Or.Two.SPP",
  "SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP"
)]]))


#filter out any with NA indicators
# aa <- long_plots0 %>% 
#   filter(
#     na_count > 0
#   )

## table of numbers of plots with NA for each indicator type ()
missing_tbl <- long_plots0[, indicator_types$indicator] %>%
  is.na() %>%
  colSums() %>% 
  enframe(
    .,
    name  = "indicator",
    value = "n_missing"
  )

missing_by_indicator <- indicator_types %>%
  select(indicator, theme, type) %>%
  left_join(missing_tbl, by = "indicator")


curation_notes_plots <- c(
  curation_notes_plots,
  "### Curated plot data description",
  paste0(
    "The curated dataset contained information from:\n\n",
    "- ", nrow(long_plots0), " plots\n",
    "- ", length(unique(long_plots0$id)), " individual surveys\n",
    "- ", length(unique(paste(long_plots0$Site.Name, long_plots0$Stratum ))), " individual strata\n",
    "- ", length(unique(long_plots0$Site.Name)), " individual sites\n",
    "- Covering ", 
    format(min(as.Date(long_plots0$ActualObservationDate, format = "%d/%m/%Y"), na.rm = TRUE), "%Y"),
    " to ",
    format(max(as.Date(long_plots0$ActualObservationDate, format = "%d/%m/%Y"), na.rm = TRUE), "%Y"),
    ".\n\n",
    "Of these, ", sum(long_plots0$na_count > 0), " plots (",
    round(100 * sum(long_plots0$na_count > 0) / nrow(long_plots0), 2), "%) still contained missing data for at least one indicator:\n\n",
    "- Tree species richness: ", results$plot_data_description$n_plots_missing_tree_richness_any_after, " plots missing data (",
    round(100 * results$plot_data_description$n_plots_missing_tree_richness_any_after / nrow(long_plots0), 2), "%)\n",
    "- Shrub species richness: ", results$plot_data_description$n_plots_missing_shrub_richness_any_after, " plots missing data (",
    round(100 * results$plot_data_description$n_plots_missing_shrub_richness_any_after / nrow(long_plots0), 2), "%)\n",
    "- Tree age category covers: ", results$plot_data_description$n_plots_missing_ta_after, " plots missing data (",
    round(100 * results$plot_data_description$n_plots_missing_ta_after / nrow(long_plots0), 2), "%)\n",
    "- Shrub cover: ", results$plot_data_description$n_plots_missing_shrub_cover_after, " plots missing data (",
    round(100 * results$plot_data_description$n_plots_missing_shrub_cover_after / nrow(long_plots0), 2), "%)\n",
    "- Regeneration tree species richness: ", length(na_remainging_tree_regen_sr_rows), " plots missing data (",
    round(100 * length(na_remainging_tree_regen_sr_rows) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration shrub species richness: ", length(na_remainging_shrub_regen_sr_rows), " plots missing data (",
    round(100 * length(na_remainging_shrub_regen_sr_rows) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration level (trees): ", length(na_treeregen_level_after), " plots missing data (",
    round(100 * length(na_treeregen_level_after) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration level (shrubs): ", length(na_shrubregen_level_after), " plots missing data (",
    round(100 * length(na_shrubregen_level_after) / nrow(long_plots0), 2), "%)\n",
    "- Features and threats indicators: ", results$plot_data_description$n.missing.features_threats_after, " plots missing data (",
    round(100 * results$plot_data_description$n.missing.features_threats_after / nrow(long_plots0), 2), "%)"
  )
)



# end ----

long_plots <- long_plots0 
rm(long_plots0)