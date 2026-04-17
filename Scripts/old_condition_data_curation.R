# Curation of old condition data prior to indicator calculation



overall0 <- overall
long_plots0 <- long_plots

long_plots0$plot.uid <- 1:nrow(long_plots0)
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
regen.sr.indicators <- c(tree.regen.sr.indicators, shrub.regen.sr.indicators)

tree_sr.indicators <- c("TS.Native.richness", "TS.Non.Native.richness")
shrub_sr.indicators <- c("SS.Native.richness", "SS.Non.Native.richness")
tree_shrub_sr.indicators <- c(tree_sr.indicators, shrub_sr.indicators)

richness_indicators <- c(tree_sr.indicators, shrub_sr.indicators, tree.regen.sr.indicators, shrub.regen.sr.indicators)

flora.indicators <- indicator_types$indicator[indicator_types$theme == "flora"]
deadwood.indicators <- indicator_types$indicator[indicator_types$theme == "deadwood"]

flora.deadwood.indicators <- c(flora.indicators, deadwood.indicators)

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

# 0. initial curation of data - remove missing ----

## find where no data recorded, across - tree.ageclass.indicators, shrub.cover.indicators, regen.level.indicators, richness_indicators, flora.deadwood.indicators, threats.indicators
data_cols <- c(tree.ageclass.indicators,
               shrub.cover.indicators,
               regen.level.indicators,
               richness_indicators,
               flora.deadwood.indicators,
               threats.indicators)
no_data_plot_id <- long_plots0$plot.uid[which(
  long_plots0[, data_cols] %>% is.na() %>% rowSums() == length(data_cols)
)]
long_plots0$no_data <- F
long_plots0$no_data[long_plots0$plot.uid %in% no_data_plot_id] <- T

no_data_overall_id <- overall0$id[which(
  overall0[, data_cols] %>% is.na() %>% rowSums() == length(data_cols)
)]

no_data_overall_and_plot_id <- no_data_overall_id[no_data_overall_id %in% long_plots0$id[long_plots0$no_data]]

## remove missing data plots
long_plots0 <- long_plots0[long_plots0$no_data == F, ]

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
  "\n\n### Plot data curation",
  paste0( "The initial plot data contained information from ", results$plot_data_description$nplots_og, " plots, covering ", results$plot_data_description$n_surveys_og, " surveys over ",  results$plot_data_description$nplot_sites_og, " sites." )
)

curation_notes_overall <- c(
  curation_notes_overall,
  "\n\n### Overall data curation",
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
# 1. info from plots ----
## 1.1  overall species richness was sometimes the total of the relevent species richnesses gathered across plots ----
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
suspicious.thresh <- 1

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


overall0$sr_summed_plots_oops <- F
overall0$sr_summed_plots_oops[survey_rows_summing_plot_sr] <- T

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
  
## 1.2 overall species richness sometimes less than the max of the plots, which is impossible - change to the max of the plots ----
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

overall0$overall_sr_less_than_plot_max <- rowSums(sr_less_than_plot_max, na.rm = TRUE) >0


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



## 1.3 checking for if surveys are split over multiple rows ----
overall0_poss.dup_or_multi_line_survey_ids <- overall0$id[duplicated(paste0(overall0$Site.Name, overall0$Stratum, year(overall0$ActualObservationDate)))|
                                                 duplicated(paste0(overall0$Site.Name, overall0$Stratum, year(overall0$ActualObservationDate)), fromLast = TRUE)]
overall0_poss_dups <- overall0[overall0$id %in% overall0_poss.dup_or_multi_line_survey_ids,]


## 1.XX washup ----

overall0$sr_changed_in_curation <- overall0$overall_sr_less_than_plot_max | overall0$sr_summed_plots_oops

overall0 <- overall0 %>%
  select(-ends_with(".matches_plot_sum_not_max")) %>%  # remove the T/F flags as they are no longer relevant after correction
  select(-starts_with("plot_tot_")) %>%  # remove the plot total columns as they are no longer relevant after correctione
  select(-starts_with("plot_max_"))  # remove the plot max columns as they are no longer relevant after correction

# 2. remove rows with NA across all tree related information (age class cover, regeneration, species count) ----
## identify tree rows
tree_cols <- indicator_types$indicator[indicator_types$theme %in% c("tree_age", "tree_shrub_species_richness", 
                                                                         "shrub_cover", 
                                                                         "regeneration_level", "regeneration_species_richness")]
tree_cols <- tree_cols[!grepl("Dominated", tree_cols)] # remove those containing "Dominated"

## identify survey ids where all NA for those tree related columns
plot_na_tree_id = long_plots0$plot.uid[ which(
  long_plots0[,tree_cols] %>% is.na() %>% rowSums() == length(tree_cols)
)]
long_plots0$tree_info_present <- T
long_plots0$tree_info_present[long_plots0$plot.uid %in% plot_na_tree_id] <- F

long_plots0_no_treeinfo <- long_plots0[long_plots0$tree_info_present == F, ]


overall_na_tree_id = overall0$id[which(
  overall0[,tree_cols] %>% is.na() %>% rowSums() == length(tree_cols)
)]
overall0$tree_info_present <- T
overall0$tree_info_present[overall0$id %in% overall_na_tree_id] <- F

# which ids have tree info at plots but not overall level
plot_but_no_overall_treeinfo_id <- long_plots0$id[long_plots0$id[long_plots0$tree_info_present] %in% 
                                                    overall0$id[overall0$tree_info_present == F] ] %>% 
  unique()
# which ids have overall but not plot tree info
overall_but_no_plot_treeinfo_id <- overall0$id[overall0$id[overall0$tree_info_present] %in% 
                                                    long_plots0$id[long_plots0$tree_info_present == F] ] %>% 
  unique()
# which ids have tree info at overall or plot level
overall_or_plot_tree_info_id <- unique(c(overall0$id[overall0$tree_info_present], 
                                         long_plots0$id[long_plots0$tree_info_present]))

# remove plots with no tree info, and overall with no tree info at overall or plot level
long_plots0 <- long_plots0[long_plots0$tree_info_present, ]
## remove overall surveys where no tree id at overall or plot level
overall0 <- overall0[overall0$id %in% overall_or_plot_tree_info_id, ]

## 2.XX curation description ----
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
    has_indicator(long_plots0, tree.ageclass.indicators)
)
long_plots0$TS.Native.richness[plot_wrong_zero_ts] <- NA
long_plots0$TS.Non.Native.richness[plot_wrong_zero_ts] <- NA

plot_wrong_zero_ss <- which(
  row_sum_na_if_all_na(long_plots0$SS.Native.richness,  long_plots0$SS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, shrub.cover.indicators)
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
    has_indicator(overall0, tree.ageclass.indicators)
)
overall0$TS.Native.richness[overall_wrong_zero_ts] <- NA
overall0$TS.Non.Native.richness[overall_wrong_zero_ts] <- NA

overall_wrong_zero_ss <- which(
  row_sum_na_if_all_na(overall0$SS.Native.richness,  overall0$SS.Non.Native.richness) == 0 &
    has_indicator(overall0, shrub.cover.indicators)
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
    "- Notably, all plots missing tree/shrub species richness information after this process recorded some measurement of corresponding tree age class/shrub cover. "
    
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
    "- Notably, all surveys missing tree/shrub species richness information after this process recorded some measurement of corresponding tree age class/shrub cover. "
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

# plots
results$plot_data_description$n_plots_missing_ta_any <- sum(
  rowSums(is.na(long_plots0[tree.ageclass.indicators])) > 0
)
results$plot_data_description$n_plots_missing_ta_all <- sum(
  rowSums(is.na(long_plots0[tree.ageclass.indicators])) > 0 &
    rowSums(!is.na(long_plots0[tree.ageclass.indicators])) == 0
)

long_plots0 <- long_plots0 %>% 
  fill_assumed_indicators(
    .,
    tree.ageclass.indicators,
    indicator_types
  ) 

# overall
results$overall_data_description$n_surveys_missing_ta_any <- sum(
  rowSums(is.na(overall0[tree.ageclass.indicators])) > 0
)
results$overall_data_description$n_surveys_missing_ta_all <- sum(
  rowSums(is.na(overall0[tree.ageclass.indicators])) > 0 &
    rowSums(!is.na(overall0[tree.ageclass.indicators])) == 0
)
overall0 <- overall0 %>% 
  fill_assumed_indicators(
    .,
    tree.ageclass.indicators,
    indicator_types
  )

###### 4.2.2 - where all age class info absent, but tree sr is 0 ----
# plots
na_ta_rows_0sr_plot = which(
  # na in all tree age cols & total tree species richness is 0
  long_plots0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() == length(tree.ageclass.indicators) &
    long_plots0$tot_tree_SR == 0
)

long_plots0[na_ta_rows_0sr_plot, tree.ageclass.indicators] <- "Absent"

# overall
na_ta_rows_0sr_overall = which(
  # na in all tree age cols & total tree species richness is 0
  overall0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() == length(tree.ageclass.indicators) &
    overall0$tot_tree_SR == 0
)
overall0[na_ta_rows_0sr_overall, tree.ageclass.indicators] <- "Absent"

###### 4.2.3 - where all age class info absent, but shrub cover or richness present, but tree sr not recorded as >0 ----
#plots
na_ta_rows_shrub_plot = which(
  # na in all tree age cols & any shrub cover or shrub species richness info present
  long_plots0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0  &
    # tree sr not recorded as >0
    !long_plots0$tot_tree_SR >0 &
    # but some shrub cover or shrub species richness info present - support that something WAS measured, and therefore NA is likely an implied absence
    (
      (long_plots0[,  shrub.cover.indicators] %>% is.na() ==F) | #shrub cover present
        (long_plots0$tot_shrub_SR %>% is.na() == F) # shrub species richness present
    )
)
long_plots0[na_ta_rows_shrub_plot, tree.ageclass.indicators] <- "Absent"

# overall
na_ta_rows_shrub_overall = which(
  # na in all tree age cols & any shrub cover or shrub species richness info present
  overall0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0  &
    # tree sr not recorded as >0
    !overall0$tot_tree_SR >0 &
    # but some shrub cover or shrub species richness info present - support that something WAS measured,
    (
      (overall0[,  shrub.cover.indicators] %>% is.na() ==F) |
        (overall0$tot_shrub_SR %>% is.na() == F)
    )
)
overall0[na_ta_rows_shrub_overall, tree.ageclass.indicators] <- "Absent"

###### 4.2.4 - where all age class info absent, but regeneration level or regeneration species info present, but tree tree sr not recorded as >0 ----
# plots
na_ta_rows_regen_plot = which(
  # na in all tree age cols & any shrub cover or shrub species richness info present
  long_plots0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0  &
    # tree sr not recorded as >0
    !long_plots0$tot_tree_SR >0 &
    # any regeneration level or regeneration species richness info present
    ((long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators) ) |
       (long_plots0[,regen.sr.indicators] %>% is.na() %>% rowSums() < length(regen.sr.indicators) ))
)

long_plots0[na_ta_rows_regen_plot, tree.ageclass.indicators] <- "Absent" 

# overall
na_ta_rows_regen_overall = which(
  # na in all tree age cols & any shrub cover or shrub species richness info present
  overall0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0  &
    # tree sr not recorded as >0
    !overall0$tot_tree_SR >0 &
    # any regeneration level or regeneration species richness info present
    ((overall0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators) ) |
       (overall0[,regen.sr.indicators] %>% is.na() %>% rowSums() < sum(indicator_types$theme == "regeneration_species_richness") ))
)
overall0[na_ta_rows_regen_overall, tree.ageclass.indicators] <- "Absent"
###### 4.2.XX - curation notes etc ----

# plots
na_remaining_tree_age_rows_plot = which(
  # na in all tree age cols
  long_plots0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0
)
n_na_remaining_tree_age_rows_plot_bigger_than_0 <- sum(
  rowSums(is.na(long_plots0[,tree.ageclass.indicators])) > 0
)

results$plot_data_description$n_plots_missing_ta_after <- length(na_remaining_tree_age_rows_plot)

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
    "- The further ", length(na_ta_rows_0sr_plot),
    " plots with tree species richness as 0.\n",
    "- The further ", length(na_ta_rows_shrub_plot),
    " plots containing shrub cover or shrub species richness information.\n",
    "- The further ", length(na_ta_rows_regen_plot),
    " plots containing information on the level or species richness of regenerating trees or shrubs.\n\n",
    "This left ",
    results$plot_data_description$n_plots_missing_ta_after,
    " plots with no tree age information, ", n_na_remaining_tree_age_rows_plot_bigger_than_0, " of which recorded tree species richness > 0."
  )
)

# overall
na_remaining_tree_age_rows_overall = which(
  # na in all tree age cols
  overall0[,tree.ageclass.indicators] %>% is.na() %>% rowSums() > 0
)
n_na_remaining_tree_age_rows_overall_bigger_than_0 <- sum(
  rowSums(is.na(overall0[,tree.ageclass.indicators])) > 0
)
results$overall_data_description$n_surveys_missing_ta_after <- length(na_remaining_tree_age_rows_overall)

curation_notes_overall <- c(
  curation_notes_overall,
  "##### Tree age class information",
  paste0(
    "For ", results$overall_data_description$n_surveys_missing_ta_any,
    " surveys at least one tree age datum was missing. ",
    "Implied absences were assumed in the following cases:\n\n",
    "- For ",
    results$overall_data_description$n_surveys_missing_ta_any - results$overall_data_description$n_surveys_missing_ta_all,
    " surveys with at least one tree age class indicator recorded.\n",
    "- The further ", length(na_ta_rows_0sr_overall),
    " surveys with tree species richness as 0.\n",
    "- The further ", length(na_ta_rows_shrub_overall),
    " surveys containing shrub cover or shrub species richness information.\n",
    "- The further ", length(na_ta_rows_regen_overall),
    " surveys containing information on the level or species richness of regenerating trees or shrubs.\n\n",
    "This left ",
    results$overall_data_description$n_surveys_missing_ta_after,
    " surveys with no tree age information, ", n_na_remaining_tree_age_rows_overall_bigger_than_0, " of which recorded tree species richness > 0."
  )
)

##### 4.3 Shrub cover  ----
####### 4.3.1 - shrub cover absent but shrub sr 0 ----

results$plot_data_description$n_plots_missing_shrub_cover_any <- sum(
  is.na(long_plots0[shrub.cover.indicators]))

results$overall_data_description$n_surveys_missing_shrub_cover_any <- sum(
  is.na(overall0[shrub.cover.indicators]))


na_shrub_cover_rows_0sr_plot = which(
  # na in all shrub cover cols & total shrub species richness is 0
  long_plots0[,shrub.cover.indicators] %>% is.na()  &
    long_plots0$tot_shrub_SR == 0
)
long_plots0[na_shrub_cover_rows_0sr_plot, shrub.cover.indicators] <- "Absent"

na_shrub_cover_rows_0sr_overall = which(
  # na in all shrub cover cols & total shrub species richness is 0
  overall0[,shrub.cover.indicators] %>% is.na()  &
    overall0$tot_shrub_SR == 0
)
overall0[na_shrub_cover_rows_0sr_overall, shrub.cover.indicators] <- "Absent"

####### 4.3.2 - shrub cover absent but tree age or species info present ----
na_shrub_cover_rows_ta_sr_plot = which(
  # na in all shrub cover cols & any tree age class or tree species info present
  long_plots0[,shrub.cover.indicators] %>% is.na()  &
    (!is.na(long_plots0[,tree.ageclass.indicators] ) %>% rowSums() > 0 |
       !is.na(long_plots0[,tree_shrub_sr.indicators]) %>% rowSums() > 0
    )
)
long_plots0[na_shrub_cover_rows_ta_sr_plot, shrub.cover.indicators] <- "Absent"

na_shrub_cover_rows_ta_sr_overall = which(
  # na in all shrub cover cols & any tree age class or tree species info present
  overall0[,shrub.cover.indicators] %>% is.na()  &
    (!is.na(overall0[,tree.ageclass.indicators] ) %>% rowSums() > 0 |
       !is.na(overall0[,tree_shrub_sr.indicators]) %>% rowSums() > 0
    )
)
overall0[na_shrub_cover_rows_ta_sr_overall, shrub.cover.indicators]


###### 4.3.XX - curation notes etc ----
na_remaining_shrub_cover_rows_plot = which(
  # na in all shrub cover cols
  long_plots0[,shrub.cover.indicators] %>% is.na()
)
results$plot_data_description$n_plots_missing_shrub_cover_after <- length(na_remaining_shrub_cover_rows_plot)

na_remaining_shrub_cover_rows_overall = which(
  # na in all shrub cover cols
  overall0[,shrub.cover.indicators] %>% is.na()
)
results$overall_data_description$n_surveys_missing_shrub_cover_after <- length(na_remaining_shrub_cover_rows_overall)

# where did overall have absent shrub cover but plots have shrub cover

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Shrub cover information",
  paste0(
    "For ", results$plot_data_description$n_plots_missing_shrub_cover_any,
    " plots shrub cover information was missing. ",
    "Implied absences were assumed in the following cases:\n\n",
    "- For ", length(na_shrub_cover_rows_0sr_plot),
    " plots where total shrub species richness was recorded as 0.\n",
    "- The further ", length(na_shrub_cover_rows_ta_sr_plot),
    " plots containing tree age class or tree species richness information.\n\n",
    "This left ",
    results$plot_data_description$n_plots_missing_shrub_cover_after,
    " plots with no shrub cover information, ", sum(long_plots0$tot_shrub_SR[na_remaining_shrub_cover_rows_plot]>0), 
    " of which recorded non-zero shrub species richness."
  )
)

curation_notes_overall <- c(
  curation_notes_overall,
  "##### Shrub cover information",
  paste0(
    "For ", results$overall_data_description$n_surveys_missing_shrub_cover_any,
    " surveys shrub cover information was missing. ",
    "Implied absences were assumed in the following cases:\n\n",
    "- For ", length(na_shrub_cover_rows_0sr_overall),
    " surveys where total shrub species richness was recorded as 0.\n",
    "- The further ", length(na_shrub_cover_rows_ta_sr_overall),
    " surveys containing tree age class or tree species richness information.\n\n",
    "This left ",
    results$overall_data_description$n_surveys_missing_shrub_cover_after,
    " surveys with no shrub cover information, ", sum(overall0$tot_shrub_SR[na_remaining_shrub_cover_rows_overall]>0), 
    " of which recorded non-zero shrub species richness."
  )
)


##### 4.4 Regeneration species richness  ----

results$plot_data_description$n.missing.regen.species_before <- sum(
  rowSums(is.na(long_plots0[regen.sr.indicators])) > 0
)

results$overall_data_description$n.missing.regen.species_before <- sum(
  rowSums(is.na(overall0[regen.sr.indicators])) > 0
)

###### 4.4.1 - where all regen tree/shrub species richness info absent, and corresponding  tree/shrub regen level absent ----
#plots
## trees
na_treeregen_sr_rows_no_treeregen_level_plot <- which(
  # na in all tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() == length(tree.regen.sr.indicators) &
    # na in all tree regen level cols
    !has_indicator(long_plots0, tree.regen.level.indicators)
)
long_plots0[na_treeregen_sr_rows_no_treeregen_level_plot, tree.regen.sr.indicators] <- 0

## shrubs
na_shrubregen_sr_rows_no_shrubregen_level_plots <- which(
  # na in all shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.sr.indicators) &
    # na in all shrub regen level cols
    !has_indicator(long_plots0, shrub.regen.level.indicators) 
)
long_plots0[na_shrubregen_sr_rows_no_shrubregen_level_plots, shrub.regen.sr.indicators] <- 0

#overall
## trees
overall_na_treeregen_sr_rows_no_treeregen_level_overall <- which(
  # na in all tree regen species richness cols
  overall0[,tree.regen.sr.indicators] %>% is.na() %>%
    rowSums() == length(tree.regen.sr.indicators) &
    # na in all tree regen level cols
    !has_indicator(overall0, tree.regen.level.indicators)
)
overall0[overall_na_treeregen_sr_rows_no_treeregen_level_overall, tree.regen.sr.indicators] <- 0

## shrubs
overall_na_shrubregen_sr_rows_no_shrubregen_level_overall <- which(
  # na in all shrub regen species richness cols
  overall0[,shrub.regen.sr.indicators] %>% is.na() %>%
    rowSums() == length(shrub.regen.sr.indicators) &
    # na in all shrub regen level cols
    !has_indicator(overall0, shrub.regen.level.indicators)
)
overall0[overall_na_shrubregen_sr_rows_no_shrubregen_level_overall, shrub.regen.sr.indicators] <- 0
  
  
###### 4.4.2 - where only one of native/Non.Native filled for regen species richness ---- ----

# plots
## trees
na_treeregen_sr_rows_some_plots = which(
  # na in some tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() < length(tree.regen.sr.indicators))
# replace those nas with 0
long_plots0[na_treeregen_sr_rows_some_plots, tree.regen.sr.indicators] <-
  long_plots0[na_treeregen_sr_rows_some_plots, tree.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

## shrubs
na_shrubregen_sr_rows_some_plots = which(
  # na in some shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() < length(shrub.regen.sr.indicators))
# replace those nas with 0
long_plots0[na_shrubregen_sr_rows_some_plots, shrub.regen.sr.indicators] <-
  long_plots0[na_shrubregen_sr_rows_some_plots, shrub.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
         
# overall
## trees
overall_na_treeregen_sr_rows_some_overall = which(
  # na in some tree regen species richness cols
  overall0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() < length(tree.regen.sr.indicators))
# replace those nas with 0
overall0[overall_na_treeregen_sr_rows_some_overall, tree.regen.sr.indicators] <-
  overall0[overall_na_treeregen_sr_rows_some_overall, tree.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

## shrubs
overall_na_shrubregen_sr_rows_some_overall = which(
  # na in some shrub regen species richness cols
  overall0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() < length(shrub.regen.sr.indicators))
# replace those nas with 0
overall0[overall_na_shrubregen_sr_rows_some_overall, shrub.regen.sr.indicators] <-
  overall0[overall_na_shrubregen_sr_rows_some_overall, shrub.regen.sr.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

###### 4.4.XX - curation notes etc ----

na_remainging_tree_regen_sr_rows_plots = which(
  # na in all tree regen species richness cols
  long_plots0[,tree.regen.sr.indicators] %>% is.na() %>% rowSums() == length(tree.regen.sr.indicators)
)
na_remainging_shrub_regen_sr_rows_plots = which(
  # na in all shrub regen species richness cols
  long_plots0[,shrub.regen.sr.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.sr.indicators)
)

na_remaining_regen_sr_rows_plots = which(
  # na in all regen species richness cols
  long_plots0[,regen.sr.indicators] %>% is.na() %>% rowSums() == length(regen.sr.indicators)
)

curation_notes_plots <- c(
  curation_notes_plots,
  "##### Regeneration species richness",
  paste0(
    "For ", results$plot_data_description$n.missing.regen.species_before,
    " plots at least one regeneration species richness datum was missing. ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing regenerating tree species richness at ",
    length(na_treeregen_sr_rows_no_treeregen_level_plot),
    " plots where tree regeneration level was entirely missing.\n",
    "- The further missing regenerating tree species richness at ",
    length(na_treeregen_sr_rows_some_plots),
    " plots that recorded only one of native or non-native tree regeneration species richness.\n",
    "- Missing regenerating shrub species richness at ",
    length(na_shrubregen_sr_rows_no_shrubregen_level_plots),
    " plots where shrub regeneration level was entirely missing.\n",
    "- The further missing regenerating shrub species richness at ",
    length(na_shrubregen_sr_rows_some_plots),
    " plots that recorded only one of native or non-native shrub regeneration species richness.\n\n",
    "This left ",
    length(na_remainging_tree_regen_sr_rows_plots),
    " plots with no tree regeneration species richness information, ",
    length(na_remainging_shrub_regen_sr_rows_plots),
    " plots with no shrub regeneration species richness information and ",
    length(na_remaining_regen_sr_rows_plots),
    " plots with neither."
  )
)

# overall
na_remainging_tree_regen_sr_rows_overall = which(
  # na in all tree regen species richness cols
  overall0[,tree.regen.sr.indicators] %>% is.na() %>%
    rowSums() == length(tree.regen.sr.indicators)
)
na_remainging_shrub_regen_sr_rows_overall = which(
  # na in all shrub regen species richness cols
  overall0[,shrub.regen.sr.indicators] %>% is.na() %>%
    rowSums() == length(shrub.regen.sr.indicators)
)
na_remaining_regen_sr_rows_overall = which(
  # na in all regen species richness cols
  overall0[,regen.sr.indicators] %>% is.na() %>%
    rowSums() == length(regen.sr.indicators)
)
curation_notes_overall <- c(
  curation_notes_overall,
  "##### Regeneration species richness",
  paste0(
    "For ", results$overall_data_description$n.missing.regen.species_before,
    " surveys at least one regeneration species richness datum was missing. ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing regenerating tree species richness at ",
    length(overall_na_treeregen_sr_rows_no_treeregen_level_overall),
    " surveys where tree regeneration level was entirely missing.\n",
    "- The further missing regenerating tree species richness at ",
    length(overall_na_treeregen_sr_rows_some_overall),
    " surveys that recorded only one of native or non-native tree regeneration species richness.\n",
    "- Missing regenerating shrub species richness at ",
    length(overall_na_shrubregen_sr_rows_no_shrubregen_level_overall),
    " surveys where shrub regeneration level was entirely missing.\n",
    "- The further missing regenerating shrub species richness at ",
    length(overall_na_shrubregen_sr_rows_some_overall),
    " surveys that recorded only one of native or non-native shrub regeneration species richness.\n\n",
    "This left ",
    length(na_remainging_tree_regen_sr_rows_overall),
    " surveys with no tree regeneration species richness information, ",
    length(na_remainging_shrub_regen_sr_rows_overall),
    " surveys with no shrub regeneration species richness information and ",
    length(na_remaining_regen_sr_rows_overall),
    " surveys with neither."
  )
)


# sum the species

# plots
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

# overall
overall0 <- overall0 %>%
  mutate(
    tot_tree_regn_SR = row_sum_na_if_all_na(
      RTS.Native.richness,
      RTS.Non.Native.richness
    ),
    tot_shrub_regen_SR = row_sum_na_if_all_na(
      RSS.Native.richness,
      RSS.Non.Native.richness
    ))

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


results$overall_data_description$n.missing.regen.level_before <- sum(
  rowSums(is.na(overall0[regen.level.indicators])) > 0
)
results$overall_data_description$n.missing.treeregen.level_before <- sum(
  rowSums(is.na(overall0[tree.regen.level.indicators])) > 0
)
results$overall_data_description$n.missing.shrubregen.level_before <- sum(
  rowSums(is.na(overall0[shrub.regen.level.indicators])) > 0
)


###### 4.5.1 - where some regen level measurements present ----

# plots
na_regen_level_rows_some_plots = which(
  # na in some regen level cols
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators)
)

na_treeregen_level_rows_some_plots = which(
  # na in some regen level cols
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() < length(tree.regen.level.indicators)
)

na_shrubregen_level_rows_some_plots = which(
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

# overall
na_regen_level_rows_some_overall = which(
  # na in some regen level cols
  overall0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators)
)
na_treeregen_level_rows_some_overall = which(
  # na in some regen level cols
  overall0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() < length(tree.regen.level.indicators)
)
na_shrubregen_level_rows_some_overall = which(
  # na in some regen level cols
  overall0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    overall0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() < length(shrub.regen.level.indicators)
)

overall0 <-
  overall0 %>%
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


###### 4.5.2 - where regen level info absent, but tree/shrub regen sr is 0  -----
# note where both were originally missing, sr will now be 0 

# plots
## either
na_regen_level_rows_0regensr_either_plots = which(
  # na in all regen level cols & either tree or shrub regen species richness is 0
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators) &
    (long_plots0$tot_tree_regn_SR == 0 | long_plots0$tot_shrub_regen_SR == 0)
)
## tree
na_treeregen_level_rows_0treeregen_sr_plots = which(
  # na in all tree regen level cols & tree regen species richness is 0
  (long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() == 
    length(tree.regen.level.indicators)) &
    long_plots0$tot_tree_regn_SR == 0 
)
long_plots0[na_treeregen_level_rows_0treeregen_sr_plots, tree.regen.level.indicators] <- "Absent"

## shrub
na_shrubregen_level_rows_0shrubregen_sr_plots = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.level.indicators) &
    long_plots0$tot_shrub_regen_SR == 0
)
long_plots0[na_shrubregen_level_rows_0shrubregen_sr_plots, shrub.regen.level.indicators] <- "Absent"
  
# overall
## either
na_regen_level_rows_0regensr_either_overall = which(
  # na in all regen level cols & either tree or shrub regen species richness is 0
  overall0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators) &
    (overall0$tot_tree_regn_SR == 0 | overall0$tot_shrub_regen_SR == 0)
)
## tree
na_treeregen_level_rows_0treeregen_sr_overall = which(
  # na in all tree regen level cols & tree regen species richness is 0
  (overall0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() == length(tree.regen.level.indicators)) &
    overall0$tot_tree_regn_SR == 0 
)
overall0[na_treeregen_level_rows_0treeregen_sr_overall, tree.regen.level.indicators] <- "Absent"
## shrub
na_shrubregen_level_rows_0shrubregen_sr_overall = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  overall0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.level.indicators) &
    overall0$tot_shrub_regen_SR == 0
)
overall0[na_shrubregen_level_rows_0shrubregen_sr_overall, shrub.regen.level.indicators] <- "Absent"


###### 4.5.XX  - curation notes etc ----

#plots
na_treeregen_level_after_plots = which(
  # na in any tree regen level cols & tree regen species richness is 0
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_treeregen_level_after_sr_plots <- long_plots0$tot_tree_regn_SR[na_treeregen_level_after_plots]

na_shrubregen_level_after_plots = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_shrubregen_level_after_sr_plots <- long_plots0$tot_shrub_regen_SR[na_treeregen_level_after_sr_plots]

na_anyregen_level_after_plots = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0
)

na_allregen_level_after_plots = which(
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
    length(na_treeregen_level_rows_some_plots),
    " plots that recorded some (but not all) tree regeneration levels.\n",
    "- The further missing tree regeneration level at ",
    length(na_treeregen_level_rows_0treeregen_sr_plots),
    " plots that recorded zero total species richness of regenerating trees.\n",
    "- Missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_some_plots),
    " plots that recorded some (but not all) shrub regeneration levels.\n",
    "- The further missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_0shrubregen_sr_plots),
    " plots that recorded zero total species richness of regenerating shrubs.\n\n",
    "This left ",
    length(na_anyregen_level_after_plots),
    " plots with some missing regeneration level information (",
    length(na_treeregen_level_after_plots),
    " missing tree regeneration level and ",
    length(na_treeregen_level_after_sr_plots),
    " missing shrub regeneration level).\n",
    "Note: Of the plots with missing tree regeneration level, ",
    length(na_treeregen_level_after_sr_plots[na_treeregen_level_after_sr_plots > 0]),
    " recorded tree regeneration species richness >1, and of the plots with missing shrub regeneration level, ",
    sum(na_shrubregen_level_after_sr_plots > 0),
    " recorded shrub regeneration species richness >1."
  )
)

# overall

na_treeregen_level_after_overall = which(
  # na in any tree regen level cols & tree regen species richness is 0
  overall0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_treeregen_level_after_sr_overall <- overall0$tot_tree_regn_SR[na_treeregen_level_after_overall]
na_shrubregen_level_after_overall = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  overall0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_shrubregen_level_after_sr_overall <- overall0$tot_shrub_regen_SR[na_shrubregen_level_after_overall]
na_anyregen_level_after_overall = which(
  # na in any shrub regen level cols & shrub regen species richness is 0
  overall0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_allregen_level_after_overall = which(
  # na in all regen level cols
  overall0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators)
)
curation_notes_overall <- c(
  curation_notes_overall,
  "##### Regeneration level information",
  paste0(
    "For ", results$overall_data_description$n.missing.regen.level_before,
    " surveys at least one regeneration level datum was missing (",
    results$overall_data_description$n.missing.treeregen.level_before,
    " missing tree regeneration level and ",
    results$overall_data_description$n.missing.shrubregen.level_before,
    " missing shrub regeneration level). ",
    "Implied zeros were assumed in the following cases:\n\n",
    "- Missing tree regeneration level at ",
    length(na_treeregen_level_rows_some_overall),
    " surveys that recorded some (but not all) tree regeneration levels.\n",
    "- The further missing tree regeneration level at ",
    length(na_treeregen_level_rows_0treeregen_sr_overall),
    " surveys that recorded zero total species richness of regenerating trees.\n",
    "- Missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_some_overall),
    " surveys that recorded some (but not all) shrub regeneration levels.\n",
    "- The further missing shrub regeneration level at ",
    length(na_shrubregen_level_rows_0shrubregen_sr_overall),
    " surveys that recorded zero total species richness of regenerating shrubs.\n\n",
    "This left ",
    length(na_anyregen_level_after_overall),
    " surveys with some missing regeneration level information (",
    length(na_treeregen_level_after_overall),
    " missing tree regeneration level and ",
    length(na_shrubregen_level_after_overall),
    " missing shrub regeneration level).\n",
    "Note: Of the surveys with missing tree regeneration level, ",
    length(na_treeregen_level_after_sr_overall[na_treeregen_level_after_sr_overall > 0]),
    " recorded tree regeneration species richness >1, and of the surveys with missing shrub regeneration level, ",
    length(na_shrubregen_level_after_sr_overall[na_shrubregen_level_after_sr_overall > 0]),
    " recorded shrub regeneration species richness >1."
  )
)



##### 4.6 Flora and Deadwood indicators ----
##### 4.6.1 - where only some flora/deadwood indicators missing ---- 

# plots
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

# overall
results$overall_data_description$n.missing.flora_before <- sum(
  rowSums(is.na(overall0[flora.indicators])) > 0
)
results$overall_data_description$n.missing.deadwood_before <- sum(
  rowSums(is.na(overall0[deadwood.indicators])) > 0
)
results$overall_data_description$n.missing.flora_deadwood_before <- sum(
  rowSums(is.na(overall0[flora.deadwood.indicators])) > 0
)

overall0 <- overall0 %>%
  fill_assumed_indicators(
    .,
    flora.deadwood.indicators,
    indicator_types
  )

##### 4.6.XX -  ----

results$plot_data_description$n.missing.flora_after <- sum(
  rowSums(is.na(long_plots0[flora.indicators])) > 0
)
results$plot_data_description$n.missing.deadwood_after <- sum(
  rowSums(is.na(long_plots0[deadwood.indicators])) > 0
)

results$plot_data_description$n.missing.flora_deadwood_after <- sum(
  rowSums(is.na(long_plots0[flora.deadwood.indicators])) > 0
)

results$overall_data_description$n.missing.flora_after <- sum(
  rowSums(is.na(overall0[flora.indicators])) > 0
)
results$overall_data_description$n.missing.deadwood_after <- sum(
  rowSums(is.na(overall0[deadwood.indicators])) > 0
)
results$overall_data_description$n.missing.flora_deadwood_after <- sum(
  rowSums(is.na(overall0[flora.deadwood.indicators])) > 0
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

curation_notes_overall <- c(
  curation_notes_overall,
  "##### Features information (flora and deadwood)",
  paste0(
    "For ", results$overall_data_description$n.missing.flora_deadwood_before,
    " surveys at least one datum was missing from flora (",
    results$overall_data_description$n.missing.flora_before,
    " surveys) or deadwood (",
    results$overall_data_description$n.missing.deadwood_before,
    " surveys). ",
    "Implied absences were assumed in the following cases:\n\n",
    "- ", results$overall_data_description$n.missing.flora_deadwood_before - results$overall_data_description$n.missing.flora_after,
    " surveys with data entered for at least one of these 'features' indicators (",
    results$overall_data_description$n.missing.flora_before - results$overall_data_description$n.missing.flora_after,
    " surveys filled for flora and ",
    results$overall_data_description$n.missing.deadwood_before - results$overall_data_description$n.missing.flora_after,
    " for deadwood).\n\n",
    "This left ",
    results$overall_data_description$n.missing.flora_after,
    " surveys with no flora or deadwood information."
  )
)


##### 4.7 Invasives, Animal damage, Human impact or Tree health: contains "Invasives.", "Animal.Damage.", "Human.Impacts.", "Tree.Health." ----

##### 4.7.1 - where only some threats indicators missing ---- 

# plots
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

# overall
results$overall_data_description$n.missing.invasives_before <- sum(
  rowSums(is.na(overall0[invasives.indicators])) > 0
)
results$overall_data_description$n.missing.animal_damage_before <- sum(
  rowSums(is.na(overall0[animal_damage.indicators])) > 0
)
results$overall_data_description$n.missing.human_impact_before <- sum(
  rowSums(is.na(overall0[human_impact.indicators])) > 0
)
results$overall_data_description$n.missing.tree_health_before <- sum(
  rowSums(is.na(overall0[tree_health.indicators])) > 0
)
results$overall_data_description$n.missing.threats_before <- sum(
  rowSums(is.na(overall0[threats.indicators])) > 0
)

overall0 <- overall0 %>%
  fill_assumed_indicators(
    .,
    threats.indicators,
    indicator_types
  )

##### 4.7.XX -  ----

# plots
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

# overall
results$overall_data_description$n.missing.invasives_after <- sum(
  rowSums(is.na(overall0[invasives.indicators])) > 0
)
results$overall_data_description$n.missing.animal_damage_after <- sum(
  rowSums(is.na(overall0[animal_damage.indicators])) > 0
)
results$overall_data_description$n.missing.human_impact_after <- sum(
  rowSums(is.na(overall0[human_impact.indicators])) > 0
)
results$overall_data_description$n.missing.tree_health_after <- sum(
  rowSums(is.na(overall0[tree_health.indicators])) > 0
)
results$overall_data_description$n.missing.threats_after <- sum(
  rowSums(is.na(overall0[threats.indicators])) > 0
)

curation_notes_overall <- c(
  curation_notes_overall,
  "##### Threats information (invasives, animal damage, human impact and tree health)",
  paste0(
    "For ", results$overall_data_description$n.missing.threats_before,
    " surveys at least one datum was missing across threats, including: invasives (",
    results$overall_data_description$n.missing.invasives_before,
    " surveys), animal damage (",
    results$overall_data_description$n.missing.animal_damage_before,
    " surveys), human impact (",
    results$overall_data_description$n.missing.human_impact_before,
    " surveys) and tree health (",
    results$overall_data_description$n.missing.tree_health_before,
    " surveys). ",
    "Implied absences were assumed in the following cases:\n\n",
    "- ", results$overall_data_description$n.missing.threats_before - results$overall_data_description$n.missing.human_impact_after,
    " surveys with data entered for at least one of these 'threats' indicators (",
    results$overall_data_description$n.missing.invasives_before - results$overall_data_description$n.missing.invasives_after,
    " for invasives; ",
    results$overall_data_description$n.missing.animal_damage_before - results$overall_data_description$n.missing.animal_damage_after,
    " for animal damage; ",
    results$overall_data_description$n.missing.human_impact_before - results$overall_data_description$n.missing.human_impact_after,
    " for human impact; ",
    results$overall_data_description$n.missing.tree_health_before - results$overall_data_description$n.missing.tree_health_after,
    " for tree health).\n\n",
    "This left ",
    results$overall_data_description$n.missing.invasives_after,
    " surveys with no information on threats."
  )
)

##### 4.8 remaining empty feature or threat indicators ----

# plots
results$plot_data_description$n.missing.features_threats_before <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    c(flora.deadwood.indicators, threats.indicators),
    indicator_types
  )

# overall
results$overall_data_description$n.missing.features_threats_before <- sum(
  rowSums(is.na(overall0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)

overall0 <- overall0 %>%
  fill_assumed_indicators(
    .,
    c(flora.deadwood.indicators, threats.indicators),
    indicator_types
  )

##### 4.7.XX -  ----

results$plot_data_description$n.missing.features_threats_after <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)
results$overall_data_description$n.missing.features_threats_after <- sum(
  rowSums(is.na(overall0[c(flora.deadwood.indicators, threats.indicators)])) > 0
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

curation_notes_overall <- c(
  curation_notes_overall,
  "##### Remaining features and threats information",
  paste0(
    "Of the ", results$overall_data_description$n.missing.features_threats_before,
    " surveys remaining that contained missing data across features (N = ",
    results$overall_data_description$n.missing.deadwood_after,
    ") or threats (N = ",
    results$overall_data_description$n.missing.invasives_after,
    "), ",
    results$overall_data_description$n.missing.features_threats_before - results$overall_data_description$n.missing.features_threats_after,
    " surveys contained information on at least one of those indicators. ",
    "These were assumed to be implied absences. \n",
    "This left ",
    results$overall_data_description$n.missing.features_threats_after,
    " surveys with no information on features or threats."
  )
)

##### 
# 5. fix "Dominated.By.One.Or.Two.SPP" info ----
## "Dominated.By.One.Or.Two.SPP" is never NA - where blank it is "no", even where no corresponeding species are recorded (eitehr 0 or NA)
##### set to NA where corresponding species richness is 0 or NA

# define rules
dominated_rules <- tibble::tibble(
  indicator = c(
    "RTS.Dominated.By.One.Or.Two.SPP",
    "RSS.Dominated.By.One.Or.Two.SPP",
    "TS.Canopy.Dominated.By.One.Or.Two.SPP",
    "SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP"
  ),
  total_sr = c(
    "tot_tree_regn_SR",
    "tot_shrub_regen_SR",
    "tot_tree_SR",
    "tot_shrub_SR"
  ),
  result_name = c(
    "n.fixed_dominated_rts",
    "n.fixed_dominated_rss",
    "n.fixed_dominated_ts",
    "n.fixed_dominated_ss"
  )
)

#apply rule in looo
for (i in seq_len(nrow(dominated_rules))) {
  
  ind  <- dominated_rules$indicator[i]
  tot  <- dominated_rules$total_sr[i]
  res  <- dominated_rules$result_name[i]
  
  # rows where dominance is impossible
  bad_rows_plots <- long_plots0[[tot]] == 0 | is.na(long_plots0[[tot]])
  bad_rows_overall <- overall0[[tot]] == 0 | is.na(overall0[[tot]])
  
  # count how many non-FALSE values are being fixed
  results$plot_data_description[[res]] <- sum(
    long_plots0[[ind]][bad_rows_plots] == FALSE,
    na.rm = TRUE
  )
  results$overall_data_description[[res]] <- sum(
    overall0[[ind]][bad_rows_overall] == FALSE,
    na.rm = TRUE
  )
  
  # set dominance to NA where impossible
  long_plots0[[ind]][bad_rows_plots] <- NA
  overall0[[ind]][bad_rows_overall] <- NA
}

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

curation_notes_overall <- c(
  curation_notes_overall,
  "#### 'Dominated by one or two species' indicators - inappropraite default value",
  paste0(
    "Orignally 'Dominated by one or two species', indicators were FALSE by default and manually overwritten to TRUE when needed.\n ",
    "- This left FALSE values even when records of zeo or NA species richness implied the indicator should be NA.", 
    " In those cases (",
    results$overall_data_description$n.fixed_dominated_rts, " for renerating trees, ", 
    results$overall_data_description$n.fixed_dominated_rss, " for regenerating shrubs, ", 
    results$overall_data_description$n.fixed_dominated_ts, " for tree species, ",
    results$overall_data_description$n.fixed_dominated_ss, " for shrub species",
    "), the indicator was changed to 'NA'"
  )
)


# 5.2 - # numebr of plots
## number of unique plot numbers in each survey id in longplots
plot_numbers_by_survey <- long_plots0 %>%
  group_by(id) %>%
  summarise(
    n_plots = n_distinct(Plot.Number)
  )
overall0 <- overall0 %>%
  left_join(plot_numbers_by_survey, by = "id")



# 6. Understand what missing data remains ----

# sum missing indicators (except for dominated indicators)
long_plots0$na_count <- rowSums(is.na(long_plots0[indicator_types$indicator[!indicator_types$indicator %in% c(
  "RTS.Dominated.By.One.Or.Two.SPP",
  "RSS.Dominated.By.One.Or.Two.SPP",
  "TS.Canopy.Dominated.By.One.Or.Two.SPP",
  "SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP"
)]]))
overall0$na_count <- rowSums(is.na(overall0[indicator_types$indicator[!indicator_types$indicator %in% c(
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
missing_tbl_plots <- long_plots0[, indicator_types$indicator] %>%
  is.na() %>%
  colSums() %>% 
  enframe(
    .,
    name  = "indicator",
    value = "n_missing"
  )
missing_by_indicator_plots <- indicator_types %>%
  select(indicator, theme, type) %>%
  left_join(missing_tbl_plots, by = "indicator")

missing_tbl_overall <- overall0[, indicator_types$indicator] %>%
  is.na() %>%
  colSums() %>% 
  enframe(
    .,
    name  = "indicator",
    value = "n_missing"
  )
missing_by_indicator_overall <- indicator_types %>%
  select(indicator, theme, type) %>%
  left_join(missing_tbl_overall, by = "indicator")



curated_data_description_plots <- c(
  "## Curated plot data description",
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
    "- Regeneration tree species richness: ", length(na_remainging_tree_regen_sr_rows_plots), " plots missing data (",
    round(100 * length(na_remainging_tree_regen_sr_rows_plots) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration shrub species richness: ", length(na_remainging_shrub_regen_sr_rows_plots), " plots missing data (",
    round(100 * length(na_remainging_shrub_regen_sr_rows_plots) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration level (trees): ", length(na_treeregen_level_after_plots), " plots missing data (",
    round(100 * length(na_treeregen_level_after_plots) / nrow(long_plots0), 2), "%)\n",
    "- Regeneration level (shrubs): ", length(na_shrubregen_level_after_plots), " plots missing data (",
    round(100 * length(na_shrubregen_level_after_plots) / nrow(long_plots0), 2), "%)\n",
    "- Features and threats indicators: ", results$plot_data_description$n.missing.features_threats_after, " plots missing data (",
    round(100 * results$plot_data_description$n.missing.features_threats_after / nrow(long_plots0), 2), "%)"
  ),
  "\n\n"
)

curated_data_description_overall <- c(
  "## Curated overall data description",
  paste0(
    "The curated dataset contained information from:\n\n",
    "- ", nrow(overall0), " surveys\n",
    "- ", length(unique(overall0$id)), " individual surveys\n",
    "- ", length(unique(paste(overall0$Site.Name, overall0$Stratum ))), " individual strata\n",
    "- ", length(unique(overall0$Site.Name)), " individual sites\n",
    "- Covering ", 
    format(min(as.Date(overall0$ActualObservationDate, format = "%d/%m/%Y"), na.rm = TRUE), "%Y"),
    " to ",
    format(max(as.Date(overall0$ActualObservationDate, format = "%d/%m/%Y"), na.rm = TRUE), "%Y"),
    ".\n\n",
    "Of these, ", sum(overall0$na_count > 0), " surveys (",
    round(100 * sum(overall0$na_count > 0) / nrow(overall0), 2), "%) still contained missing data for at least one indicator:\n\n",
    "- Tree species richness: ", results$overall_data_description$n_surveys_missing_tree_richness_any_after, " surveys missing data (",
    round(100 * results$overall_data_description$n_surveys_missing_tree_richness_any_after / nrow(overall0), 2), "%)\n",
    "- Shrub species richness: ", results$overall_data_description$n_surveys_missing_shrub_richness_any_after, " surveys missing data (",
    round(100 * results$overall_data_description$n_surveys_missing_shrub_richness_any_after / nrow(overall0), 2), "%)\n",
    "- Tree age category covers: ", results$overall_data_description$n_surveys_missing_ta_after, " surveys missing data (",
    round(100 * results$overall_data_description$n_surveys_missing_ta_after / nrow(overall0), 2), "%)\n",
    "- Shrub cover: ", results$overall_data_description$n_surveys_missing_shrub_cover_after, " surveys missing data (",
    round(100 * results$overall_data_description$n_surveys_missing_shrub_cover_after / nrow(overall0), 2), "%)\n",
    "- Regeneration tree species richness: ", length(na_remainging_tree_regen_sr_rows_overall), " surveys missing data (",
    round(100 * length(na_remainging_tree_regen_sr_rows_overall) / nrow(overall0), 2), "%)\n",
    "- Regeneration shrub species richness: ", length(na_remainging_shrub_regen_sr_rows_overall), " surveys missing data (",
    round(100 * length(na_remainging_shrub_regen_sr_rows_overall) / nrow(overall0), 2), "%)\n",
    "- Regeneration level (trees): ", length(na_treeregen_level_after_overall), " surveys missing data (",
    round(100 * length(na_treeregen_level_after_overall) / nrow(overall0), 2), "%)\n",
    "- Regeneration level (shrubs): ", length(na_shrubregen_level_after_overall), " surveys missing data (",
    round(100 * length(na_shrubregen_level_after_overall) / nrow(overall0), 2), "%)\n",
    "- Features and threats indicators: ", results$overall_data_description$n.missing.features_threats_after, " surveys missing data (",
    round(100 * results$overall_data_description$n.missing.features_threats_after / nrow(overall0), 2), "%)"),
  "\n\n"
)



# 7. transcribe to WECA-like indicators -----
# ...based on some intuitive thinking with MH, assuming 5 sampling plots


transcribe_to_weca_notes <- c(
  "## Transcribing to WECA-like indicators",
  "Here follows an initial draft of a method to transcribe the data collected during historic woodland condition assessment to something comparible to the methods used to measure and value the new WECA indicators.:\n\n"
)

weca_faux_indicators <- data.frame(
  indicator = NA,
  plot_comparibility = NA,
  overall_comparibility = NA,
  useful_weca_alternative = NA,
  notes = NA
)

# converting overall DAFOR to expected proportion of plots with X present
overall_weighting_lookup <- c(
  "Absent" = 0,
  "R" = 0.2,
  "O" = 0.4,
  "F" = 0.7,
  "A" = 1,
  "D" = 1
)

transcribe_to_weca_notes <- c(
  transcribe_to_weca_notes,
  "### Overall approach",
  "The ideal aimed for was to, derive measurements that were as close as possible to those from WECA and apply the same value functions. ",
  "Where this was not possible, first an attempt was made to value what information was availible, based on the expert opinion of EMcH and MH, referencing the indicator's value functions. \n\n",
  "The WECA value functions were estimated for the plot level, so typically that is where they are most appropriate to use. However, the WCA data had many surveys with only 'overall woodland' data, and so a certain dergree of loose interpretation was required here to build comparible values. ",
  "Generally, where a feature was to be determined present or absent, we assumed that DAFOR estimates related to plot presentace presence as follows: Absent = 0, R = 0.2, O = 0.4, F = 0.7 and A/D = 1. Here, it is important to emphasise the caveat stated above RE the scale at which measures were valued.\n\n",
  "We scored our confidence in the comparibility of each indicator at the plot and overall level on a scale of 1-5."
  )


## 7.1 age class indicators ----
# number of age classes present, combinign max(TA.Veteran.Pollards.Ancient.Coppice.Stools and TA.100.200.Years)

# does R mean absent???
# 0-4 age classes, max of top two in upper  

# plot
## mature is row max of TA.Veteran.Pollards.Ancient.Coppice.Stools and TA.100.200.Years

long_plots0$TA_mature <- pmax(long_plots0$TA.Veteran.Pollards.Ancient.Coppice.Stools,
  long_plots0$TA.100.200.Years, na.rm = TRUE
)
long_plots0$TA_count <- rowSums(
  long_plots0[, c( "TA.Less.Than.20.Years", "TA.20.50.Years", "TA.50.100.Years", "TA_mature")] > "Absent")

# aa = long_plots0[,c("TA.Less.Than.20.Years", "TA.20.50.Years", "TA.50.100.Years", "TA.100.200.Years","TA.Veteran.Pollards.Ancient.Coppice.Stools", "TA_mature", "TA_count")]

# overall
# at overall F = 0.8 O = 0.4 R = 0.2
overall0$TA_mature <- pmax(overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools,
  overall0$TA.100.200.Years, na.rm = TRUE
)
#cbind(overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools, overall0$TA.100.200.Years, overall0$TA_mature)
overall0$TA_count <- rowSums(
  overall0[, c( "TA.Less.Than.20.Years", "TA.20.50.Years", "TA.50.100.Years", "TA_mature")] %>% 
    mutate_all(~ overall_weighting_lookup[.]))

## valuing
age_structure_predictions <- rbind(
  data.frame(measure = 0, value = 0),
  age_structure_predictions)

long_plots0$WECA_tree_age_score <- approx(
  x = age_structure_predictions$measure,
  y = age_structure_predictions$value,
  xout = long_plots0$TA_count
)$y

plot_av_bysurvey_tree_age_score <- long_plots0 %>%
  group_by(id) %>%
  summarise(
    plot_av_tree_age_score = mean(WECA_tree_age_score, na.rm = TRUE)
  )

overall0$WECA_tree_age_score_overall <- approx(
  x = age_structure_predictions$measure,
  y = age_structure_predictions$value,
  xout = overall0$TA_count
)$y

overall0$WECA_tree_age_score_plot_av <- overall0$TA_count %>% 
  left_join(plot_av_bysurvey_tree_age_score, by = "id") %>%
  pull(plot_av_tree_age_score)
                                                                  
                                                                  
                                                                  
transcribe_to_weca_notes <- c(
  transcribe_to_weca_notes,
  "### Age class indicators",
  "WECA age class indicators are based on the number of age classes present, with a maximum of 4 (Youne, Juvinile, Adult and Mature). ",
  "The old WCA data included DAFOR cover of trees < 20 years, 20-50 years, 50-100 years, 100-200 years and Veteran/Pollards/Ancient/Coppice Stools. ",
  "Here, we combined the TA.Veteran.Pollards.Ancient.Coppice.Stools and TA.100.200.Years indicators to create a 'mature' category (max of DAFOR).", 
  "For plots we counted the number of age classes present.",
  "For overall surveys, we assumed that R = 0.2, O = 0.4, F = 0.7 and A/D = 1 for each age class indicator (recreating an analogoud survey of 5 plots where that age class was detected at 1, 2, 3-4 and 5 plots, respectivly), and summed these across the age class indicators to give a score between 0 and 4.")

weca_faux_indicators <- weca_faux_indicators %>%
  add_row(
    indicator = "Age class diversity",
    plot_comparibility = 4.5,
    overall_comparibility = 3,
    useful_weca_alternative = NA,
    notes = "Not exactly same age classes. Ovearll scoring a little hacky."
  )


# how happy score
## plot 4.5
## overall 3


## 7.2 canopy nativness ----
## TS.Native.richness, TS.Non.Native.richness, TS.Canopy.Dominated.By.One.Or.Two.SPP, SS.Native.richness, SS.Non.Native.richness, SS.Shrub.Layer.Dominated.By.One.Or.Two.SPP

# where only native/ non native can be sure
# where shrub is one and tree species another???
# cant graduate

long_plots0$canopy_all_native <- NA
long_plots0$canopy_all_native <- ifelse(
  (long_plots0$TS.Non.Native.richness == 0 & long_plots0$TS.Native.richness > 0 &
    long_plots0$SS.Native.richness > 0 & long_plots0$SS.Non.Native.richness == 0),
  TRUE, NA)
long_plots0$canopy_all_nonnative <- NA
long_plots0$canopy_all_nonnative <- ifelse(
  (long_plots0$TS.Non.Native.richness > 0 & long_plots0$TS.Native.richness == 0 &
    long_plots0$SS.Native.richness == 0 & long_plots0$SS.Non.Native.richness > 0),
  TRUE, NA)
long_plots0$canopy_mixed <- NA
long_plots0$canopy_mixed <- ifelse(
  (long_plots0$TS.Non.Native.richness > 0 & long_plots0$TS.Native.richness > 0) |
    (long_plots0$SS.Native.richness > 0 & long_plots0$SS.Non.Native.richness > 0),
  TRUE, NA)

overall0$canopy_all_native <- NA
overall0$canopy_all_native <- ifelse(
  (overall0$TS.Non.Native.richness == 0 & overall0$TS.Native.richness > 0 &
    overall0$SS.Native.richness > 0 & overall0$SS.Non.Native.richness == 0),
  TRUE, NA)
overall0$canopy_all_nonnative <- NA
overall0$canopy_all_nonnative <- ifelse(
  (overall0$TS.Non.Native.richness > 0 & overall0$TS.Native.richness == 0 &
    overall0$SS.Native.richness == 0 & overall0$SS.Non.Native.richness > 0),
  TRUE, NA)
overall0$canopy_mixed <- NA
overall0$canopy_mixed <- ifelse(
  (overall0$TS.Non.Native.richness > 0 & overall0$TS.Native.richness > 0) |
    (overall0$SS.Native.richness > 0 & overall0$SS.Non.Native.richness > 0),
  TRUE, NA)

transcribe_to_weca_notes <- c(
  transcribe_to_weca_notes,
  "### Canopy nativness",
  "WECA canopy nativness is based on the volume of the tree and shrub canopy that is UK-native. ",
  "The old WCA data included native and non-native species richness for both the canopy and shrub species seperatly.",
  "Here, we created indicators of whether the canopy was all native, all non-native or mixed (where both native and non-native species were present).",
  "We can be confident about the extremities (all native or all non-natuive), but valuing mixed canopies was deemed unreliable.",
  "Note that larger plots/zones are more likley to be mixed, purely becasue of their larger sampling area. ",
  "No attempt was made here to value this indicator"
)

weca_faux_indicators <- weca_faux_indicators %>%
  add_row(
    indicator = "Canopy nativness",
    plot_comparibility = 1,
    overall_comparibility = 0.5,
    useful_weca_alternative = NA,
    notes = "Extremes (all native or all non-native) are comparable, but mixed canopy is not.")


## 7.3 verticle structure ----

# WECA
## count of 0-6
## only where >DOMIN 5 10% cover of plot

transcribe_to_weca_notes <- c(
  transcribe_to_weca_notes,
  "### Vertical structure",
  "The WECA vertical structure metric is based on the number of six potential vegetation layers (ground, field, shrub, canopy low, mid & high) with >10% vegetation volume in the plot.",
  " The old WCA data did not have directly comparible indicators for the different verticle strata, but did have a range of indicators that could be used to create some indicators with a degree of indirect comparibility to three of the strata:\n\n", 
  "- Ground and field layer (inseperatable): DAFOR of different ground flora classes, seedlings and saplings of trees and shrubs\n",
  "- Shrub layer: DAFOR of shrub cover\n",
  "- Canopy layers (not possible): tree age class DAFOR were deemed inappropriate for defining verticle stratification.\n",
  "Importantly, the first two needed to be combined (ground/field, shrub) and any comparison would only be possible with the lower-canopy levels from the new WECA data, and even then, with the caveat that the strata are indirect estimates. ",
  "No attempt was made here to value this indicator. Any comparison to the new WECA should be based on comparision to those lower three layers."
)
  
# considering only occational or greater (>11%) at plot level

# ground layer <0.1  or field layer
# Flora.Ancient.Woodland.Plants..Specialists., Flora.Other.Woodland.Plants..Generalists., Flora.Other.Native.Plants, Flora.Coarse.Vegetation, Flora.Other.Plants, Flora.No.Vegitation
# LTR.Seedlings.Less.Than.10cm, LTR.Saplings.Greater.Than.100cm, LTR.Coppice.Regrowth.or.Suckering
# LSR.Seedlings.Less.Than.10cm, LSR.Saplings.Greater.Than.100cm, 

long_plots0$ground_flora <- pmax(
  long_plots0$Flora.Ancient.Woodland.Plants..Specialists.,
  long_plots0$Flora.Other.Woodland.Plants..Generalists.,
  long_plots0$Flora.Other.Native.Plants,
  long_plots0$Flora.Coarse.Vegetation,
  long_plots0$Flora.Other.Plants,
  na.rm = TRUE
)
overall0$ground_flora <- pmax(
  overall0$Flora.Ancient.Woodland.Plants..Specialists.,
  overall0$Flora.Other.Woodland.Plants..Generalists.,
  overall0$Flora.Other.Native.Plants,
  overall0$Flora.Coarse.Vegetation,
  overall0$Flora.Other.Plants,
  na.rm = TRUE
)


long_plots0$seedlings <- pmax(
  long_plots0$LTR.Seedlings.Less.Than.10cm,
  long_plots0$LTR.Seedlings.10.100cm,
  long_plots0$LSR.Seedlings.Less.Than.10cm,
  long_plots0$LSR.Seedlings.10.100cm,
  na.rm = TRUE
)
overall0$seedlings <- pmax(
  overall0$LTR.Seedlings.Less.Than.10cm,
  overall0$LTR.Seedlings.10.100cm,
  overall0$LSR.Seedlings.Less.Than.10cm,
  overall0$LSR.Seedlings.10.100cm,
  na.rm = TRUE
)

long_plots0$saplings <- pmax(
  long_plots0$LTR.Saplings.Greater.Than.100cm,
  long_plots0$LSR.Saplings.Greater.Than.100cm,
  na.rm = TRUE
)
overall0$saplings <- pmax(
  overall0$LTR.Saplings.Greater.Than.100cm,
  overall0$LSR.Saplings.Greater.Than.100cm,
  na.rm = TRUE
)

# shrub layer
# Shrub.Cover
# TA.Less.Than.20.Years

long_plots0$shrub_layer <- pmax(
  long_plots0$Shrub.Cover,
  long_plots0$TA.Less.Than.20.Years,
  na.rm = TRUE
)
overall0$shrub_layer <- pmax(
  overall0$Shrub.Cover,
  overall0$TA.Less.Than.20.Years,
  na.rm = TRUE
)


# Alt measurement out of 3

long_plots0$verticle_structure_score <- rowSums(
  cbind(1*(long_plots0$ground_flora > "O"),
        0.5*(long_plots0$seedlings > "O"),
        0.5*(long_plots0$saplings > "O"),
        1*(long_plots0$Shrub.Cover > "O"),
        1*(long_plots0$shrub_layer > "O")),
  
  na.rm = TRUE
)
# overall weigthed by aupssumed plot-level presence (R = 0.2, O = 0.4, F = 0.7, A/D = 1)
overall0$verticle_structure_score <- rowSums(
  cbind(1*(overall_weighting_lookup[overall0$ground_flora] %>% as.numeric()),
        0.5*(overall_weighting_lookup[overall0$seedlings] %>% as.numeric()),
        0.5*(overall_weighting_lookup[overall0$saplings] %>% as.numeric()),
        1*(overall_weighting_lookup[overall0$Shrub.Cover] %>% as.numeric()),
        1*(overall_weighting_lookup[overall0$shrub_layer] %>% as.numeric())),
  na.rm = TRUE
)

transcribe_to_weca_notes <- c(
  transcribe_to_weca_notes,
  "A score to aproximpate the number of the three lower strata with ecologically functioning vegetation structure (DAFOR score >= O) was calculated by summing thusly (noteably allowing for half-presences):\n\n",
   "- shrub layer (1)\n",
   "- ground flora (1)\n",
   "- seedlings (0.5)\n",
   "- saplings (0.5)\n",
  "An analogous score at the overall level further weighted each indicator by the assumed plot-level presence from overall cover (R = 0.2, O = 0.4, F = 0.7, A/D = 1)."
  )

# poss alt WECA indicator for comparison combining these three layers (with one combined)




# 15 - 12, 12 - 20 and 20 +
# TA.Veteran.Pollards.Ancient.Coppice.Stools
# TA.100.200.Years
# TA.50.100.Years
# TA.20.50.Years
# "uncertain"?

# alternative comparision indicator for lower layer


# plot level 2
# overall 1

## 7.4 native species richness ------

# TS.Native.richness, SS.Native.richness

long_plots0$tot_native_SR_trees_shrubs <- rowSums(long_plots0[, c("TS.Native.richness", "SS.Native.richness")], na.rm = TRUE)
overall0$tot_native_SR_trees_shrubs <- rowSums(overall0[, c("TS.Native.richness", "SS.Native.richness")], na.rm = TRUE)

# trust site managers to know nativnes
# take regional 80th of all in regoin for denominator

plot_regional_80th_native_sr_trees_shrubs <- long_plots0 %>% 
  group_by(Region) %>%
  summarise(
    regional_80th_native_sr_trees_shrubs = quantile(tot_native_SR_trees_shrubs, 0.8, na.rm = TRUE)
  )
overall_regional_80th_native_sr_trees_shrubs <- overall0 %>%
  group_by(Region) %>%
  summarise(
    regional_80th_native_sr_trees_shrubs = quantile(tot_native_SR_trees_shrubs, 0.8, na.rm = TRUE)
    )

long_plots0$prop_native_sr_trees_shrubs <- long_plots0$tot_native_SR_trees_shrubs / 
  overall_regional_80th_native_sr_trees_shrubs$regional_80th_native_sr_trees_shrubs[match(long_plots0$Region, overall_regional_80th_native_sr_trees_shrubs$Region)]
long_plots0$prop_native_sr_trees_shrubs[long_plots0$prop_native_sr_trees_shrubs>1] <- 1
overall0$prop_native_sr_trees_shrubs <- overall0$tot_native_SR_trees_shrubs / 
  overall_regional_80th_native_sr_trees_shrubs$regional_80th_native_sr_trees_shrubs[match(overall0$Region, overall_regional_80th_native_sr_trees_shrubs$Region)]
overall0$prop_native_sr_trees_shrubs[overall0$prop_native_sr_trees_shrubs>1] <- 1


# overall
# could compare to alt WECA summing species over site

# plot score 3
# overall score 2


# look into relationship between plot max and overall estimate
plotmax_prop_native_sr_trees_shrubs <- long_plots0 %>%
  group_by(id) %>%
  summarise(
    plotmax_prop_native_sr_trees_shrubs = max(prop_native_sr_trees_shrubs, na.rm = TRUE)
  ) %>% 
  select(id, plotmax_prop_native_sr_trees_shrubs)
overall0 <- overall0 %>%
  left_join(plotmax_prop_native_sr_trees_shrubs, by = "id")

figure_plots$prop_native_sr_trees_shrubs.overall.plotmax <- ggplot(overall0, aes(x = prop_native_sr_trees_shrubs, y = plotmax_prop_native_sr_trees_shrubs,
                                                                                 color = sr_changed_in_curation)) +
  geom_jitter(width = 0.01, height = 0.01, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Overall", y = "Plot-max",
       title = "Proportion of expected native tree and shrub species present",
       color = "SR changed in curation") +
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )

figure_plots$prop_native_sr_trees_shrubs.plotmax_nplot <-   ggplot(overall0, aes(x = n_plots, 
                                                                                 y = plotmax_prop_native_sr_trees_shrubs / prop_native_sr_trees_shrubs,
                                                                                 color = sr_changed_in_curation)) +
  
  # background boxplot
  geom_boxplot(aes(group = n_plots),
               color = "black",
               fill = NA,
               width = 0.2,
               #dotn show outliers
               outlier.shape = NA
               ) +
  
  # jitter points
  geom_point(alpha = 0.5) +
  # geom_jitter(width = 0.015, height = 0.015, alpha = 0.5) +
  
  labs(x = "N plots", 
       y = "Plot-max as proportion of overall",
       title = "Proportion of expected native tree and shrub species present",
       color = "SR changed in curation") +
  
  theme_minimal() +
  theme(legend.position = "bottom")

# look into consideration of variation in scoring etc where plot level info exisits
# relate WECA scores to

## 7.5 invasives ------
#caveat some high therat not on list
# confidence score:
## plot score 3.5
## overall 2


# high threat = Invasives.Rhododendron, Invasives.Himalayan.Balsam, Invasives.Japanese.Knotweed, Invasives.Giant.Hogweed, 
# other = Invasives.Other

# high threat at plot = 0 score
long_plots0$high_threat_invasive <- pmax(
  long_plots0$Invasives.Rhododendron,
  long_plots0$Invasives.Himalayan.Balsam,
  long_plots0$Invasives.Japanese.Knotweed,
  na.rm = TRUE
)
overall0$high_threat_invasive <- pmax(
  overall0$Invasives.Rhododendron,
  overall0$Invasives.Himalayan.Balsam,
  overall0$Invasives.Japanese.Knotweed,
  na.rm = TRUE
)

# only other, value  R = 0.7  , O = 0.33ish  F = 0.10, A = 0

long_plots0$invasive_value_other <- case_when(
  long_plots0$Invasives.Other == "Absent" ~ 1,
  long_plots0$Invasives.Other == "R" ~ 0.7,
  long_plots0$Invasives.Other == "O" ~ 0.33,
  long_plots0$Invasives.Other == "F" ~ 0.1,
  long_plots0$Invasives.Other %in% c("A", "D") ~ 0
)
overall0$invasive_value_other <- case_when(
  overall0$Invasives.Other == "Absent" ~ 1,
  overall0$Invasives.Other == "R" ~ 0.7,
  overall0$Invasives.Other == "O" ~ 0.33,
  overall0$Invasives.Other == "F" ~ 0.1,
  overall0$Invasives.Other %in% c("A", "D") ~ 0
)

long_plots0$invasive_value_high_threat_multiplier <- ifelse(long_plots0$high_threat_invasive > "Absent", 0, 1)
# hi threat at overall
# R = 0.8
# O = 0.6
# F = 0.2 
# D-A = 0
overall0$invasive_value_high_threat_multiplier <- case_when(
  overall0$high_threat_invasive == "Absent" ~ 1,
  overall0$high_threat_invasive == "R" ~ 0.8,
  overall0$high_threat_invasive == "O" ~ 0.6,
  overall0$high_threat_invasive == "F" ~ 0.2,
  overall0$high_threat_invasive %in% c("A", "D") ~ 0
)

long_plots0$WECA_invasive_score <- long_plots0$invasive_value_other * long_plots0$invasive_value_high_threat_multiplier
overall0$WECA_invasive_score <- overall0$invasive_value_other * overall0$invasive_value_high_threat_multiplier


## 7.6 deadwood ------
# only two types - Deadwood.Standing, Deadwood.Fallen
## could compare to the standing and fallen parts of WECA 
## shoudl really calobrate /16 score rather than assume constancy between deadwood type accumulation and value
# confidence score:
## plot score 2
## overall score 1


# plot level, number of quarters assumed
# D = 4
# A = 2
# O & F = 1
#  R = 0
long_plots0$deadwood_score_standing <- case_when(
  long_plots0$Deadwood.Standing == "Absent" ~ 0,
  long_plots0$Deadwood.Standing == "R" ~ 0,
  long_plots0$Deadwood.Standing == "O" ~ 1,
  long_plots0$Deadwood.Standing == "F" ~ 1,
  long_plots0$Deadwood.Standing %in% c("A", "D") ~ 2
)
long_plots0$deadwood_score_fallen <- case_when(
  long_plots0$Deadwood.Fallen == "Absent" ~ 0,
  long_plots0$Deadwood.Fallen == "R" ~ 0,
  long_plots0$Deadwood.Fallen == "O" ~ 1,
  long_plots0$Deadwood.Fallen == "F" ~ 1,
  long_plots0$Deadwood.Fallen %in% c("A", "D") ~ 2
)
# overall
overall0$deadwood_score_standing <- case_when(
  overall0$Deadwood.Standing == "Absent" ~ 0,
  overall0$Deadwood.Standing == "R" ~ 0,
  overall0$Deadwood.Standing == "O" ~ 0.5,
  overall0$Deadwood.Standing == "F" ~ 0.5,
  overall0$Deadwood.Standing %in% c("A", "D") ~ 1
)
overall0$deadwood_score_fallen <- case_when(
  overall0$Deadwood.Fallen == "Absent" ~ 0,
  overall0$Deadwood.Fallen == "R" ~ 0,
  overall0$Deadwood.Fallen == "O" ~ 0.5,
  overall0$Deadwood.Fallen == "F" ~ 0.5,
  overall0$Deadwood.Fallen %in% c("A", "D") ~ 1
)

 

## 7.7 AVTs ------
# TA.Veteran.Pollards.Ancient.Coppice.Stools

# only 
# D  = 1
# A  = 0.75
# F = 0.5
# O = 0.25
# R = 0.1

overall0$WECA_avt_score <- case_when(
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "Absent" ~ 0,
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "R" ~ 0.1,
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "O" ~ 0.25,
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "F" ~ 0.5,
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "A" ~ 0.75,
  overall0$TA.Veteran.Pollards.Ancient.Coppice.Stools == "D" ~ 1
)

# overall score - 3

## 7.8 regeneration ------
# LTR.Seedlings.Less.Than.10cm, LTR.Seedlings.10.100cm, LTR.Saplings.Greater.Than.100cm, 
# LSR.Seedlings.Less.Than.10cm, LSR.Seedlings.10.100cm, LSR.Saplings.Greater.Than.100cm, 
# TA young
# max 

long_plots0$seedlings <- pmax(
  long_plots0$LTR.Seedlings.Less.Than.10cm,
  long_plots0$LTR.Seedlings.10.100cm,
  long_plots0$LSR.Seedlings.Less.Than.10cm,
  long_plots0$LSR.Seedlings.10.100cm,
  na.rm = TRUE
)
overall0$seedlings <- pmax(
  overall0$LTR.Seedlings.Less.Than.10cm,
  overall0$LTR.Seedlings.10.100cm,
  overall0$LSR.Seedlings.Less.Than.10cm,
  overall0$LSR.Seedlings.10.100cm,
  na.rm = TRUE
)
long_plots0$saplings <- pmax(
  long_plots0$LTR.Saplings.Greater.Than.100cm,
  long_plots0$LSR.Saplings.Greater.Than.100cm,
  na.rm = TRUE
)
overall0$saplings <- pmax(
  overall0$LTR.Saplings.Greater.Than.100cm,
  overall0$LSR.Saplings.Greater.Than.100cm,
  na.rm = TRUE
)
long_plots0$TA_young <- long_plots0$TA.Less.Than.20.Years
overall0$TA_young <- overall0$TA.Less.Than.20.Years

long_plots0$regeneration_score <- rowSums(
  cbind(1*(long_plots0$seedlings > "Absent"),
        1*(long_plots0$saplings > "Absent"),
        1*(long_plots0$TA_young > "Absent")),
  na.rm = TRUE
)
overall0$regeneration_score <- rowSums(
  cbind(1*(overall_weighting_lookup[overall0$seedlings] %>% as.numeric()),
        1*(overall_weighting_lookup[overall0$saplings] %>% as.numeric()),
        1*(overall_weighting_lookup[overall0$TA_young] %>% as.numeric())),
  na.rm = TRUE
)

# plot score 5
# overall score 3


## 7.9 Herbivore impact ------
# HIA survey info
## Animal.Damage.Deer, Animal.Damage.Other

# extremes ok
# A  all palitable

# compare

# rubbish talk to NRB

# plot score 2
# overall score 1



## 7.10 tree health ------
# Tree.Health.Notifiable.Pest.Or.Disease Tree.Health.Other.Disease.Or.Pest

# plot
# Tree.Health.Notifiable.Pest.Or.Disease present = 0
# Tree.Health.Other.Disease.Or.Pest DAF = 0, O = 10, R = 0.75

long_plots0$tree_health_notifiable_multiplier <- ifelse(long_plots0$Tree.Health.Notifiable.Pest.Or.Disease == "Absent", 1, 0)
long_plots0$tree_health_other_multiplier <- case_when(
  long_plots0$Tree.Health.Other.Disease.Or.Pest == "Absent" ~ 1,
  long_plots0$Tree.Health.Other.Disease.Or.Pest == "R" ~ 0.75,
  long_plots0$Tree.Health.Other.Disease.Or.Pest == "O" ~ 0.5,
  long_plots0$Tree.Health.Other.Disease.Or.Pest == "F" ~ 0.25,
  long_plots0$Tree.Health.Other.Disease.Or.Pest %in% c("A", "D") ~ 0
)
long_plots0$WECA_tree_health_score <- long_plots0$tree_health_notifiable_multiplier * long_plots0$tree_health_other_multiplier

# overall
## same logic as high threat invasive
overall0$tree_health_notifiable_multiplier <- case_when(
  overall0$Tree.Health.Notifiable.Pest.Or.Disease == "Absent" ~ 1,
  overall0$Tree.Health.Notifiable.Pest.Or.Disease == "R" ~ 0.8,
  overall0$Tree.Health.Notifiable.Pest.Or.Disease == "O" ~ 0.6,
  overall0$Tree.Health.Notifiable.Pest.Or.Disease == "F" ~ 0.2,
  overall0$Tree.Health.Notifiable.Pest.Or.Disease %in% c("A", "D") ~ 0
)
overall0$tree_health_other_multiplier <- case_when(
  overall0$Tree.Health.Other.Disease.Or.Pest == "Absent" ~ 1,
  overall0$Tree.Health.Other.Disease.Or.Pest == "R" ~ 0.75,
  overall0$Tree.Health.Other.Disease.Or.Pest == "O" ~ 0.5,
  overall0$Tree.Health.Other.Disease.Or.Pest == "F" ~ 0.25,
  overall0$Tree.Health.Other.Disease.Or.Pest %in% c("A", "D") ~ 0
)
overall0$WECA_tree_health_score <- overall0$tree_health_notifiable_multiplier * overall0$tree_health_other_multiplier

# plot score 2
# overall score 1.5


## 7.11 ground flora ------
# Flora.Ancient.Woodland.Plants..Specialists., Flora.Other.Woodland.Plants..Generalists., Flora.Other.Native.Plants, Flora.Coarse.Vegetation, Flora.Other.Plants, Flora.No.Vegitation

# plot score 0
# overall score 0


## 7.12 Horizontal complexity ------
# only at overall level
## TotalTreeCanopyCoverPercentCurrent 
## OpenSpaceSemiNaturalHabitatPercentCurrent OpenSpaceRidesPercentCurrent OpenSpaceTemporaryPercentCurrent OpenSpaceWaterFeaturesPercentCurrent
## ShrubCoverPercentCurrent
# plot score 0
# overall score 1



## 7.13 human impact ------
# Human.Impacts.One.Off.Impacts Human.Impacts.Continuous.Impacts

# max of each
# DAF = 0
# O  = 0.4
# R  = 0.8
# Absent = 1

long_plots0$human_impact_cover <- pmax(
  long_plots0$Human.Impacts.One.Off.Impacts,
  long_plots0$Human.Impacts.Continuous.Impacts,
  na.rm = TRUE
)
long_plots0$WECA_human_impact_score <- case_when(
  long_plots0$human_impact_cover == "Absent" ~ 1,
  long_plots0$human_impact_cover == "R" ~ 0.8,
  long_plots0$human_impact_cover == "O" ~ 0.4,
  long_plots0$human_impact_cover %in% c("F", "A", "D") ~ 0
)

overall0$human_impact_cover <- pmax(
  overall0$Human.Impacts.One.Off.Impacts,
  overall0$Human.Impacts.Continuous.Impacts,
  na.rm = TRUE
)
overall0$WECA_human_impact_score <- case_when(
  overall0$human_impact_cover == "Absent" ~ 1,
  overall0$human_impact_cover == "R" ~ 0.8,
  overall0$human_impact_cover == "O" ~ 0.4,
  overall0$human_impact_cover %in% c("F", "A", "D") ~ 0
)

# plot score 2
# overall score 1.5




## 7.14 Microhabitats ------
#0

# plot 0
# overall0
# 



# end ----

long_plots <- long_plots0 
rm(long_plots0)

overall <- overall0
rm(overall0)