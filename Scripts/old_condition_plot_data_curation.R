# Curation of old condition data prior to indicator calculation


long_plots0 <- long_plots

results$data_description <- list(NA)
results$data_description$nplots_og = nrow(long_plots0)
results$data_description$n_surveys_og <- length(unique(long_plots0$id))
results$data_description$nplot_sites_og <- length(unique(long_plots0$Site.Name))
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


# 1. remove rows with NA across all tree related information (age class cover, regeneration, species count) ----
## identify tree rows
plot_tree_cols <- indicator_types$indicator[indicator_types$theme %in% c("tree_age", "tree_shrub_species_richness", 
                                                                         "shrub_cover", 
                                                                         "regeneration_level", "regeneration_species_richness")]
plot_tree_cols <- plot_tree_cols[!grepl("Dominated", plot_tree_cols)] # remove those containing "Dominated"
## identify rows where all NA
na_tree_rows = which(
  long_plots0[,plot_tree_cols] %>% is.na() %>% rowSums() == length(plot_tree_cols)
)
# remove na_tree_rows
long_plots0 <- long_plots0[-na_tree_rows, ]

results$data_description$nplots_no_tree_info = length(na_tree_rows)
results$data_description$nplots_tree_info = nrow(long_plots0)
results$data_description$n_surveys_tree_info <- length(unique(long_plots0$id))
results$data_description$nplot_sites_tree_info <- length(unique(long_plots0$Site.Name))

curation_notes <- c(
  curation_notes,
  paste0( "- Of original data", results$data_description$nplots_no_tree_info, " plots contained no information on trees or shrubs (tree age, species counts, shrub cover or regneration)" ),
  paste0( "- Data from those ",  results$data_description$nplots_no_tree_info ," plots without tree/shrub information were removed, leaving data from ", 
          results$data_description$nplots_tree_info, " plots, covering ", results$data_description$n_surveys_tree_info, " surveys over ",  results$data_description$nplot_sites_tree_info, " sites" )
)


# 2. Manipulations where data entered wrong ----
## 2.1 sometimes species richness was confused as % canopy nativness for natives and non natives - really high sr values given ----

richness_indicators <- indicator_types$indicator[indicator_types$theme %in% c( "tree_shrub_species_richness",
                                                                               "regeneration_species_richness")]
# 
# aa = long_plots0[, c(richness_indicators,
#                      "tot_tree_SR", "tot_shrub_SR", "tot_tree_regn_SR", "tot_shrub_regen_SR")]

sr_thought_to_be_percent_error_rows = which(
  long_plots0$TS.Native.richness >= 20 | # inspected aa manuely to find that threshold
    long_plots0$TS.Non.Native.richness >= 20|
    long_plots0$SS.Native.richness >= 20 |
    long_plots0$SS.Non.Native.richness >= 20 |
    long_plots0$RSS.Native.richness >= 20 |
    long_plots0$RSS.Non.Native.richness >= 20|
    long_plots0$RTS.Native.richness >= 20 |
    long_plots0$RTS.Non.Native.richness >= 20
)

long_plots0[sr_thought_to_be_percent_error_rows, 
  richness_indicators] <- NA

curation_notes <- c(
  curation_notes,
  paste0("- For ", length(sr_thought_to_be_percent_error_rows) , " plots very high tree/shrub species richness values were entered that were likely data entry errors (confused with % canopy cover).",
         "  All species richness entries were replaced with NA in those plots."
  )
)

## 2.2 cases where 0 species richness was recorded, but the presence of trees/shrubs was indicated elsewhere (eg tree age class cover, shrub cover, regeneration level) ----

wrong_zero_ts <- which(
  row_sum_na_if_all_na(long_plots0$TS.Native.richness,  long_plots0$TS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, indicator_types$indicator [indicator_types$theme == "tree_age"])
)
long_plots0$TS.Native.richness[wrong_zero_ts] <- NA
long_plots0$TS.Non.Native.richness[wrong_zero_ts] <- NA

wrong_zero_ss <- which(
  row_sum_na_if_all_na(long_plots0$SS.Native.richness,  long_plots0$SS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, indicator_types$indicator [indicator_types$theme == "shrub_cover"])
)
long_plots0$SS.Non.Native.richness[wrong_zero_ss] <- NA
long_plots0$SS.Native.richness[wrong_zero_ss] <- NA

wrong_zero_ts.regen <- which(
  row_sum_na_if_all_na(long_plots0$RTS.Native.richness,  long_plots0$RTS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, tree.regen.level.indicators)
)
long_plots0$RTS.Native.richness[wrong_zero_ts.regen] <- NA
long_plots0$RTS.Non.Native.richness[wrong_zero_ts.regen] <- NA

wrong_zero_ss.regen <- which(
  row_sum_na_if_all_na(long_plots0$RSS.Native.richness,  long_plots0$RSS.Non.Native.richness) == 0 &
    has_indicator(long_plots0, shrub.regen.level.indicators)
)
long_plots0$RSS.Native.richness[wrong_zero_ss.regen] <- NA
long_plots0$RSS.Non.Native.richness[wrong_zero_ss.regen] <- NA

curation_notes <- c(
  curation_notes,
  paste0("For ", length(unique(c(wrong_zero_ts, wrong_zero_ss, wrong_zero_ts.regen, wrong_zero_ss.regen) )) , " plots tree/shrub/regen species richness was recorded as 0, but the presence of recorded cover for the corresponding vegetation suggested this was an error. These 0s were replaced with NAs at: \n",
         " - ", length(wrong_zero_ts) , " plots for tree species richness\n",  
         " - ", length(wrong_zero_ss) , " plots for shrub species richness\n",
         " - ", length(wrong_zero_ts.regen) , " plots for tree regeneration species richness\n",
         " - ", length(wrong_zero_ss.regen) , " plots for shrub regeneration species richness\n"
  )
)

# 3. filling accidentally missing indicator values ----
## implied observations where plot exists but indicator is NA
### DAFOR "Absent"
### Logical FALSE
### Numeric/Richness 0 (no filling)
##### 3.1 Tree/shrub species richness indicators:  ----

tree_shrub_species_richness.indicators <- indicator_types$indicator[indicator_types$theme %in% c( "tree_shrub_species_richness") &
                                                                   indicator_types$type %in% c("numeric", "richness")]

tree_species_richness.indicators <- c("TS.Native.richness", "TS.Non.Native.richness")
shrub_species_richness.indicators <- c("SS.Native.richness", "SS.Non.Native.richness")

results$data_description$n_plots_missing_richness_any_before <- sum(
  rowSums(is.na(long_plots0[tree_shrub_species_richness.indicators])) > 0
)


###### 3.1.1 - where all tree/shrub species richness info absent, and corresponding  tree/shrub cover also absent ----

# trees
na_tree_sr_rows_no_tree_info = which(
  # na in all tree species richness cols
  long_plots0[,tree_species_richness.indicators] %>% is.na() %>% rowSums() == length(tree_species_richness.indicators) &
    # na in all tree age cols
    !has_indicator(long_plots0, tree.ageclass.indicators)
)
long_plots0[na_tree_sr_rows_no_tree_info, tree_species_richness.indicators] <- 0

# shrubs
na_shrub_sr_rows_no_shrub_info = which(
  # all shrub species richness missing
  rowSums(is.na(long_plots0[, shrub_species_richness.indicators, drop = FALSE])) == length(shrub_species_richness.indicators) &
    
    # all shrub cover missing
    !has_indicator(long_plots0, shrub.cover.indicators) &
    
    # but some tree age or SR info present - support that something WAS measured
    (has_indicator(long_plots0, tree.ageclass.indicators) |
     rowSums(is.na(long_plots0[, tree_species_richness.indicators, drop = FALSE])) < length(tree_species_richness.indicators)
    )
)
long_plots0[na_shrub_sr_rows_no_shrub_info, shrub_species_richness.indicators] <- 0


###### 3.1.2 - where only one of native/non native filled for species richness ----

# trees
na_tree_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  long_plots0[,tree_species_richness.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,tree_species_richness.indicators] %>% is.na() %>% rowSums() < length(tree_species_richness.indicators)
)
# replace those nas with 0
long_plots0[na_tree_sr_rows_some, tree_species_richness.indicators] <-
  long_plots0[na_tree_sr_rows_some, tree_species_richness.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

# shrubs
na_shrub_sr_rows_some = which(
  # na in some tree/shrub species richness cols
  long_plots0[,shrub_species_richness.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,shrub_species_richness.indicators] %>% is.na() %>% rowSums() < length(shrub_species_richness.indicators)
)
# replace those nas with 0
long_plots0[na_shrub_sr_rows_some, shrub_species_richness.indicators] <-
  long_plots0[na_shrub_sr_rows_some, shrub_species_richness.indicators] %>%
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

###### 3.1.XX - curation notes etc ----

na_remaining_tree_sr_rows = which(
  # na in all tree species richness cols
  long_plots0[,tree_species_richness.indicators] %>% is.na() %>% rowSums() > 0
)
results$data_description$n_plots_missing_tree_richness_any_after <- length(na_remaining_tree_sr_rows)

na_remaining_shrub_sr_rows = which(
  # na in all shrub species richness cols
  long_plots0[,shrub_species_richness.indicators] %>% is.na() %>% rowSums() > 0
)
results$data_description$n_plots_missing_shrub_richness_any_after <- length(na_remaining_shrub_sr_rows)

na_remaining_tree_shrub_sr_rows = which(
  # na in all tree/shrub species richness cols
  long_plots0[,tree_shrub_species_richness.indicators] %>% is.na() %>% rowSums() > length(tree_shrub_species_richness.indicators)
)
results$data_description$n_plots_missing_treeandshrub_richness_any_after <- length(na_remaining_tree_shrub_sr_rows)

curation_notes <- c(
  curation_notes,
  paste0("For ", results$data_description$n_plots_missing_richness_any_before, " plots at least one tree/shrub species richness datum was missing.", 
         " Implied zeros were assumed for in the following cases: /n",
         " - The missing tree species richness data at ", length(na_tree_sr_rows_no_tree_info), " plots that also had entirely missing tree age class information. \n",
         " - The missing shrub species richness data at ", length(na_shrub_sr_rows_no_shrub_info), " plots that also had missing shrub cover information, provided at least some data was recorded for tree age or tree species richness. \n",
         " - The further missing tree species richness data at ", length(na_tree_sr_rows_some), " plots that recorded only one of native or non-native tree species richness. \n",
         " - The further missing shrub species richness data at ",  length(na_shrub_sr_rows_some), " plots that recorded only one of native or non-native shrub species richness. \n",
         " This left ", results$data_description$n_plots_missing_tree_richness_any_after, " plots with no tree species richness information, ",
         results$data_description$n_plots_missing_shrub_richness_any_after, " plots with no shrub species richness information (",
         length(results$data_description$n_plots_missing_treeandshrub_richness_any_after)," plots with neither)."
  )
)

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

##### 3.2 Tree age indicators ----
###### 3.2.1 - where some age measurements present ----
results$data_description$n_plots_missing_ta_any <- sum(
  rowSums(is.na(long_plots0[tree.ageclass.indicators])) > 0
)
results$data_description$n_plots_missing_ta_all <- sum(
  rowSums(is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "tree_age"]])) > 0 &
    rowSums(!is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "tree_age"]])) == 0
)

long_plots0 <- long_plots0 %>% 
  fill_assumed_indicators(
    .,
    indicator_types$indicator [indicator_types$theme == "tree_age"],
    indicator_types
  ) 


###### 3.2.2 - where all age class info absent, but tree sr is 0 ----
na_ta_rows_0sr = which(
  # na in all tree age cols & total tree species richness is 0
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() == length(indicator_types$indicator [indicator_types$theme == "tree_age"]) &
    long_plots0$tot_tree_SR == 0
)

long_plots0[na_ta_rows_0sr, indicator_types$indicator [indicator_types$theme == "tree_age"]] <- "Absent"


###### 3.2.3 - where all age class info absent, but shrub cover or richness present ----
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

###### 3.2.4 - where all age class info absent, but regeneration level or regeneration species info present ----
na_ta_rows_regen = which(
  # na in all tree age cols & any regeneration level or regeneration species richness info present
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() == sum(indicator_types$theme == "tree_age")  &
    ((long_plots0[,indicator_types$indicator [indicator_types$theme == "regeneration_level"]] %>% is.na() %>% rowSums() < sum(indicator_types$theme == "regeneration_level") ) |
       (long_plots0[,indicator_types$indicator [indicator_types$theme == "regeneration_species_richness"]] %>% is.na() %>% rowSums() < sum(indicator_types$theme == "regeneration_species_richness") ))
)

long_plots0[na_ta_rows_regen, indicator_types$indicator [indicator_types$theme == "tree_age"]] <- "Absent" 

###### 3.2.XX - curation notes etc ----

na_remaining_tree_age_rows = which(
  # na in all tree age cols
  long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] %>% is.na() %>% rowSums() > 0
)

results$data_description$n_plots_missing_ta_after <- length(na_remaining_tree_age_rows)

curation_notes <- c(
  curation_notes,
  paste0("- For ", results$data_description$n_plots_missing_ta_any, " plots at least one tree age datum was missing.",
         " For ", 
         results$data_description$n_plots_missing_ta_any - results$data_description$n_plots_missing_ta_all ,  
         " of these data was recorded for at least one tree age indicator indicator,", 
         " a further ", length(na_ta_rows_0sr), " recorded tree species richness as 0,", 
         " a further ", length(na_ta_rows_shrub), " contained shrub cover or shrub species richness information,",
         " and a further ", length(na_ta_rows_regen), " contained information on the level or species of regnerating trees/shrubs.",
         " All missing tree age class information in those plots was assumed to imply the abense of that age class, leaving ", 
         results$data_description$n_plots_missing_ta_after, " plots with no tree age information."
  )
)


##### 3.3 Shrub cover  ----
####### 3.3.1 - shrub cover absent but shrub sr 0 ----

results$data_description$n_plots_missing_shrub_cover_any <- sum(
  is.na(long_plots0[indicator_types$indicator [indicator_types$theme == "shrub_cover"]]))

na_shrub_cover_rows_0sr = which(
  # na in all shrub cover cols & total shrub species richness is 0
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()  &
    long_plots0$tot_shrub_SR == 0
)
long_plots0[na_shrub_cover_rows_0sr, indicator_types$indicator [indicator_types$theme == "shrub_cover"]] <- "Absent"


####### 3.3.2 - shrub cover absent but tree age or species info present ----
na_shrub_cover_rows_ta_sr = which(
  # na in all shrub cover cols & any tree age class or tree species info present
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()  &
    (!is.na(long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_age"]] ) %>% rowSums() > 0 |
       !is.na(long_plots0[,indicator_types$indicator [indicator_types$theme == "tree_shrub_species_richness"]]) %>% rowSums() > 0
    )
)
long_plots0[na_shrub_cover_rows_ta_sr, indicator_types$indicator [indicator_types$theme == "shrub_cover"]] <- "Absent"


###### 3.3.XX - curation notes etc ----
na_remaining_shrub_cover_rows = which(
  # na in all shrub cover cols
  long_plots0[,indicator_types$indicator [indicator_types$theme == "shrub_cover"]] %>% is.na()
)
results$data_description$n_plots_missing_shrub_cover_after <- length(na_remaining_shrub_cover_rows)

curation_notes <- c(
  curation_notes,
  paste0("- For ", results$data_description$n_plots_missing_shrub_cover_any, " plots shrub cover information was missing. ",
         "For ", length(na_shrub_cover_rows_0sr), " of these, total shrub species richness was recorded as 0, ",
         " and a further ", length(na_shrub_cover_rows_ta_sr), " contained tree age or tree species richness information.",
         " Shrub cover was assumed to be absent in those plots, leaving ",
         results$data_description$n_plots_missing_shrub_cover_after,
         " plots with no shrub cover information"
  )
)

##### 3.4 Regeneration species richness  ----
regen.species.indicators <- indicator_types$indicator[indicator_types$theme == "regeneration_species_richness" &
                                                        indicator_types$type %in% c("numeric", "richness")] 

results$data_description$n.missing.regen.species_before <- sum(
  rowSums(is.na(long_plots0[regen.species.indicators])) > 0
)

###### 3.4.1 - where all regen tree/shrub species richness info absent, and corresponding  tree/shrub regen level absent ----

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


###### 3.4.2 - where only one of native/non native filled for regen species richness ---- ----

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
         

###### 3.4.XX - curation notes etc ----

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

curation_notes <- c(
  curation_notes,
  paste0("For ", results$data_description$n.missing.regen.species_before, " plots at least one regeneration species richness datum was missing.",
         " Implied zeros were assumed for in the following cases: /n",
         " - The missing regenerating tree species richness data at ", length(na_treeregen_sr_rows_no_treeregen_level), " plots that also had entirely missing tree regeneration level information. \n",
         " - The missing regenerating shrub species richness data at ", length(na_shrubregen_sr_rows_no_shrubregen_level), " plots that also had entirely missing shrub regeneration level information. \n",
         " - The further missing regenerating tree species richness data at ", length(na_treeregen_sr_rows_some), " plots that recorded only one of native or non-native tree regeneration species richness. \n",
         " - The further missing regenerating shrub species richness data at ",  length(na_shrubregen_sr_rows_some), " plots that recorded only one of native or non-native shrub regeneration species richness. \n",
         " This left ", length(na_remainging_tree_regen_sr_rows), " plots with no tree regeneration species richness information and ",
         length(na_remainging_shrub_regen_sr_rows), " plots with no shrub regeneration species richness information,",
         " (", length(na_remaining_regen_sr_rows), " plots had neither)."
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

##### 3.5 regen level ----

results$data_description$n.missing.regen.level_before <- sum(
  rowSums(is.na(long_plots0[regen.level.indicators])) > 0
)

###### 3.5.1 - where some regen level measurements present ----

na_regen_level_rows_some = which(
  # na in some regen level cols
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0 &
    # but data present for at least one of them
    long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() < length(regen.level.indicators)
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    regen.level.indicators,
    indicator_types
  )

na_regen_level_rows_all = which(
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() == length(regen.level.indicators)
)


###### 3.5.2 - where regen level info absent, but tree/shrub regen sr is 0 or missing -----
# note where both were originally missing, sr will now be 0 

# tree
na_treeregen_level_rows_0treeregen_sr = which(
  # na in all tree regen level cols & tree regen species richness is 0
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() == length(tree.regen.level.indicators) &
    (long_plots0$tot_tree_regn_SR == 0 |is.na(long_plots0$tot_tree_regn_SR))
)
long_plots0[na_treeregen_level_rows_0treeregen_sr, tree.regen.level.indicators] <- "Absent"

# shrub
na_shrubregen_level_rows_0shrubregen_sr = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() == length(shrub.regen.level.indicators) &
    (long_plots0$tot_shrub_regen_SR == 0 | is.na(long_plots0$tot_shrub_regen_SR))
)
long_plots0[na_shrubregen_level_rows_0shrubregen_sr, shrub.regen.level.indicators] <- "Absent"
  

###### 3.5.XX  - curation notes etc ----

na_treeregen_level_after = which(
  # na in all tree regen level cols & tree regen species richness is 0
  long_plots0[,tree.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_treeregen_level_after_sr <- long_plots0$tot_tree_regn_SR[na_treeregen_level_after]

na_shrubregen_level_after = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  long_plots0[,shrub.regen.level.indicators] %>% is.na() %>% rowSums() > 0
)
na_shrubregen_level_after_sr <- long_plots0$tot_shrub_regen_SR[na_shrubregen_level_after]

na_anyregen_level_after = which(
  # na in all shrub regen level cols & shrub regen species richness is 0
  long_plots0[,regen.level.indicators] %>% is.na() %>% rowSums() > 0
)


curation_notes <- c(
  curation_notes,
  paste0("- For ", results$data_description$n.missing.regen.level_before, " plots at least one regeneration level datum was missing.",
         " Of these, ", length(na_regen_level_rows_some)," plots had data entered for at least one regeneration level,",
         " a further ", length(na_regen_level_rows_0regensr), " plots recorded 0 regeneration species richness for both trees and shrubs,",
         " a further ", length(na_regen_level_rows_0regensr_either), " plots recorded 0 regeneration species richness for either trees or shrubs,",
         " and a further ", length(na_regen_level_rows_ta_shrub_sr), " plots contained tree age class cover, shrub cover or tree/shrub species richness information.",
         " All missing regeneration level information in those plots was assumed to imply the absence of regeneration at that level, leaving ",
         results$data_description$n.missing.regen.level_after, " plots with no regeneration level information"
  )
)


##### 3.6 Flora and Deadwood indicators ----

flora.indicators <- indicator_types$indicator[indicator_types$theme == "flora"]
deadwood.indicators <- indicator_types$indicator[indicator_types$theme == "deadwood"]

flora.deadwood.indicators <- c(flora.indicators, deadwood.indicators)

results$data_description$n.missing.flora_before <- sum(
  rowSums(is.na(long_plots0[flora.indicators])) > 0
)
results$data_description$n.missing.deadwood_before <- sum(
  rowSums(is.na(long_plots0[deadwood.indicators])) > 0
)
results$data_description$n.missing.flora_deadwood_before <- sum(
  rowSums(is.na(long_plots0[flora.deadwood.indicators])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    flora.deadwood.indicators,
    indicator_types
  )

results$data_description$n.missing.flora_after <- sum(
  rowSums(is.na(long_plots0[flora.indicators])) > 0
)
results$data_description$n.missing.deadwood_after <- sum(
  rowSums(is.na(long_plots0[deadwood.indicators])) > 0
)


curation_notes <- c(
  curation_notes,
  paste0("- For ", results$data_description$n.missing.flora_deadwood_before,
         " plots at least one datum was missing from flora (",
         results$data_description$n.missing.flora_before,
         " plots) or deadwood (",
         results$data_description$n.missing.deadwood_before,
         " plots).",
         " Of these, ", results$data_description$n.missing.flora_deadwood_before - results$data_description$n.missing.flora_after,
         " plots had data entered for at least one of these 'features' indicators (",
         results$data_description$n.missing.flora_before - results$data_description$n.missing.flora_after ," for flora and ",
         results$data_description$n.missing.deadwood_before - results$data_description$n.missing.flora_after,  " for deadwood).",
         " All the missing data for flora or deadwood in these plots where assumed to be implied absences, leaving ",
         results$data_description$n.missing.flora_after, " plots with no flora or deadwood information."
  )  )

##### 3.7 Invasives, Animal damage, Human impact or Tree health: contains "Invasives.", "Animal.Damage.", "Human.Impacts.", "Tree.Health." ----
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

results$data_description$n.missing.invasives_before <- sum(
  rowSums(is.na(long_plots0[invasives.indicators])) > 0
)
results$data_description$n.missing.animal_damage_before <- sum(
  rowSums(is.na(long_plots0[animal_damage.indicators])) > 0
)
results$data_description$n.missing.human_impact_before <- sum(
  rowSums(is.na(long_plots0[human_impact.indicators])) > 0
)
results$data_description$n.missing.tree_health_before <- sum(
  rowSums(is.na(long_plots0[tree_health.indicators])) > 0
)
results$data_description$n.missing.threats_before <- sum(
  rowSums(is.na(long_plots0[threats.indicators])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    threats.indicators,
    indicator_types
  )

results$data_description$n.missing.invasives_after <- sum(
  rowSums(is.na(long_plots0[invasives.indicators])) > 0
)
results$data_description$n.missing.animal_damage_after <- sum(
  rowSums(is.na(long_plots0[animal_damage.indicators])) > 0
)
results$data_description$n.missing.human_impact_after <- sum(
  rowSums(is.na(long_plots0[human_impact.indicators])) > 0
)
results$data_description$n.missing.tree_health_after <- sum(
  rowSums(is.na(long_plots0[tree_health.indicators])) > 0
)

results$data_description$n.missing.threats_after <- sum(
  rowSums(is.na(long_plots0[threats.indicators])) > 0
)

curation_notes <- c(
  curation_notes,
  paste0("- For ", 
         results$data_description$n.missing.threats_before,
         " plots at least one datum was missing across threats, including: invasives (",
         results$data_description$n.missing.invasives_before,
         " plots), animal damage (",
         results$data_description$n.missing.animal_damage_before,
         " plots), human impact (",
         results$data_description$n.missing.human_impact_before,
         " plots) and tree health (",
         results$data_description$n.missing.tree_health_before,
         " plots).",
         " Of these, ", results$data_description$n.missing.threats_before - results$data_description$n.missing.human_impact_after,
         " plots had data entered for at least one of these 'threats' indicators", 
         " (", results$data_description$n.missing.invasives_before - results$data_description$n.missing.invasives_after, " for invasives; ",
         results$data_description$n.missing.animal_damage_before - results$data_description$n.missing.animal_damage_after, " for animal damage; ",
         results$data_description$n.missing.human_impact_before - results$data_description$n.missing.human_impact_after, " for human impact; ",
         results$data_description$n.missing.tree_health_before - results$data_description$n.missing.tree_health_after, " for tree health).",
         " All the missing data for threats in these plots where assumed to be implied absences, leaving ",
         results$data_description$n.missing.invasives_after,
         " plots with no information on threats."
         
  )  )

##### 3.8 remaining empty feature or threat indicators ----

results$data_description$n.missing.features_threats_before <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)

long_plots0 <- long_plots0 %>%
  fill_assumed_indicators(
    .,
    c(flora.deadwood.indicators, threats.indicators),
    indicator_types
  )

results$data_description$n.missing.features_threats_after <- sum(
  rowSums(is.na(long_plots0[c(flora.deadwood.indicators, threats.indicators)])) > 0
)

curation_notes <- c(
  curation_notes,
  paste0("- Of the ", results$data_description$n.missing.features_threats_before, 
         " plots remaining that contained missing data across features (N = ", results$data_description$n.missing.deadwood_after, 
         ") or threats (N = ", results$data_description$n.missing.invasives_after ,"), ", 
         results$data_description$n.missing.features_threats_before - results$data_description$n.missing.features_threats_after ,
         " contained information on at least one of those indicators. These were assumed to be implied absences, leaving ", 
         results$data_description$n.missing.features_threats_after, " plots with no information on features or threats."
  )  )

##### 
# 4. fix "Dominated.By.One.Or.Two.SPP" info ----
## "Dominated.By.One.Or.Two.SPP" is never NA - where blank it is "no", even where no corresponeding species are recorded (eitehr 0 or NA)
##### set to NA where corresponding species richness is 0 or NA

results$data_description$n.fixed_dominated_rts <- sum(
  long_plots0$RTS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_regn_SR == 0 |
                                                is.na(long_plots0$tot_tree_regn_SR)] == FALSE,
  na.rm = TRUE
)
results$data_description$n.fixed_dominated_rss <- sum(
  long_plots0$RSS.Dominated.By.One.Or.Two.SPP[long_plots0$tot_shrub_regen_SR == 0 |
                                                is.na(long_plots0$tot_shrub_regen_SR)] == FALSE,
  na.rm = TRUE
)
results$data_description$n.fixed_dominated_ts <- sum(
  long_plots0$TS.Canopy.Dominated.By.One.Or.Two.SPP[long_plots0$tot_tree_SR == 0 |
                                                      is.na(long_plots0$tot_tree_SR)] == FALSE,
  na.rm = TRUE
)
results$data_description$n.fixed_dominated_ss <- sum(
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

curation_notes <- c(
  curation_notes,
  paste0(
    "- Orignally 'Dominated by one or two species', indicators were FALSE by default and manually overwritten to TRUE when needed. ",
    "This left FALSE values even when records of zeo or NA species richness implied the indicator should be NA.", 
    " In those cases (",
    results$data_description$n.fixed_dominated_rts, " for renerating trees, ", 
    results$data_description$n.fixed_dominated_rss, " for regenerating shrubs, ", 
    results$data_description$n.fixed_dominated_ts, " for tree species, ",
    results$data_description$n.fixed_dominated_ss, " for shrub species",
    "), the indicator was changed to 'NA'"
  )
)


# 5. Understand what missing data remains ----

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



curation_notes <- c(
  curation_notes,
  paste0("The curated dataset contained information from ", nrow(long_plots0), 
         " plots across ", length(unique(long_plots0$id)) ,
         " individual surveys across ", length(unique(long_plots0$Site.Name)) ," 
         individual sites, spanning from ", 
         long_plots0$ActualObservationDate %>% 
           as.Date(format = "%d/%m/%Y") %>%
           format("%Y") %>% min(na.rm = T),
         " to ", 
         long_plots0$ActualObservationDate %>% 
           as.Date(format = "%d/%m/%Y") %>%
           format("%Y") %>% max(na.rm = T),
         ". Of these ", sum(long_plots0$na_count>0), 
         " plots (", round(100*sum(long_plots0$na_count>0)/nrow(long_plots0),2), "%) still contained missing data for at least one indicator after curation:\n",
         "Tree/shrub species richness: ", results$data_description$n_plots_missing_richness_any_after, " plots missing data.\n",
         "Tree age category covers: ", results$data_description$n_plots_missing_ta_after, " plots missing data.\n",
         "Shrub cover: ", results$data_description$n_plots_missing_shrub_cover_after, " plots missing data.\n",
         "Regeneration species richness: ", results$data_description$n_plots_missing_regen_sr_after, " plots missing data.\n",
         "Regeneration level: ", results$data_description$n.missing.regen.level_after, " plots missing data.\n",
         "Features and threats indicators: ", results$data_description$n.missing.features_threats_after, " plots missing data."
         ),
  paste0("- Notably, some plots contained data that implied an impossibility:\n",
         "e.g., shrub species richness of 0 but non-Absent shrub cover (n = ", 
         sum( long_plots0$tot_shrub_SR == 0 &
                has_indicator(long_plots0, shrub.cover.indicators)
           , na.rm = T
         ), " plots);\n",
         "Tree species richness of 0 but non-Absent across all tree age class covers (n = ",
         sum(long_plots0$tot_tree_SR == 0 &
               has_indicator(long_plots0, tree.ageclass.indicators), 
                 na.rm = T), " plots);\n",
         "Regenerating tree species richness of 0 but non-Absent regenerating tree cover (n = ",
         sum(long_plots0$tot_tree_regn_SR == 0 &
               has_indicator(long_plots0, tree.regen.level.indicators), 
                 na.rm = T), " plots);\n",
         "Regenerating shrub species richness of 0 but non-Absent regenerating shrub cover (n = ",
         sum(long_plots0$tot_shrub_regen_SR == 0 &
               has_indicator(long_plots0, shrub.regen.level.indicators), 
                 na.rm = T), " plots);\n",
         "These inconsistencies were not curated further, but users should be aware of them when using the data."
  )
)

