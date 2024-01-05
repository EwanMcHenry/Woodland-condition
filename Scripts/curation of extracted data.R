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
) %>% merge(., ind.matcher.df, by.x = "indicator_name", by.y = "indicator_name_for_extraction", all.x = T) # this is wrong -- its sheet name.. but its fine

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
                  c("indicator_name", "respondant_name", "cert_val_funct", "cert_weight", "sheet_name", "weight")] %>% 
  mutate(respondant_name = as.factor(respondant_name),
         indicator_name = as.factor(indicator_name),
         sheet_name = as.factor(sheet_name)
  )
just.one.df$mean_repond_cert_vf <- ave(just.one.df$cert_val_funct, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))
just.one.df$mean_repond_cert_wt <- ave(just.one.df$cert_weight, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))
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
     respondant_colours,
     file = paste0(extraction.location,"curated.RData"))
