# have a look at round 2 responses
# check which folk responded, changed some things etc
# make sure that the extraction all went well

library(tidyverse)
source("Scripts\\functions_delphi_analysis.R") # also contains directorys for forms etc

r1_data <- prepare_round_data(1, "Data\\Delphi round 1\\", "response sheets\\", run_extraction = F)
r2_data <- prepare_round_data(2, "Data\\Delphi round 2\\", "response sheets\\", run_extraction = F)

# comparing responses between round by respondent ----
all.respondents <- unique(c(r1_data$completed_summary$respondent_names, r2_data$completed_summary$respondent_names))
#table of respondents by round
respondent_table <- tibble(
  respondent_names = all.respondents,
  round1 = as.integer(all.respondents %in% r1_data$completed_summary$respondent_names),
  round2 = as.integer(all.respondents %in% r2_data$completed_summary$respondent_names)
)

# view df for each respondent, 



# Combine rounds data ----

combine_rounds <- function(r1, r2, round1 = 1, round2 = 2) {
  bind_rows(
    mutate(r1, round = round1),
    mutate(r2, round = round2)
  )
}

c.df <- combine_rounds(r1_data$df, r2_data$df)
c.completed_summary <- combine_rounds(r1_data$completed_summary, r2_data$completed_summary)
c.ind.matcher.df <- combine_rounds(r1_data$ind_matcher_df, r2_data$ind_matcher_df)
c.has_completed <- combine_rounds(r1_data$has_completed, r2_data$has_completed)
c.just.one.df <- combine_rounds(r1_data$just_one_df, r2_data$just_one_df)

# Identify respondents with either only one round or differing values across rounds
inconsistent_respondents <- c.completed_summary %>%
  group_by(respondent_names) %>%
  summarize(
    rounds_participated = n_distinct(round),
    vf_diff = n_distinct(vf) > 1,
    vf_cert_diff = n_distinct(vf_cert) > 1,
    wt_diff = n_distinct(wt) > 1,
    wt_cert_diff = n_distinct(wt_cert) > 1,
    .groups = "drop"
  ) %>%
  filter(
    rounds_participated == 1 | vf_diff | vf_cert_diff | wt_diff | wt_cert_diff
  )
# View respondent names
inconsistent_respondents$respondent_names

c.completed_summary[c.completed_summary$respondent_names %in% inconsistent_respondents$respondent_names, ] %>% 
  arrange(respondent_names, round)


# round 1
delphi.round <- 1
config_for_delphi_round(delphi.round)
# source("Scripts\\Extract_expert_info.R")
# source("Scripts\\curation of extracted data.R")
load(paste0(extraction.location,"curated.RData"))

r1.df <- df
r1.completed_summary <- completed_summary
r1.ind.matcher.df <- ind.matcher.df
r1.has_completed <- has_completed
r1.just.one.df <- just.one.df

# round 2 
delphi.round <- 2
config_for_delphi_round(delphi.round)
# source("Scripts\\Extract_expert_info.R")
# source("Scripts\\curation of extracted data.R")
load(paste0(extraction.location,"curated.RData"))

r2.df <- df
r2.completed_summary <- completed_summary
r2.ind.matcher.df <- ind.matcher.df
r2.has_completed <- has_completed
r2.just.one.df <- just.one.df

# add round indicator and combine data

c.df <- rbind(data.frame(r1.df, round = 1), data.frame(r2.df, round = 2))
c.completed_summary <- rbind(data.frame(r1.completed_summary, round = 1), data.frame(r2.completed_summary, round = 2))
c.ind.matcher.df <- rbind(data.frame(r1.ind.matcher.df, round = 1), data.frame(r2.ind.matcher.df, round = 2))
c.has_completed <- rbind(data.frame(r1.has_completed, round = 1), data.frame(r2.has_completed, round = 2))
c.just.one.df <- rbind(data.frame(r1.just.one.df, round = 1), data.frame(r2.just.one.df, round = 2))

# exploring ----

## looking at change between rounds
# check if the same people responded in both rounds
identical(r1.has_completed, r2.has_completed)

# which dont change? no need to have separate data for each round
# check if r1.ind.matcher.df the same as r2.ind.matcher.df
identical(r1.ind.matcher.df, r2.ind.matcher.df)
r1.ind.matcher.df ==r2.ind.matcher.df # ind.axis.title has some inconsistencies
cbind(r1.ind.matcher.df$ind.axis.title, r2.ind.matcher.df$ind.axis.title) # they are the ones where the indicator was changed

#completed_summary
## select rows where respondents had different numbers in at least one of the cols c(vf vf_cert    wt wt_cert)
completed_summary[completed_summary$`Number of indicators` != completed_summary$`Number of indicators.1`,]

completed_summary[completed_summary$`Number of indicators` != completed_summary$`Number of indicators.1`,]

# check that the general structure of the data is the same between rounds
names(r1.df) == names(r2.df)
names(r1.completed_summary) == names(r2.completed_summary)
names(r1.has_completed) == names(r2.has_completed)
names(r1.just.one.df) == names(r2.just.one.df)

r1.df == r2.df
r1.completed_summary == r2.completed_summary


# add round indicator and combine data
r1.df$round <- 1
r2.df$round <- 2

df <- rbind(r1.df, r2.df)



str(df)
str(completed_summary)
str(ind.matcher.df)
str(has_completed)
str(just.one.df)



# table of respondents participation in each round

# assess which parts of the form were changed
