# have a look at round 2 responses
# check which folk responded, changed some things etc
# make sure that the extraction all went well

source("Scripts\\functions_delphi_analysis.R") # also contains directorys for forms etc
library(tidyverse)


delphi.round <- 1
extraction.location <- paste0("Data\\Delphi round ", delphi.round,"\\")
load(paste0(extraction.location,"curated.RData"))

r1.df <- df
r1.completed_summary <- completed_summary
r1.ind.matcher.df <- ind.matcher.df
r1.has_completed <- has_completed
r1.just.one.df <- just.one.df


# round 2 
delphi.round <- 2
forms.direct <- paste0(extraction.location, "response sheets\\")
load(paste0(extraction.location,"curated.RData"))

r2.df <- df
r2.completed_summary <- completed_summary
r2.ind.matcher.df <- ind.matcher.df
r2.has_completed <- has_completed
r2.just.one.df <- just.one.df

str(df)
str(completed_summary)
str(ind.matcher.df)
str(has_completed)
str(just.one.df)
# table of respondents participation in each round

# assess which parts of the form were changed
