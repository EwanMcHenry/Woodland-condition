# have a look at round 2 responses
# check which folk responded, changed some things etc
# make sure that the extraction all went well

library(tidyverse)

# round 1
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


# which dont change? no need to have separate data for each round
# check if r1.ind.matcher.df the same as r2.ind.matcher.df
identical(r1.ind.matcher.df, r2.ind.matcher.df)

# check that the general structure of the data is the same between rounds
names(r1.df) == names(r2.df)
names(r1.completed_summary) == names(r2.completed_summary)
names(r1.has_completed) == names(r2.has_completed)
names(r1.just.one.df) == names(r2.just.one.df)

#

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
