# pull to examine
c.df1 %>% 
  filter(sheet_name == this.sheet & round == 2, respondant_name == "Sonia") %>%
  select(respondant_name, measure, value) %>%
  arrange(respondant_name, measure)

# find who had a measure == 0
zero_measures <- c.df1 %>%
  filter(sheet_name == this.sheet & round == 2 & measure == 0) %>%
  select(respondant_name, measure, value)
