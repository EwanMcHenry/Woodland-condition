##------ Thu Jun 22 15:11:13 2023 ------##
# SCRIPT TO extract info from delphi forms 
# Ewan McHenry
# will save ind matcher df and expert opinion responses from the Delphi forms as objects within the specified extraction.location

                       
#|---------------|---------------

library(readxl)
library(tidyverse)
library(compiler)

# Functions ---------------------------------------------------------------

## respondant name
respondant.name <-  function(string = list.files(forms.direct)[1]){
  # Extracts the respondent's name from a given file name by identifying the text preceding a specific pattern (" - give your expert").
  pattern <- " - give your expert"
  string1 <- string
  # Extract the part of the string preceding the pattern
  str_extract(string1, paste0(".*(?=", pattern, ")"))
}

# cell that contains a defined pattern
matching_cell_index <- function(tibble_data, pattern) {
  # Finds the row and column indices of the first cell in a tibble or data frame that contains a value matching a specified pattern. 
  # Returns the indices or an error message if more than one match is found.
  
  matching_cells <- tibble_data %>%
    mutate(across(everything(), as.character)) %>%
    rowid_to_column() %>%
    
    pivot_longer(cols = -rowid, names_to = "column", values_to = "value") %>%
    
    filter(str_detect(value, pattern)) %>%
    mutate(column = as.integer(factor(column, levels = names(tibble_data)))) %>%
    select(rowid, column)
  
  if (length(matching_cells) == 2){
    return(matching_cells)
  }else{
    return("Error Mc: more than one matching cell") 
  }
}

# INITIAL CURATION + PREP -------------------------------------------------

## Create ind.matcher a helper df to match different name versions of the same indicator ----

all_sheets <- excel_sheets(paste0(forms.direct,list.files(forms.direct)[1]))
vf_sheets <- all_sheets[-length(all_sheets)] # the names of each excel sheet
source("Scripts\\Indicator matcher data.R") # loads data to match different versions of indicator names

#-----------------------------------------------------
#  EXTRACT DATA FROM EXCEL SHEETS ----------------------------------------------

extract.from.delphi.form <- function(forms.direct){
  ## create a list of lists to store all the info in ----
  expert.data <- list(expert_ID = list(NA),
                      indicator_num = list(NA),
                      respondant_name = list(NA),
                      indicator_name = list(NA),
                      descriptive_sentance = list(NA),
                      value.points = list(NA),
                      value_confidence_score = list(NA),
                      value_func_exceptions = list(NA),
                      weight_indicy_score = list(NA),
                      weight_indicy_confidence = list(NA),
                      weight_indicy_exceptions = list(NA))
  
  # for each expert extract the data for each indicator and save it all in the list of lists ----
  
  bk <- 1 # book keeping var
  for(expert_ID in 1:length(list.files(forms.direct))){
    for (indicator_num in 1:length(vf_sheets)){
      
      this.form.direct <- paste0(forms.direct,list.files(forms.direct)[expert_ID])
      
      expert.data$respondant_name[[bk]] <- respondant.name(string = list.files(forms.direct)[expert_ID])
      expert.data$expert_ID[[bk]] <- expert_ID
      
      ## value function info ----
      data_val_func <- read_excel(this.form.direct, sheet = indicator_num) # load data for this indicator:respondent
      
      expert.data$indicator_name_topof_sheet[[bk]] <- names(data_val_func)[1] # this is the top cell
      expert.data$indicator_num[[bk]] <- indicator_num
      
      descriptive_sentance.loc <- matching_cell_index(data_val_func, pattern = "Write a sentence or so describing") + c(1, 0)
      expert.data$descriptive_sentance[[bk]] <- data_val_func[descriptive_sentance.loc$rowid[1], descriptive_sentance.loc$column[1]] %>% as.character()
      
      indicator.measure.loc.start <-  matching_cell_index(data_val_func, pattern = "the table below to describe that typical relationship") + 
        c(4,0) 
      expert.data$value.points[[bk]] <- data.frame(
        measure = data_val_func[indicator.measure.loc.start$rowid[1]:nrow(data_val_func), indicator.measure.loc.start$column[1]] %>% unlist() %>% as.numeric(),
        value = data_val_func[indicator.measure.loc.start$rowid[1]:nrow(data_val_func), indicator.measure.loc.start$column[1] + 1]%>% unlist() %>% as.numeric()
      ) %>%
        na.omit() %>% arrange(measure)
      
      confidence.score.loc <- matching_cell_index(data_val_func, pattern = "Confidence score ") + c(0,1)
      expert.data$value_confidence_score[[bk]] <- data_val_func[confidence.score.loc$rowid[1], confidence.score.loc$column[1]]
      
      exceptions.loc.start <- matching_cell_index(data_val_func, pattern = "4: provide exceptions to") + c(3,0)
      expert.data$value_func_exceptions[[bk]] <- data_val_func[exceptions.loc.start$rowid[1]:nrow(data_val_func), exceptions.loc.start$column[1]] %>% na.omit() %>% unlist(use.names = F) 
      
      ## weight info ----
      data_weight  <- read_excel(this.form.direct, sheet = "Relative importance")
      
      weight_indicies_loc <- which(data_weight == 
                                     ind.matcher.df$weight.name[ind.matcher.df$indicator_name_topof_sheet == expert.data$indicator_name_topof_sheet[[bk]]],
                                   arr.ind = T) %>% as.numeric() + 
        c(0,1) 
      expert.data$weight_indicy_score[[bk]] <- data_weight[weight_indicies_loc[1], weight_indicies_loc[2]] %>% as.numeric()
      expert.data$weight_indicy_confidence[[bk]] <- data_weight[weight_indicies_loc[1], weight_indicies_loc[2]+1] %>% as.numeric()
      expert.data$weight_indicy_exceptions[[bk]] <- data_weight[weight_indicies_loc[1], (weight_indicies_loc[2]+2):ncol(data_weight)] %>% 
        unlist() %>% Filter(function(x)!all(is.na(x)), .) %>% as.character()
      
      expert.data$indicator_name[[bk]] <- ind.matcher.df$indicator_name_topof_sheet[indicator_num]
      
      bk <- bk +1
    }
  }
  
  return(expert.data)
  
}

extract.from.delphi.form <- cmpfun(extract.from.delphi.form)

expert.data <- extract.from.delphi.form(forms.direct = forms.direct)


#  --------------------------------------------------------------
# SAVE STUFF --------------------------------------------------------------
#  --------------------------------------------------------------

save(expert.data, ind.matcher.df, file = paste0(extraction.location,"sheets_data.RData"))

