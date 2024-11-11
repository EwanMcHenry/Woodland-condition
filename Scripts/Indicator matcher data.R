# indicator name matcher df 

ind.matcher.df <- data.frame(sheet_name = vf_sheets) # the names of each excel sheet
### whats written at the top of each sheet ----
ind.matcher.df$indicator_name <- c("Number of tree size classes (age proxy)",
                                   "Proportion of the tree and shrub canopy that is native",
                                   "Vertical structure: Number of tree and shrub canopy layers present",
                                   "Number of native tree and shrub species",
                                   "Invasive plant species presence and cover",
                                   "Number deadwood classes present (of 3 potential classes)  across 4 plot-quarters",
                                   "Number of ancient/veteran trees per ha",
                                   "Area of woodland surrounding the plot",
                                   "Number of regeneration classes present",
                                   "Amount of potential annual growth removed",
                                   "Presence of tree disease and rapid mortality",
                                   "Number of 'positive indicator' plants per plot (10m radius circle)",
                                   "Number of top-height classes present",
                                   "Cover of anthropogenic damage")
### whats written for that indicator in teh weights sheet ----
ind.matcher.df$weight.name <- c("Tree Age distribution" ,"Canopy nativeness (all layers)" ,"Vertical structure" ,
                                "Native tree and shrub species number" , "Invasive plants" ,
                                "Deadwood", "Veteran trees",  "Woodland extent", "Tree regeneration", 
                                "Herbivore impact", "Tree health", "Ground flora" , "Horizontal complexity", 
                                "Anthropogenic damage")
# whats needed to match in the extractio nprocess to find locations of things in excel sheets
ind.matcher.df$indicator_name_topof_sheet <- ind.matcher.df$indicator_name
ind.matcher.df$indicator_name_topof_sheet[4] <- "Number of native tree and shrub species"
ind.matcher.df$indicator_name_topof_sheet[8] <- "Extent/ area of woodland"
ind.matcher.df$indicator_name_topof_sheet[9] <- "Tree regeneration"
ind.matcher.df$indicator_name_topof_sheet[10] <- "Herbivore impact"
ind.matcher.df$indicator_name_topof_sheet[11] <- "Tree disease and rapid mortality"
ind.matcher.df$indicator_name_topof_sheet[13] <- "Horizontal complexity (structural mosaics across a wood)"
ind.matcher.df$indicator_name_topof_sheet[14] <- "Anthropogenic damage"

### whats written on the axis title for that indicator's sheet.... wow, this is exhausting! ----
ind.matcher.df$ind.axis.title <- NA
for (indicator_num in 1:length(vf_sheets)){
  ## indicator axis.title
  data_val_func <- read_excel(paste0(forms.direct,list.files(forms.direct)[1]), sheet = indicator_num) # load data for this indicator:respondent
  
  axis.title.loc <-  matching_cell_index(data_val_func, 
                                         pattern = "the table below to describe that typical relationship") + 
    c(3,0) 
  ind.matcher.df$ind.axis.title[indicator_num] <- data_val_func[axis.title.loc$rowid[1], axis.title.loc$column[1]] %>% as.character()
}
