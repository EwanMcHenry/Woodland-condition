
##------ Tue Jun 13 12:43:31 2023 ------##
# Ewan Mchenry

# processing Delphi results from expert opnions

# ---------------------------------------------------------------
# RUN SCRIPT TO EXTACT FROM EXCEL SHEETS ----------------------------------
# ---------------------------------------------------------------

forms.direct <- "Delphi round 1\\response sheets\\"
extraction.location <- "Delphi round 1\\"
# source("Extract_expert_info.R")
# if all is well this should:
# save ind.matcher ind.matcher.df and expert.data (expert opinion responses from the Delphi forms) 
# as files within the specified extraction.location to be loaded together
# then later I loads it (Italian-American mafiosi accent)

# LIBARIES ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(stringr)
library(plotly)
library(ggpubr)
library(mgcv)
library(RColorBrewer)
library(htmltools)
library(htmlwidgets)


#-----------------------------------------------------
#  LOAD DATA ----------------------------------------------

load(paste0(extraction.location,"sheets_data.RData"))

# ---------------------------------------------------------------
# ok, that was "fun", lets get to woooork!


#-----------------------------------------------------
# DATA CURATION --------------------------------------------------
#-----------------------------------------------------
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
) %>% merge(., ind.matcher.df, by.x = "indicator_name", by.y = "indicator_name", all.x = T) # this is wrong -- its sheet name

## scale and augment ----

df0 <- df %>% 
  group_by(indicator_name, respondant_name) %>% 
  mutate(value = value * (100 / max(value)) # scale by biggest value for each respondant 
        ) 
df0 <- df0 %>% 
  group_by(respondant_name) %>% 
  mutate(weight = weight * (100 / max(weight)) # scale by biggest value for each respondant 
  )

which.scaled.VF <- paste(df$respondant_name, df$indicator_name)[df0$value != df$value] %>% 
  sort() %>%   unique()
which.scaled.wt <- paste(df$respondant_name)[df0$weight != df$weight] %>% 
  sort() %>%   unique()

df$value <- df0$value %>% round(0)
df$weight <- df0$weight %>% round(0)

df$value.dec = df$value/100 # a decimal version of value to work in the later binomial gams

## version with sinlge entry for each indicator:respondant ----
just.one.df <- df[!duplicated(paste0(df$sheet_name, df$respondant_name)),
c("indicator_name", "respondant_name", "cert_val_funct", "cert_weight", "sheet_name", "weight")]

just.one.df$mean_repond_cert_vf <- ave(just.one.df$cert_val_funct, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))
just.one.df$mean_repond_cert_wt <- ave(just.one.df$cert_weight, just.one.df$respondant_name, FUN = function(x) mean(x, na.rm = TRUE))

# completed df
has_completed <- data.frame(respondent_names = respondent_names,
                            indicator_names = indicator_names,
                            wt = !is.na(weight),
                            wt.cert = !is.na(cert_weight),
                            vf = sapply(expert.data$value.points, nrow)>1,
                            vf.cert = !is.na(as.numeric(cert_val_funct))
                            )

#-----------------------------------------------------
# WHOS DONE IT? -----------------------------------------------------------
non_empty_in_list <- function(x){sapply(x, function(df) any(!is.na(unlist(df))))}

non_empty_val_funcs <- non_empty_in_list(expert.data$value.points)
non_empty_val_conf <- non_empty_in_list(expert.data$value_confidence_score)
non_empty_weights <- non_empty_in_list(expert.data$weight_indicy_score)
non_empty_weights_conf <- non_empty_in_list(expert.data$weight_indicy_confidence)

## whos value functions ---- 
completed_val_functions <- expert.data$respondant_name[non_empty_val_funcs] %>% unlist() %>% unique()
incompleted_val_functions <- expert.data$respondant_name[!non_empty_val_funcs] %>% unlist() %>% unique()
some_val_functions <- completed_val_functions[completed_val_functions %in% incompleted_val_functions]

completed_val_conf <- expert.data$respondant_name[non_empty_val_conf] %>% unlist() %>% unique()
incompleted_val_conf <- expert.data$respondant_name[!non_empty_val_conf] %>% unlist() %>% unique()
some_val_conf <- completed_val_conf[completed_val_conf %in% incompleted_val_conf]
# did all val funcs but left off some confidecne guesses
some_val_conf[!some_val_conf %in% some_val_functions]

## whos done weights ----
completed_weights <- expert.data$respondant_name[non_empty_val_funcs] %>% unlist() %>% unique() %>% sort()
incompleted_weights <- expert.data$respondant_name[!non_empty_val_funcs] %>% unlist() %>% unique() %>% sort()
some_weights <- completed_weights[completed_weights %in% incompleted_weights] %>% sort()

completed_weights_conf <- expert.data$respondant_name[non_empty_weights_conf] %>% unlist() %>% unique()
incompleted_weights_conf <- expert.data$respondant_name[!non_empty_weights_conf] %>% unlist() %>% unique()
some_weights_conf <- completed_weights_conf[completed_weights_conf %in% incompleted_weights_conf]
# did all val funcs but left off some confidecne guesses
some_weights_conf[!some_weights_conf %in% incompleted_weights_conf]

#-----------------------------------------------------
# INVESTIGATE CERTAINTIES --------------------------------------------------
#-----------------------------------------------------
## by indicators ----
indicator_certainty <- aggregate(cert_val_funct ~ indicator_name, data = just.one.df,  simplify = T,
                                 FUN = function(x) c(vf_mean_cert = mean(x, na.rm = TRUE),
                                                     vf_sd_cert = sd(x, na.rm = TRUE) ) ) %>% 
  full_join(., aggregate(cert_weight ~ indicator_name, data = just.one.df,  FUN = function(x) c(wt_mean_cert = mean(x, na.rm = TRUE), 
                                                                                                 wt_sd_cert = sd(x, na.rm = TRUE) ) ),
             by = "indicator_name") 
indicator_certainty <- data.frame(
  indicator_name = indicator_certainty$indicator_name,
  vf_mean_cert = indicator_certainty$cert_val_funct[, "vf_mean_cert"],
  vf_sd_cert = indicator_certainty$cert_val_funct[, "vf_sd_cert"],
  wt_mean_cert = indicator_certainty$cert_weight[, "wt_mean_cert"],
  wt_sd_cert = indicator_certainty$cert_weight[, "wt_sd_cert"]
) %>% 
  full_join(., ind.matcher.df ,
             by = "indicator_name") 

### plot ----
#### vf
plot <- ggplot() +
  geom_point(data = just.one.df, aes(x = sheet_name, y = cert_val_funct, color = respondant_name,
                                     text = map(
                                       paste0(respondant_name), 
                                       HTML)),
             size = 1, alpha = 0.5, type = 16,
             position = position_jitter(width = 0.2, height = 0),
  ) +
  geom_point(data = indicator_certainty, aes(x = sheet_name, y = vf_mean_cert),
             size = 2) +
  geom_errorbar(data = indicator_certainty, aes(x = sheet_name, ymin = vf_mean_cert - vf_sd_cert, 
                                                ymax = vf_mean_cert + vf_sd_cert),
                width = 0.2, color = "black") +
  labs(x = NULL, y = "Certainty score", title = "Certainty: value functions",
       subtitle = "Black dots and line show mean +- sd") +
  ylim(0, 5.5) +
  theme_pubr()+
  theme(axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
        plot.margin = margin(0.5, 1.5, 0.5, 1, "cm"),
        plot.subtitle = element_text(size = 10))
ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
  config(displayModeBar = F) 


#### weights
plot <- ggplot() +
  geom_point(data = just.one.df, aes(x = sheet_name, y = cert_weight, color = respondant_name,
                                     text = map(
                                       paste0(respondant_name), 
                                       HTML)),
             size = 1, alpha = 0.5, type = 16,
             position = position_jitter(width = 0.2, height = 0),
  ) +
  geom_point(data = indicator_certainty, aes(x = sheet_name, y = wt_mean_cert),
             size = 2) +
  geom_errorbar(data = indicator_certainty, aes(x = sheet_name, ymin = wt_mean_cert - wt_sd_cert, 
                                                ymax = wt_mean_cert + wt_sd_cert),
                width = 0.2, color = "black") +
  labs(x = NULL, y = "Certainty score", title = "Certainty: weights",
       subtitle = "Black dots and line show mean +- sd") +
  ylim(0, 5.5) +
  theme_pubr()+
  theme(axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
        plot.margin = margin(0.5, 1.5, 0.5, 1, "cm"),
        plot.subtitle = element_text(size = 10))
ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
  config(displayModeBar = F) 

## by respondent ----

respondant_certainty <- aggregate(cert_val_funct ~ respondant_name, data = just.one.df,  FUN = function(x) c(vf_mean_cert = mean(x, na.rm = TRUE), 
                                                                                                             vf_sd_cert = sd(x, na.rm = TRUE) ) )%>% 
  full_join(., aggregate(cert_weight ~ respondant_name, data = just.one.df,  
                          FUN = function(x) c(wt_mean_cert = mean(x, na.rm = TRUE),
                                              wt_sd_cert = sd(x, na.rm = TRUE) ) ),
             by = "respondant_name")

respondant_certainty <- data.frame(
  respondant_name = respondant_certainty$respondant_name,
  vf_mean_cert = respondant_certainty$cert_val_funct[, "vf_mean_cert"],
  vf_sd_cert = respondant_certainty$cert_val_funct[, "vf_sd_cert"],
  wt_mean_cert = respondant_certainty$cert_weight[, "wt_mean_cert"],
  wt_sd_cert = respondant_certainty$cert_weight[, "wt_sd_cert"]
) 

### plot ----
#### vf
plot <- ggplot() +
  geom_point(data = just.one.df, aes(x = respondant_name, y = cert_val_funct,
                                     text = map(
                                       paste0(sheet_name), 
                                       HTML)),
             size = 1, alpha = 0.5, type = 16,
             position = position_jitter(width = 0.2, height = 0),
  ) +
  geom_point(data = respondant_certainty, aes(x = respondant_name, y = vf_mean_cert),
             size = 2) +
  geom_errorbar(data = respondant_certainty, aes(x = respondant_name, ymin = vf_mean_cert - vf_sd_cert, 
                                                ymax = vf_mean_cert + vf_sd_cert),
                width = 0.2, color = "black") +
  labs(x = NULL, y = "Certainty score", title = "Certainty: value functions",
       subtitle = "Black dots and line show mean +- sd") +
  ylim(0, 5.5) +
  theme_pubr()+
  theme(axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
        plot.margin = margin(0.5, 1.5, 0.5, 1, "cm"),
        plot.subtitle = element_text(size = 10))
ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
  config(displayModeBar = F) 

#### wt
plot <- ggplot() +
  geom_point(data = just.one.df, aes(x = respondant_name, y = cert_weight,
                                     text = map(
                                       paste0(sheet_name), 
                                       HTML)),
             size = 1, alpha = 0.5, type = 16,
             position = position_jitter(width = 0.2, height = 0),
  ) +
  geom_point(data = respondant_certainty, aes(x = respondant_name, y = wt_mean_cert),
             size = 2) +
  geom_errorbar(data = respondant_certainty, aes(x = respondant_name, ymin = wt_mean_cert - wt_sd_cert, 
                                                ymax = wt_mean_cert + wt_sd_cert),
                width = 0.2, color = "black") +
  labs(x = NULL, y = "Certainty score", title = "Certainty: weights",
       subtitle = "Black dots and line show mean +- sd") +
  ylim(0, 5.5) +
  theme_pubr()+
  theme(axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
        plot.margin = margin(0.5, 1.5, 0.5, 1, "cm"),
        plot.subtitle = element_text(size = 10))
ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
  config(displayModeBar = F) 


#-----------------------------------------------------
# PLOT VALUE FUNCTIONS --------------------------------------------------
#-----------------------------------------------------
gam_model <- list(NA)
dummy_data <- list(NA)
each.plotly <- list(NA)
each.plotly.no.model <- list(NA)
should.model <- list(NA)

for(i in 1: length(ind.matcher.df$indicator_name)){
  # Choose an indicator name
  indicator_name <- ind.matcher.df$indicator_name[i]
  # Filter the data for the selected indicator
  filtered_data <- df[df$indicator_name == indicator_name, ] %>% 
    mutate(respondant_name = as.factor(respondant_name))
  # Color palette
  respondant_colours <- brewer.pal(nlevels(as.factor(filtered_data$respondant_name)), "Paired")
  
  # prediction of mean trend ----
  filtered_data$point.influence <- 1/(table(filtered_data$respondant_name)[as.factor(filtered_data$respondant_name)]) %>% 
    as.numeric()
  gam_model[[i]] <- gam(value.dec ~ s(measure, k = 4),
                        data = filtered_data, family = binomial(), weights = point.influence)
  # Create dummy data for prediction
  dummy_data[[i]] <- data.frame(measure = seq(min(filtered_data$measure), max(filtered_data$measure), length.out = 50))
  # Predict using the GAM model
  dummy_data[[i]]$predicted_value <- predict(gam_model[[i]], newdata = dummy_data[[i]], type = "response")*100
  
  # Plot of lines and mean predictions
  plot <- ggplot() +
    geom_line(data = dummy_data[[i]], size = 2, aes(y = predicted_value, x = measure), colour = "black") +
    geom_line(data = filtered_data, 
              aes(x = measure, y = value, color = respondant_name,
                  text = map(
                    paste0("<b>", `respondant_name`, "</b>",
                           "<br><b>Certainty:</b>", cert_val_funct, "<br>",
                           "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                    HTML),
                  alpha = cert_val_funct),
              size = 0.5) +
    geom_point(data = filtered_data, 
               size = 2, shape = 16,
               aes(x = measure, y = value, color = respondant_name,
                   text = map(
                     paste0("<b>", `respondant_name`, "</b>",
                            "<br><b>Certainty:</b>", cert_val_funct, "<br>",
                            "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                     HTML))) +
    scale_colour_manual(values = respondant_colours) +
    labs(title = indicator_name, x = ind.matcher.df$ind.axis.title[i], y = "Value Score",
         colour = "Respondant") +
    guides(alpha = F) +
    theme_pubr()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 12 , face = "bold"),
          legend.text =  element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title =  element_text(size = 12),
          legend.position = "right",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm")
    ) 
  each.plotly[[i]] <- ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
    config(displayModeBar = F) %>% 
    layout(yaxis = list(range = c(-5, 105)))
  
  
  # Plot of lines and mean predictions
  plot <- ggplot() +
    # geom_line(data = dummy_data[[i]], size = 2, aes(y = predicted_value, x = measure), colour = "black") +
    geom_line(data = filtered_data, 
              aes(x = measure, y = value, color = respondant_name,
                  text = map(
                    paste0("<b>", `respondant_name`, "</b>",
                           "<br><b>Certainty:</b>", cert_val_funct, "<br>",
                           "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                    HTML),
                  alpha = cert_val_funct),
              size = 0.5) +
    geom_point(data = filtered_data, 
               size = 2, shape = 16,
               aes(x = measure, y = value, color = respondant_name,
                   text = map(
                     paste0("<b>", `respondant_name`, "</b>",
                            "<br><b>Certainty:</b>", cert_val_funct, "<br>",
                            "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                     HTML))) +
    scale_colour_manual(values = respondant_colours) +
    labs(title = indicator_name, x = ind.matcher.df$ind.axis.title[i], y = "Value Score",
         colour = "Respondant") +
    guides(alpha = F) +
    theme_pubr()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 12 , face = "bold"),
          legend.text =  element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title =  element_text(size = 12),
          legend.position = "right",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm")
    ) 
  each.plotly.no.model[[i]] <- ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
    config(displayModeBar = F) %>% 
    layout(yaxis = list(range = c(-5, 105)))
  
  
  
  
}

#-----------------------------------------------------
# PLOT WEIGHTS --------------------------------------------------
#-----------------------------------------------------

plot <- ggplot() +
  geom_boxplot(data = just.one.df, aes(x = sheet_name, y = weight), 
               width=0.3, color="grey70", fill = "white", alpha=0.2) +
  geom_point(data = just.one.df, 
             position = position_jitter(width = 0.2, height = 0),
             size = 1, shape = 16,
             aes(x = sheet_name, y = weight, color = respondant_name,
                 text = map(
                   paste0("<b>", `respondant_name`, "</b><br>",
                          "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                   HTML))) +
  stat_summary(data = just.one.df, aes(x = sheet_name, y = weight,
                                       text = map(
                                         paste0("Mean weight"),
                                         HTML)),
               geom = "point", fun = mean,
               color = "red", shape = 3, size = 2,
               position = position_nudge(x = 0.05, y = 0)
               ) +
  labs(x = NULL, y = "Relative importance (weight)", 
       title = "Indicator weights",
       color = "Respondant") +
  theme_pubr()+
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 12 , face = "bold"),
        legend.text =  element_text(size = 12),
        axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(0.5, 1, 0.5, 1, "cm")
  ) 
ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
  config(displayModeBar = F) %>% 
  layout(yaxis = list(range = c(-5, 105)))
#-----------------------------------------------------
# FUNCTION TO CALCULATE CONDITION SCORE ------
#-----------------------------------------------------
indicator.measures <- data.frame(matrix(nrow = 0, ncol = length(ind.matcher.df$indicator_name)))
colnames(indicator.measures) <- ind.matcher.df$indicator_name

max_measures <- df %>% 
  group_by(indicator_name) %>% 
  summarise(max_measure = max(measure))


#-----------------------------------------------------
# SIMULATE SOME WOODLANDS AND GIVE THEM A CONDITION SCORE ------
#-----------------------------------------------------

