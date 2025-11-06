# set up survey extract function

site_name = "Harrisons Wood",
zone_name = "Z001",
site_survey_folderpath = "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z001\\",
site_survey_filename = "WECA - Harrisons Woodland Z001 BNG - Field test - 2025.xlsx",
site_survey_habitat_type = "base_rich_lowland"


excel_sheet_folderpath =   "Data\\Field Test\\Harrisons Woodland, Louth, Lincs\\Z001\\"
excel_sheet_filename = "WECA - Harrisons Woodland Z001 BNG - Field test - 2025.xlsx"

# beechy
excel_sheet_folderpath =   "Data\\Field Test\\Beechy Wood, Sussex - Corsican Pine PAWS\\Z01\\"
excel_sheet_filename = "WCA - Beechy Wood - Field test 22.05.25.xlsx"
site_survey_habitat_type = "base_rich_lowland"

# ladypark
excel_sheet_folderpath =   "Data\\Field Test\\Ladypark Wood, Wye valley - Non-intervention 70yrs broadleaf SSSI ASNW\\Z01\\"
excel_sheet_filename = "WCA - Lady park wood - Field test - 12.06.2025.xlsx"
habitat_type = "base_rich_lowland"

# Inverlever
site_name = "Inverliever, Argyll"
zone_name = "1"
site_survey_folderpath = "Data\\Field Test\\Inverliever, Argyll - Conifer clearfell FLS\\Z01\\"
site_survey_filename = "WCA - Inverliever - Field test 06.06.25.xlsx"
site_survey_habitat_type = "acidic_upland"

params$site_name = "Inverliever, Argyll"
params$zone_name = "1"
params$site_survey_folderpath = "Data\\Field Test\\Inverliever, Argyll - Conifer clearfell FLS\\Z01\\"
params$site_survey_filename = "WCA - Inverliever - Field test 06.06.25.xlsx"
params$site_survey_habitat_type = "acidic_upland"

site_name: "Inverliever, Argyll"
zone_name: "1"
site_survey_folderpath: "Data\\Field Test\\Inverliever, Argyll - Conifer clearfell FLS\\Z01\\"
site_survey_filename: "WCA - Inverliever - Field test 06.06.25.xlsx"
site_survey_habitat_type: "acidic_upland"




write_plot.csv = F
verticle_dont_contribute = c("Absent", "< 4%", "4 - 10%")
n.deadwood_types = 4 # total number of deadwood categories - hard coded
last.micohabitat.listed = "Heavy resinosis" # last microhabitat listed in the sheet, used to find the last row of microhabitats
sheet_appropriate_ground_flora = read.csv("Data\\Appropriate_groundflora_spp_list.csv") 
sheet_appropriate_tree_spp = read.csv("Data\\Appropriate_tree_spp_list.csv") 
list_high_threat_invasives = read.csv("Data\\high_threat_invasives_list.csv")$species # list of high threat invasive species, used to check if any are present in the plot data
herbivory_category_names = c("Low damage", "Moderate damage", "High damage", "Very high damage", "Extreme damage") # names of the herbivory categories
domin.absent = "0% Absent" # value of absent domin, used to replace NAs in tree species counts
avt_search_radius_m = 10
transect_length = 100









# pull to examine
c.df1 %>% 
  filter(sheet_name == this.sheet & round == 2, respondant_name == "Dean K") %>%
  select(respondant_name, measure, value) %>%
  arrange(respondant_name, measure)

c.df %>% filter(sheet_name == this.sheet & 
                  round == this.round) %>% 
  select(respondant_name, measure, value, value.dec) %>%
  arrange(respondant_name, measure)


# which have na value
c.df %>% 
#  filter(sheet_name == this.sheet & round == this.round) %>% 
  select(respondant_name, measure, value, value.dec) %>%
  filter(is.na(value)) %>%
  arrange(respondant_name, measure)



which(is.na(c.df1$value))


# find who had a measure == 0
zero_measures <- c.df1 %>%
  filter(sheet_name == this.sheet & round == 2 & measure == 0) %>%
  select(respondant_name, measure, value)

# print data from respondents with issues
respondents_with_issues <- c("Martin Hu", "Bob Epsom", "Dean K", "Kylie Jo-Ma", 
                            "Nick RB", "Sonia", "Mick Br", "Lou Ha", "Peter Lo")
for (respondent in respondents_with_issues) {
  print(c.df1 %>% 
          filter(sheet_name == this.sheet & round == 2, respondant_name == respondent) %>%
          select(respondant_name, measure, value) %>%
          arrange(respondant_name, measure)
  )
}
[1] "Martin Hu does not cover full range of 0 - 5 Horizontal complexity"
[1] "Bob Epsom does not cover full range of 0 - 5 Horizontal complexity"
[1] "Dean K does not cover full range of 0 - 5 Horizontal complexity"
[1] "Kylie Jo-Ma does not cover full range of 0 - 5 Horizontal complexity"
[1] "Nick RB does not cover full range of 0 - 5 Horizontal complexity"
[1] "Sonia does not cover full range of 0 - 5 Horizontal complexity"
[1] "Mick Br does not cover full range of 0 - 5 Horizontal complexity"
[1] "Lou Ha does not cover full range of 0 - 5 Horizontal complexity"
[1] "Peter Lo does not cover full range of 0 - 5 Horizontal complexity"
[1] "All respondents have value functions within 0-100"

this.sheet <- "Deadwood"
ind_check <- ind_checker(c.df1)
call_ind_mess(ind_check)

df = c.df1
respondent =  "Jim Sm-Wr"
sheet_name =  "Vertical structure"
round = 2


this.sheet <- "Deadwood"
ind_check <- ind_checker(df)
                         
call_ind_mess(ind_check)




```{r interpolation_of_vfs, warning=FALSE}


#create interpolated.data list structure: ----
#sheet_name -> round -> respondent ->  df( measurement and interpolated values), certainties and weights

interpolated.data <- list()
for (i in unique(c.df$sheet_name)) { # for each indicator
  interpolated.data[[i]] <- list()
  for (r in unique(c.df$round)) { # for each round
    interpolated.data[[i]][[r]] <- list()
    for (resp in unique(c.df$respondant_name)) { # for each respondent
      explanatory <- c.df$measure[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp]
      response <- c.df$value[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp]
      
      
      # add value_function - if theres more than 1 non-NA value and it actually changes
      if (sum(!is.na(response))>1 && length(response) > 1 && diff(range(response, na.rm = TRUE)) > 0) {
        interpolated.data[[i]][[r]][[resp]] <- list(
          value_function = data.frame("measurement" = interpolation_measurements[[i]]),
          og_vf = data.frame("measurement" = explanatory, "value" = response),
          weight = c.df$weight[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1],
          vf_cert = c.df$cert_val_funct[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1],
          wt_cert = c.df$cert_weight[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1],
          round = r,
          respondent = resp,
          indicator = i
          
        )
      } else {
        interpolated.data[[i]][[r]][[resp]] <- list(
          weight = c.df$weight[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1],
          vf_cert = c.df$cert_val_funct[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1],
          wt_cert = c.df$cert_weight[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp][1]
        )
      }
      
      
    } # resp
  }# round
} # indicator


# interpolation of vfs ----

# use data from corresponding round, respondent and indicator to interpolate values

# i <- unique(c.df$sheet_name)[1]
# r <- unique(c.df$round)[1]
# resp <- unique(c.df$respondant_name)[1]
# i <- "Horizontal complexity"
# r <- 1
# resp <- "Martin Hu"


bk2 = 1
interpolation.warnings <- NA
for (i in unique(c.df$sheet_name)) { # for each indicator
  for (r in unique(c.df$round)) { # for each round
    for (resp in unique(c.df$respondant_name)) { # for each respondent
      ## if value_function exists
      if (!is.null(interpolated.data[[i]][[r]][[resp]]$value_function)) {
        
        ## Prepare data for modeling ----
        ### save dummy explanatory for interpolation, recorded explanatory and recorded response data
        meas <- interpolated.data[[i]][[r]][[resp]]$value_function$measurement
        response <- c.df$value[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp]
        ### Create a data frame for modeling
        data <- data.frame(explanatory = c.df$measure[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp], 
                           response_scaled = (response - min(response)) / (max(response) - min(response)))
        ### delete duplicated rows
        data <- data[!duplicated(data), ]
        
        ## check response range is 0 100
        if(!identical(range(response), c(0,100))) {
          print(paste("Warning: response range for", i, r, resp, "is" , range(response)))}
        
        ## Fit models ----
        # ### glm - linear ----
        # linear.mod <- glm(response_scaled ~ explanatory, data = data)
        # 
        # ### glm binomial ----
        # binom.glm <- glm(
        #   response_scaled ~ explanatory,
        #   family = binomial(link = "logit"),
        #   data = data
        # )
        
        ### GAM - linear ----
        gam <- gam(response_scaled ~ s(explanatory, bs = "cs", k = length(data$explanatory)),
                   data = data,
                   method = "REML"
        )
        ### GAM - binomial ----
        binom.gam <- gam(response_scaled ~ s(explanatory, bs = "cs", k = length(data$explanatory)),
                         family = binomial(link = "logit"),
                         data = data,
                         method = "REML"
        )
        ### GAM - binomial with offset ----
        # binom.spline_model <- gam(
        #   response_scaled ~ s(explanatory, bs = "cs", k = length(data$explanatory)),
        #   family = binomial(link = "logit"),
        #   data = data,
        #   method = "REML",
        #   offset = log(response_scaled)
        # )
        
        
        
        # # exponential
        # data$response_offset <- pmax(data$response_scaled, 1e-5)  # avoid log(0)
        #       ## Exponential Model
        # data$response_offset <- pmax(data$response_scaled, 1e-5)  # avoid log(0)
        # glm_exp <- glm(log(response_offset) ~ explanatory, data = data)
        # 
        # 
        # 
        # AIC(gam, binom.gam, glm_exp) # compare AIC of models
        
        
        # data$response_offset <- pmax(data$response_scaled, 1e-5)  # avoid log(0)
        # glm_exp <- glm(log(response_offset) ~ explanatory, data = data)
        
        
        # ## Beta GAM
        # beta.spline_model <- gam(
        #   response_scaled ~ s(explanatory, bs = "cs", k = length(explanatory) - 1),
        #   family = betar(link = "logit"),
        #   data = data,
        #   method = "REML"
        # )
        
        # 
        # 
        # ## Logarithmic Model
        # data$log_explanatory <- log(data$explanatory+0.00001) # Log-transform explanatory
        # log.mod <- glm(response_scaled ~ log_explanatory, data = data)
        # 
        # ## Negative Logarithmic Model
        # neglog.mod <- glm(response_scaled ~ -log_explanatory, data = data)
        
        # # Compare models using AIC
        model_aics <- AIC(
          # linear.mod,
          # binom.glm,
          gam,
          binom.gam #,
          # glm_exp,
          # log.mod,
          # neglog.mod
        )
        
        # Predict over the interpolation measurements using the best model
        best_model <- which.min(model_aics$AIC)
        meas_transformed <- data.frame(
          explanatory = meas,
          log_explanatory = log(meas),
          negexp_predictor = 1 - exp(-meas)
        )
        
        if (best_model == 1) {
          predictions_scaled <- predict(gam, newdata = meas_transformed, type = "response")
          #   predictions_scaled <- predict(linear.mod, newdata = meas_transformed)
        } else if (best_model == 2) {
          predictions_scaled <- predict(binom.gam, newdata = meas_transformed, type = "response")
        }
        #   predictions_scaled <- predict(binom.spline_model, newdata = meas_transformed, type = "response")
        # } else if (best_model == 3) {
        # } else if (best_model == 4) {
        #   predictions_scaled <- exp(predict(exp.mod, newdata = meas_transformed)) # Back-transform predictions
        # } else if (best_model == 5) {
        #   predictions_scaled <- predict(negexp.mod, newdata = meas_transformed)
        # } else if (best_model == 6) {
        #   predictions_scaled <- predict(log.mod, newdata = meas_transformed)
        # } else if (best_model == 7) {
        #   predictions_scaled <- predict(neglog.mod, newdata = meas_transformed)
        # } else {
        #   predictions_scaled <- predict(binom.mod, newdata = meas_transformed, type = "response")
        # }
        
        # fix to 0-100 bounds
        predictions_fixed <- (predictions_scaled - min(predictions_scaled))*(100/(max(predictions_scaled) - min(predictions_scaled)))
        
        
        if(!identical(range(predictions_fixed), c(0,100))) {
          interpolation.warnings[bk2] <- paste("Warning: INTERPOLATED response range", range(predictions_fixed)[1], range(predictions_fixed)[2] ,", not 0-100 for", i, r, resp)
          bk2 = bk2+1
        }
        
        # add to interpolated.data
        interpolated.data[[i]][[r]][[resp]]$value_function$value <- predictions_fixed
      }
    }
  }
}

```

```{r bootstrap_predictions}

set.seed(5448)
boot_results <- list(indicator = list(NA))

boot_results <- list()

for (i in unique(c.df$sheet_name)) {
  boot_results[[i]] <- list()  
  for (r in unique(c.df$round)) {
    if(paste0(i,r) != "Herbivore damage1" ){
      resp_list <- NA
      bk = 1
      for (resp.n in 1:length(names(interpolated.data[[i]][[r]]))){
        response <- c.df$value[c.df$sheet_name == i & c.df$round == r & c.df$respondant_name == resp]
        if(length(response)>2){
          resp_list[bk] <- names(interpolated.data[[i]][[r]])[resp.n]
          bk = bk + 1
        }
      }
      
      boot_expl <- interpolated.data[[i]][[r]][[resp_list[1]]]$value_function$measurement
      
      # sample by respondent
      boot_resps <- sample(resp_list, 1000, replace = TRUE)
      boot_res_pre = data.frame(expl = numeric(length(boot_expl)),
                                median = numeric(length(boot_expl)),
                                mean = numeric(length(boot_expl)),
                                upper95 = numeric(length(boot_expl)),
                                lower95 = numeric(length(boot_expl)),
                                upperq = numeric(length(boot_expl)),
                                lowerq = numeric(length(boot_expl)))
      for(e in 1:length(boot_expl)){
        y = map(boot_resps, ~ interpolated.data[[i]][[r]][[.x]]$value_function$value[e]) %>%
          flatten_dbl()
        
        boot_res_pre$expl[e] <- boot_expl[e]
        boot_res_pre$median[e] <- median(y)
        boot_res_pre$mean[e] <- mean(y)
        boot_res_pre$upper95[e] <- quantile(y, 0.975)
        boot_res_pre$lower95[e] <- quantile(y, 0.025)
        boot_res_pre$lowerq[e] <- quantile(y, 0.25)
        boot_res_pre$upperq[e] <- quantile(y, 0.75)
        
      }
      
      boot_results[[i]][[r]] <- list(indicator = i,             # Indicator name
                                     round = r,                 # Round number
                                     respondents = resp_list, # Vector of respondent names
                                     boot_response = data.frame( # Data frame with explanatory and response data
                                       expl = boot_res_pre$expl,
                                       median = boot_res_pre$median,
                                       mean = boot_res_pre$mean,
                                       upper95 = boot_res_pre$upper95,
                                       lower95 = boot_res_pre$lower95,
                                       lowerq = boot_res_pre$lowerq,
                                       upperq = boot_res_pre$upperq
                                     )
      )
    }
  }
}




```

```{r f_vf_plot_function}
plot_indicator_rounds <- function(
    c.df = c.df,
    boot_results, 
    interpolated_data, 
    respondent_colors, 
    ind.matcher.df, 
    output_dir = "plots/vfs-av_and_individual/", 
    include_respondent_lines = TRUE, 
    include_legend = TRUE
) {
  # Generate a dummy data frame and legend if required
  if (include_legend) {
    unique_respondents <- unique(unlist(lapply(interpolated_data, function(ind) {
      unlist(lapply(ind, names))
    })))
    
    dummy_resps <- data.frame(
      resp = rep(unique_respondents, each = 2),
      x = rep(0:1, times = length(unique_respondents)),
      y = rep(0:1, times = length(unique_respondents))
    )
    
    legend <- get_legend(
      ggplot() +
        geom_line(data = dummy_resps, aes(x = x, y = y, color = resp)) +
        scale_colour_manual(values = respondent_colors,
                            name = NULL,
                            guide = guide_legend(ncol = 10)) +
        theme_pubr() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 8))
    )
  }
  
  # Iterate over each indicator
  for (indicator in names(boot_results)) {
    round_plots <- list()
    n.respondents <- NA
    for (round in seq_along(boot_results[[indicator]])) {
      round_data <- boot_results[[indicator]][[round]]
      n.respondents[round] <- length(c.df$respondant_name[c.df$sheet_name==indicator & c.df$round == round] %>% unique())
      
      
      if (is.null(round_data$boot_response) || 
          round_data$indicator == "Herbivore damage" && round == 1) {
        plot <- ggplot() +
          theme_void() +
          ggtitle(paste(indicator, "Round", round, "(No Data)"))
      } else {
        plot_data <- round_data$boot_response
        respondent_data <- data.frame(measurement = NA, value = NA, resp = NA)[0, ]
        
        if (include_respondent_lines) {
          resps <- names(interpolated_data[[indicator]][[round]])
          for (resp in resps) {
            if (!is.null(interpolated_data[[indicator]][[round]][[resp]]$value_function)) {
              respondent_data <- rbind(respondent_data,
                                       data.frame(
                                         measurement = interpolated_data[[indicator]][[round]][[resp]]$value_function$measurement,
                                         value = interpolated_data[[indicator]][[round]][[resp]]$value_function$value,
                                         resp = resp
                                       ))
            }
          }
        }
        
        plot <- ggplot(plot_data, aes(x = expl, y = median)) +
          geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, fill = "grey63") +
          geom_ribbon(aes(ymin = lowerq, ymax = upperq), alpha = 0.2, fill = "grey13") +
          {if (include_respondent_lines) geom_line(data = respondent_data, 
                                                   aes(x = measurement, y = value, color = resp),
                                                   alpha = 0.3, linetype = "longdash")} +
          geom_line(color = "black", linewidth = 1) +
          scale_color_manual(values = respondent_colors) +
          labs(
            title = paste("Round", round, "N =", n.respondents[round]),
            y = "Value"
          ) +
          theme_pubr() +
          theme(axis.title.x = element_blank(), legend.position = "none")
      }
      
      round_plots[[round]] <- plot
    }
    
    grid_plots <- plot_grid(
      plotlist = round_plots,
      ncol = 2,
      labels = NULL
    )
    
    shared_title <- ggdraw() +
      draw_label(
        label = paste(indicator),
        fontface = "bold",
        size = 16,
        hjust = 0.5
      )
    
    shared_x_axis <- ggdraw() +
      draw_label(
        label = ind.matcher.df$ind.axis.title[ind.matcher.df$sheet_name == indicator],
        fontface = "bold",
        size = 12,
        hjust = 0.5
      )
    
    components <- list(shared_title, grid_plots, shared_x_axis)
    
    if (include_legend) {
      components <- c(components, list(legend))
      rel_heights <- c(1, 10, 1, 4)
    } else {
      rel_heights <- c(1, 10, 1)
    }
    
    final_plot <- plot_grid(
      plotlist = components,
      ncol = 1,
      rel_heights = rel_heights
    )
    
    ggsave(
      filename = file.path(output_dir, paste0(indicator, "_round_plots.png")),
      plot = final_plot,
      width = 10, height = if (include_legend) 7 else 6
    )
    
    print(final_plot)
  }
}

```
```{r respondent_colours}
respondent_colors <-  set_resp_col_fun(unique(c.df$respondant_name))
```
```{r plots_boot}

plot_indicator_rounds(
  c.df = c.df,
  boot_results = boot_results,
  interpolated_data = interpolated.data,
  respondent_colors = respondent_colors,
  ind.matcher.df = ind.matcher.df, output_dir = "plots/just_final vf/", 
  include_respondent_lines = F,
  include_legend = F
)

```

# Plots of respondent value functions

Coloured by respondent

```{r plot-boot-and-resp}
# using continuous_vf_fig() function, 

plot_indicator_rounds(
  c.df = c.df,
  boot_results = boot_results,
  interpolated_data = interpolated.data,
  respondent_colors = respondent_colors,
  ind.matcher.df = ind.matcher.df, output_dir = "plots/vfs-av_and_individual/", 
  include_respondent_lines = T,
  include_legend = T
)


dum.resps = data.frame(resp = rep(unique(c.df$respondant_name), each = 2),
                       x = rep(0:1, times = length(unique(c.df$respondant_name))),
                       y = rep(0:1, times = length(unique(c.df$respondant_name))))


# Extract the legend
legend <- get_legend(
  ggplot() +
    geom_line(data = dum.resps, aes(x = x, y = y, color = resp)) +
    scale_colour_manual(values = respondant_colours,
                        name = NULL,
                        guide = guide_legend(ncol = 10)) + # Adjust the number of columns here
    theme_pubr() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
)

round_data <- list()

for (indicator in names(boot_results)) {
  # Initialize an empty list to hold plots for the rounds
  round_plots <- list()
  
  for (round in seq_along(boot_results[[indicator]])) {
    
    round_data[[indicator]][[round]] <- boot_results[[indicator]][[round]]
    
    if (is.null(round_data[[indicator]][[round]]$boot_response) || 
        round_data[[indicator]][[round]]$indicator == "Herbivore damage" && round == 1) {
      plot <- ggplot() +
        theme_void() +
        ggtitle(paste(indicator, "Round", round, "(No Data)"))
    } else {
      plot_data <- round_data[[indicator]][[round]]$boot_response
      resps <- names(interpolated.data[[indicator]][[round]])
      respondent_data <- data.frame(measurement = NA, value = NA, resp = NA)[0, ]
      
      for (resp in resps) {
        if (!is.null(interpolated.data[[indicator]][[round]][[resp]]$value_function)) {
          respondent_data <- rbind(respondent_data,
                                   data.frame(
                                     measurement = interpolated.data[[indicator]][[round]][[resp]]$value_function$measurement,
                                     value = interpolated.data[[indicator]][[round]][[resp]]$value_function$value,
                                     resp = resp
                                   ))
        }
      }
      
      # Create the plot
      plot <- ggplot(plot_data, aes(x = expl, y = median)) +
        geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.2, fill = "grey63") +
        geom_ribbon(aes(ymin = lowerq, ymax = upperq), alpha = 0.2, fill = "grey13") +
        geom_line(data = respondent_data, aes(x = measurement, y = value, color = resp),
                  alpha = 0.3,
                  linetype = "longdash") +
        geom_line(color = "black", linewidth = 1) +
        scale_color_manual(values = respondent_colors) +
        labs(
          title = paste("Round", round, "N =", length(unique(respondent_data$resp))),
          y = "Value"
        ) +
        theme_pubr() +
        theme(axis.title.x = element_blank(), legend.position = "none")
    }
    
    round_plots[[round]] <- plot
  }
  
  # Combine plots into a grid
  grid_plots <- plot_grid(
    plotlist = round_plots,
    ncol = 2,
    labels = NULL
  )
  
  
  # Create shared title and x-axis
  shared_title <- ggdraw() +
    draw_label(
      label = paste(indicator),
      fontface = "bold",
      size = 16,
      hjust = 0.5
    )
  
  shared_x_axis <- ggdraw() +
    draw_label(
      label = ind.matcher.df$ind.axis.title[ind.matcher.df$sheet_name == indicator],
      fontface = "bold",
      size = 12,
      hjust = 0.5
    )
  
  # Combine all components: title, plots, x-axis, and legend
  final_plot <- plot_grid(
    shared_title,
    grid_plots,
    shared_x_axis,
    legend,
    ncol = 1,
    rel_heights = c(1, 10, 1, 4) # Adjust relative heights for title, plots, x-axis, and legend
  )
  
  # Save the plot
  ggsave(
    filename = paste0("plots/vfs-av_and_individual/", indicator, "_round_plots.png"),
    plot = final_plot,
    width = 10, height = 7
  )
  
  print(final_plot)
}

```

# Weightings

## Round 1
```{r R1_weights_kable}

# kable of median weightings for each indicator in each round using c.just.one.df
library(kableExtra)

c.just.one.df %>% 
  filter(round == 1) %>% 
  group_by(sheet_name) %>% 
  summarise(median_weighting = median(weight, na.rm = T),
            upper_q_weighting = quantile(weight, probs = 0.75, na.rm = T),
            lower_q_weighting = quantile(weight, probs = 0.25, na.rm = T),
            indicator_num= indicator_num[1]) %>% 
  arrange(indicator_num) %>% 
  mutate(inter_q_range = paste(lower_q_weighting, upper_q_weighting, sep = " - ")) %>%
  select(sheet_name, median_weighting, inter_q_range) %>%
  rename("Indicator" = sheet_name, "Median Weighting" = median_weighting, "Interquartile Range" = inter_q_range) %>%
  kable()



```
```{r R1_weights_plot}
weightings_box(just.one.df = c.just.one.df %>% 
                 filter(round == 1), respondant_colours)

```


## Round 2

```{r R2_weights_kable}
r2_weights_df <- c.just.one.df %>% 
  filter(round == 2) %>% 
  group_by(sheet_name) %>% 
  summarise(median_weighting = median(weight, na.rm = T),
            upper_q_weighting = quantile(weight, probs = 0.75, na.rm = T),
            lower_q_weighting = quantile(weight, probs = 0.25, na.rm = T),
            indicator_num= indicator_num[1]) %>% 
  arrange(indicator_num) %>% 
  mutate(inter_q_range = paste(lower_q_weighting, upper_q_weighting, sep = " - ")) %>%
  select(sheet_name, median_weighting, inter_q_range) %>%
  rename("Indicator" = sheet_name, "Median Weighting" = median_weighting, "Interquartile Range" = inter_q_range)

r2_weights_df %>%
  kable()

```

```{r R2_weights_plot}
weightings_box(just.one.df = c.just.one.df %>% 
                 filter(round == 2), respondant_colours)

```

# Preicting woodland condition for example woodlands

```{r Setting up prediction}

# set parameters
r = 2
weights_df = r2_weights_df
# Save the data as RDS files for shiny app
saveRDS(boot_results, "Data//boot_results.rds")
saveRDS(weights_df, "Data//weights_df.rds")


set.seed(25631)




prediction_data <- data.frame(
  "Tree age distribution" =     c(2,3),
  "Native canopy percentage " = c(100, 50),
  "Vertical structure" =       c(NA, NA),
  "N tree & shrub spp." =      c(NA, NA),       
  "Invasive plants % cover" =  c(NA, NA),
  "Deadwood" =                 c(NA, NA), 
  "Veteran trees" =            c(NA, NA), 
  "Woodland extent" =          c(NA, NA),
  "Regen" =                    c(NA, NA), 
  "Herbivore damage" =         c(NA, NA), 
  "Tree health" =              c(NA, NA), 
  "Ground flora" =             c(NA, NA),              
  "Horizontal complexity" =    c(NA, NA), 
  "Anthropogenic damage" =     c(NA, NA),
  check.names = FALSE # Prevents automatic renaming
)

prediction_values <- prediction_data[0,]
prediction_weighted_values <- prediction_data[0,]


for (y in 1:nrow(prediction_data)){ # each scenario
  for (i in 1:ncol(prediction_data)){# for each indicator
    ind = colnames(prediction_data)[i]
    
    prediction_values[y,i] <- 
      ifelse(is.na(prediction_data[y,i]),
             NA,
             boot_results[[i]][[r]]$boot_response$median[
               # find the boot_results expl with the closest value to the prediction data
               which.min(
                 abs(
                   boot_results[[i]][[r]]$boot_response$expl - prediction_data[y,i]
                 )
               )
             ])
    prediction_weighted_values[y,i] <-
      prediction_values[y,i]* weights_df$`Median Weighting`[weights_df$Indicator == ind]/100
  }
} 

condition_score <- NA
for (y in 1:nrow(prediction_weighted_values)){
  condition_score[y] <- 100 * sum(prediction_weighted_values[y,], na.rm = T)/
    sum(weights_df$`Median Weighting`[
      weights_df$Indicator %in% colnames(prediction_weighted_values)[
        !is.na(prediction_weighted_values[y,])]
    ], 
    na.rm = T) 
}

condition_score
```

```{r shiny_calculation}
library(shiny)
library(rhandsontable)

# Define the condition scoring function
calculate_condition_score <- function(prediction_data, boot_results, weights_df, r) {
  print("Starting calculation of condition score...")  # Debug line
  
  prediction_values <- prediction_data[0,]
  prediction_weighted_values <- prediction_data[0,]
  
  for (y in 1:nrow(prediction_data)) { # each scenario
    for (i in 1:ncol(prediction_data)) { # for each indicator
      ind = colnames(prediction_data)[i]
      
      print(paste("Calculating for row", y, "column", i, "indicator", ind))  # Debug line
      
      prediction_values[y,i] <- 
        ifelse(is.na(prediction_data[y,i]),
               NA,
               boot_results[[i]][[r]]$boot_response$median[
                 which.min(abs(boot_results[[i]][[r]]$boot_response$expl - prediction_data[y,i]))
               ])
      prediction_weighted_values[y,i] <- 
        prediction_values[y,i] * weights_df$`Median Weighting`[weights_df$Indicator == ind] / 100
    }
  }
  
  condition_score <- NA
  for (y in 1:nrow(prediction_weighted_values)) {
    condition_score[y] <- 100 * sum(prediction_weighted_values[y,], na.rm = TRUE) /
      sum(weights_df$`Median Weighting`[
        weights_df$Indicator %in% colnames(prediction_weighted_values)[
          !is.na(prediction_weighted_values[y,])]
      ], na.rm = TRUE)
  }
  
  print("Condition score calculation complete.")  # Debug line
  return(condition_score)
}

# UI
ui <- fluidPage(
  titlePanel("Condition Score Calculator"),
  sidebarLayout(
    sidebarPanel(
      h3("Input Prediction Data"),
      numericInput("num_scenarios", "Number of Scenarios", value = 2, min = 1, max = 10, step = 1),
      actionButton("update_grid", "Update Grid"),
      actionButton("calculate", "Calculate Condition Scores"),
      hr(),
      h3("Condition Scores"),
      tableOutput("condition_scores")
    ),
    mainPanel(
      h3("Survey Data"),
      rHandsontableOutput("prediction_grid")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load boot_results and weights_df from RDS files
  boot_results <- readRDS("Data//boot_results.rds")
  weights_df <- readRDS("Data//weights_df.rds")
  
  # Reactive expression for number of scenarios
  num_scenarios_reactive <- reactive({
    input$num_scenarios
  })
  
  # Initialize prediction data with reactiveVal
  prediction_data <- reactiveVal(
    data.frame(
      matrix(NA, nrow = 14, ncol = 2, dimnames = list(
        c("Tree age distribution", "Native canopy percentage", "Vertical structure",
          "N tree & shrub spp.", "Invasive plants % cover", "Deadwood", "Veteran trees", 
          "Woodland extent", "Regen", "Herbivore damage", "Tree health", "Ground flora", 
          "Horizontal complexity", "Anthropogenic damage"), 
        paste0("Scenario ", 1:2)))
    )
  )
  
  # Update the grid based on the number of scenarios
  observeEvent(input$update_grid, {
    num_scenarios <- num_scenarios_reactive()
    current_data <- prediction_data()
    new_data <- data.frame(
      matrix(NA, nrow = nrow(current_data), ncol = num_scenarios,
             dimnames = list(rownames(current_data), paste0("Scenario ", 1:num_scenarios)))
    )
    prediction_data(new_data)
  })
  
  # Render the prediction data grid
  output$prediction_grid <- renderRHandsontable({
    rhandsontable(prediction_data(), rowHeaders = TRUE) %>%
      hot_cols(colWidths = 100, format = "0.0", type = "numeric") # Ensure cells are numeric
  })
  
  # Update prediction data based on user edits
  observeEvent(input$prediction_grid, {
    new_data <- hot_to_r(input$prediction_grid)
    prediction_data(new_data)
  })
  
  # Calculate condition scores when button is clicked
  output$condition_scores <- renderTable({
    req(input$calculate) # Wait for the user to click calculate
    
    
    # Ensure the input data is not empty
    prediction_data_current <- prediction_data()
    
    if (nrow(prediction_data_current) == 0) {
      return(data.frame(Scenario = character(0), ConditionScore = numeric(0)))
    }
    
    # Ensure calculation function is returning valid values
    scores <- calculate_condition_score(prediction_data_current, boot_results, weights_df)
    
    if (length(scores) == 0) {
      return(data.frame(Scenario = character(0), ConditionScore = numeric(0)))
    }
    
    data.frame(Scenario = colnames(prediction_data_current), ConditionScore = round(scores, 2))
  })
}


# Run the application
shinyApp(ui = ui, server = server)


```
