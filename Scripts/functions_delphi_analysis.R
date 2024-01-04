library(ggnewscale)
library(ggiraph)
library(cowplot)




# FUNCTION - PLOT RESPONDANTS' continuous INDICATOR VFs ----
ggplot_gam_resp_vf <- function(indicator_name, gam.col = "black", x.lab = ind.matcher.df$ind.axis.title[i], pal = respondant_colours){
  # Filter the data for the selected indicator
  filtered_data <- df[df$indicator_name == indicator_name, ] %>% 
    mutate(respondant_name = as.factor(respondant_name))
  
  # PREDICT TREND ----
  filtered_data$point.influence <- 1/(table(filtered_data$respondant_name)[as.factor(filtered_data$respondant_name)]) %>% 
    as.numeric()
  gam_model <- mgcv::gam(value.dec ~ s(measure, k = 4),
                   data = filtered_data, family = binomial(), weights = point.influence)
  # Create dummy data for prediction
  dummy_data <- data.frame(measure = seq(min(filtered_data$measure), max(filtered_data$measure), length.out = 50))
  # Predict using the GAM model
  dummy_data$predicted_value <- predict(gam_model, newdata = dummy_data, type = "response")*100
  
  # PLOT SPEC, LINES AND TREND ----
  plot <- ggplot() +
    geom_line(data = dummy_data, size = 2, aes(y = predicted_value, x = measure), colour = gam.col) + # gam prediction
    geom_line(data = filtered_data, 
              aes(x = measure, y = value, color = respondant_name,
                  text = map(
                    paste0("<b>", `respondant_name`, "</b>",
                           "<br><b>Indicator:</b> ", measure, ", <b>Value:</b> ", value,"<br>",
                           "<br><b>Certainty:</b> ", cert_val_funct, "<br>",
                           "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"#,
                           #"<b>Sentance: </b>", vf.sentance
                    ),
                    HTML),
                  alpha = cert_val_funct),
              size = 0.5) +
    geom_point(data = filtered_data, 
               size = 2, shape = 16,
               aes(x = measure, y = value, color = respondant_name,
                   text = map(
                     paste0("<b>", `respondant_name`, "</b>",
                            "<br><b>Indicator:</b> ", measure, ", <b>Value:</b> ", value,"<br>",
                            "<br><b>Certainty:</b> ", cert_val_funct, "<br>",
                            "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"#,
                            #        "<b>Sentance: </b>", vf.sentance
                     ),
                     HTML),
                   alpha = cert_val_funct)) +
    scale_colour_manual(values = pal) +
    labs(title = indicator_name, x = x.lab, y = "Value Score",
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
  
  plot
}

# FUNCTION - PLOT RESPONDANTS' CATEGORICAL INDICATOR VFs ----
ggplot_resp_cat_vf <- function(indicator_name, x.lab = ind.matcher.df$ind.axis.title[i], data = vf_cat_data, pal = respondant_colours){
  # vf_cat_data is filtered_data, formatted to include categories - cat_measure
  
  # PLOT SPEC, LINES AND TREND ----
  plot <- ggplot() +
    geom_boxplot(data = data, 
                 aes(x = cat_measure, y = value), text = NULL, outlier.shape = NA) +
    stat_summary(data = data, aes(x = cat_measure, y = value),
                 geom = "point", fun = mean,
                 color = "red", shape = 3, size = 5,
                 position = position_nudge(x = 0.05, y = 0)
    ) +
    geom_jitter(data = data,
                size = 2, shape = 16,
                position=position_jitter(width=.1, height=0),
                aes(x = cat_measure, y = value, color = respondant_name,
                    text = map(
                      paste0("<b>", `respondant_name`, "</b>",
                             "<br><b>Indicator:</b> ", cat_measure, ", <b>Value:</b> ", value,"<br>",
                             "<br><b>Certainty:</b> ", cert_val_funct, "<br>",
                             "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                      HTML),
                    alpha = cert_val_funct)) +
    scale_colour_manual(values = pal) +
    labs(title = indicator_name, x = x.lab, y = "Value Score",
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
  
  plot
}

# FUNCTION - PLOT RESPONDANTS' WEIGHTS - FOR INDICATOR  ----
ggplot_resp_weight_ind <- function(indicator_name, this.ind.num = ind.num, pal = respondant_colours){
  # plot weights by respondant for this indicator
  
  #filter data of one weight for each respondant:indicator
  weights_filtered_data <- df[ !duplicated(paste0(df$respondant_name, df$indicator_name)), ] %>% 
    mutate(respondant_name = as.factor(respondant_name),
           this.ind = (indicator_num == this.ind.num))
  # Filter the data for the selected indicator
  filtered_data <- weights_filtered_data[weights_filtered_data$indicator_name == indicator_name, ]
  
  # PLOT weights for this ind ----
  plot <- ggplot(filtered_data) +
    geom_boxplot(aes(x = 0, y = weight)) +
    stat_summary(data = filtered_data,
                 aes(x = 0, y = weight),
                 geom = "point", fun = mean,
                 color = "red", shape = 3, size = 5) +
    ggiraph::geom_jitter_interactive(aes(x = 0, y = weight, 
                                color = respondant_name,
                                tooltip = respondant_name,
                                data_id = respondant_name,
                                text = map(
                                  paste0("<b>", `respondant_name`, "</b><br>",
                                         "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>",
                                         "<b>Weight rank: </b>", weight_rank),
                                  HTML),
                                alpha = cert_weight), 
                            size = 4, shape = 16,
                            position=position_jitter(width=.1, height=0),
                            show_guide = FALSE) +
    scale_colour_manual(values = pal, name = "Respondant") +
    scale_y_continuous(limits = c(0,100)) +
    labs(y = "Weight",
         colour = "Respondant", title = indicator_name) +
    guides(alpha = F) +
    theme_pubr()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 12 , face = "bold"),
          legend.text =  element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title =  element_text(size = 12),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.box = "vertical",  # Set the legend box to vertical
          legend.position = "right",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm")
    ) 
  
  # PLOT all indicator weights - for all respondants, by indicator, interactive  
  this.indicator.lines = data.frame(
    y = rep(c(0,100), each = length(weights_filtered_data$sheet_name %>% unique())),
    sheet_name = rep(weights_filtered_data$sheet_name %>% unique(), times = 2) %>% 
      as.factor(),
    ind.num = rep(weights_filtered_data$indicator_num %>% unique(), times = 2) 
  )
  this.indicator.lines$this.ind <- this.ind.num == this.indicator.lines$ind.num 
  
  indiv.weights <- ggplot() +
    geom_line(data = this.indicator.lines,
              aes(x = sheet_name, y = y,
                  col = this.ind,
                  data_id = sheet_name),
              show.legend = FALSE)+
    scale_colour_manual(values = c("gray89","steelblue")) +
    new_scale_colour()+
    geom_point_interactive(data = weights_filtered_data, 
                           position = position_jitter(width = 0.2, height = 0),
                           size = 2, shape = 16,
                           aes(x = sheet_name, y = weight,
                               col = weights_filtered_data$respondant_name,
                               tooltip = respondant_name,
                               data_id = respondant_name #,
                               # text = map(
                               #   paste0("<b>", respondant_name, "</b><br>",
                               #          "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                               # HTML)
                           )) +
    scale_colour_manual(values = pal, name = "Respondant") +
    scale_y_continuous(limits = c(0,100)) +
    guides(color = guide_legend(ncol =1)) +
    labs(y = NULL, x = NULL, title = "Comparison to other indicators") +
    theme_pubr()+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 12 , face = "bold"),
          legend.text =  element_text(size = 12),
          axis.text.x = element_text(angle = -60, hjust = 0, size = 10),
          axis.text.y = element_text(size = 12),
          axis.title =  element_text(size = 12),
          legend.box = "vertical",  # Set the legend box to vertical
          legend.position = "right",
          plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
    ) 
  
  plot_grid <-   plot_grid(plot, indiv.weights, rel_widths = c(1, 2), ncol = 2, align = "h", axis = "l")

  ggsave(filename = paste0("Figs//ind_",
                           formatC(ind.num, width = 2, format = "d", flag = "0"),
                           "_", indicator_name, "weights.png"),
         plot = plot_grid,
         width = 300, height = 150, units = "mm")
  

  girafe(
    ggobj = plot_grid,
    width_svg = 10,
    height_svg = 8, 
    options = list(
      opts_hover_inv(css = "stroke-opacity:0.01;"),
      opts_hover(css = "stroke:orange;stroke-width:5;fill-opacity:1")
    ) ) 
  
}

# FUNCTION - RUN PLOTTING OF CATEGORISED VF ----
categorised_vf_fig <- function(){
  # categorical plot
  vf_cat_data = filtered_data %>% 
    mutate(cat_measure = as.factor(measure))
  plot <- ggplot_resp_cat_vf(indicator_name = indicator_name,
                             x.lab = ind.axis.title,
                             data = vf_cat_data )
  
  ggsave(filename = paste0("Figs//ind_",
                           formatC(ind.num, width = 2, format = "d", flag = "0"),
                           "_", indicator_name, "vf_boxplot.png"),
         plot = plot,
         width = 300, height = 150, units = "mm")
  
  ##ggplotly
  plotly_p <- ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
    config(displayModeBar = F) %>% 
    layout(yaxis = list(range = c(-5, 105)))
  
  # remove outliers from plotly
  # for(i in 1:length(plotly_p)){
  # plotly_p$x$data[[i]]$marker$opacity = 0 
  # }
  
  plotly_p
}