library(ggnewscale)
library(ggiraph)
library(cowplot)
library(factoextra)
library(FactoMineR)
library(dplyr)
library(tidyr)
library(ggrepel)

# forms.direct <- "Data\\Delphi round 1\\response sheets\\"
# extraction.location <- "Data\\Delphi round 1\\"
# forms.direct <- "Data\\Delphi round 1\\editing for round 2\\"
# forms.direct <- "Data\\Delphi round 1\\response sheets\\"
forms.direct <- "Data\\Delphi round 2\\response sheets\\"
extraction.location <- "Data\\Delphi round 2\\"
# forms.direct <- "Data\\Delphi round 1\\response sheets\\"
# extraction.location <- "Data\\Delphi round 1\\"

# interactive PCA plot -------------------------
generate_interactive_PCA <- function(data, weight_variable) {
  # Select relevant columns and spread the data  
  weights.mat <- data %>%
    dplyr::select(respondant_name, sheet_name, !!sym(weight_variable)) %>%
    spread(sheet_name, !!sym(weight_variable)) %>%
    # Make respondant_name the row names     
    column_to_rownames(var = "respondant_name") %>%
    na.omit() %>% # Filter out rows with all NA     
    as.matrix()  
  
  # Generate PCA   
  pca <- PCA(weights.mat, graph = F)  
  
  # Create PCA biplot
  biplot <- fviz_pca_biplot(pca, repel = TRUE,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            col.ind = "#696969",  # Individuals color
                            geom.ind = "point",
                            col.quanti.sup = "black",
                            col.ind.sup = "black",
                            col.var.sup = "black",
                            pointsize = 1,
                            title = "",
                            axes = c(1, 2),
                            ellipse.level = 0.95,
                            labelsize = 3,
                            legend.title = "",
                            legend.position = "none",
                            legend.justification = "none",
                            legend.direction = "none",
                            legend.box = "none",
                            repel.max.iter = 1000,
                            repel.strength = 5,
                            repel.seed = 123,
                            ggtheme = theme_pubr())
  
  # Add labels using ggrepel
  biplot + geom_text_repel(data = as.data.frame(pca$ind$coord),
                           aes(x = Dim.1, y = Dim.2, label = row.names(pca$ind$coord)),
                           box.padding = 0.5,
                           point.padding = 0.5,
                           segment.color = "grey",
                           segment.size = 0.5,
                           segment.alpha = 0.5,
                           size = 3,
                           force = 2
  )
}

# function to plot continuous delphi results ------------
continuous_vf_fig <- function(line.col = "black"){
  plot <- ggplot_gam_resp_vf(indicator_name = indicator_name,
                             x.lab = ind.axis.title, gam.col = line.col)
  
  ggsave(filename = paste0("Figs//ind_",
                           formatC(ind.num, width = 2, format = "d", flag = "0"),
                           "_", indicator_name, "vf_continuous.png"),
         plot = plot,
         width = 300, height = 150, units = "mm")
  
  ggplotly(plot, tooltip = "text", dynamicTicks = F) %>% 
    config(displayModeBar = F) %>% 
    layout(yaxis = list(range = c(-5, 105)))
}

# FUNCTION - PLOT RESPONDANTS' continuous INDICATOR VFs ------------
ggplot_gam_resp_vf <- function(indicator_name, gam.col = "black", x.lab = ind.matcher.df$ind.axis.title[i], pal = respondant_colours){

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
          legend.text =  element_text(size = 9),
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
                             "<br><b>Measure:</b> ", cat_measure, ", <b>Value:</b> ", value,"<br>",
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
          legend.position = "none",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm")
    ) 
  
  plot
}

# FUNCTION - PLOT RESPONDANTS' WEIGHTS - FOR INDICATOR  ----
ggplot_resp_weight_ind <- function(indicator_name, 
                                   indicator_name_topof_sheet =  ind.matcher.df$indicator_name_topof_sheet[ind.num], 
                                   this.ind.num = ind.num, pal = respondant_colours,
                                   weight_variable = "weight"){
  # plot by respondant for this indicator
  ### weight_variable = weight or weight_standardised
  
  #cheat to fix incase old version of ind.matcher.df is used
  if(names(ind.matcher.df)[4] == "indicator_name_for_extraction"){
    names(ind.matcher.df)[4] <- "indicator_name_topof_sheet"
  }
  
  #filter data of one weight for each respondant:indicator
  weights_filtered_data <- just.one.df %>% #df[ !duplicated(paste0(df$respondant_name, df$indicator_name)), ] %>% 
    mutate(respondant_name = as.factor(respondant_name),
           this.ind = (indicator_num == this.ind.num)
           ) %>%
    # add weights standardised by respondant
    group_by(respondant_name) %>%
    mutate(weight_standardised = (weight - mean(weight, na.rm = T))/sd(weight, na.rm = T),
           ) %>% 
    ungroup()
  
      # arrange sheet_name levels by the median of weight_variable - for plot
  weights_filtered_data$sheet_name <- factor(weights_filtered_data$sheet_name, 
                                                 levels =   weights_filtered_data %>%
                                                   group_by(sheet_name) %>%
                                                   summarise(med_weight = median(get(weight_variable), na.rm = T)) %>%
                                                   arrange(-med_weight) %>%
                                                   select(sheet_name) %>%
                                                   unlist()
  )
  
  # Filter the data for the selected indicator
  filtered_data <- weights_filtered_data[weights_filtered_data$indicator_name == indicator_name_topof_sheet, ] 
    # arrange 
  
ylim <- min(c(0,min(weights_filtered_data[,weight_variable], na.rm = T)), na.rm = T)
ylim[2] <- max(weights_filtered_data[,weight_variable], na.rm = T)
  
    # setup for some lines to help read the plots
    this.indicator.lines = data.frame(
      y = rep(ylim, each = length(weights_filtered_data$sheet_name %>% unique())),
      sheet_name = rep(weights_filtered_data$sheet_name %>% unique(), times = 2) %>% 
        as.factor(),
      ind.num = rep(weights_filtered_data$indicator_num %>% unique(), times = 2) 
    )
  this.indicator.lines$this.ind <- this.ind.num == this.indicator.lines$ind.num 
    
  
  # PLOT weight_variable for this ind ----
  plot <- ggplot(filtered_data) +
    geom_boxplot(aes(x = 0, y = get(weight_variable)), outlier.shape = NA) +
    # geom_boxplot(aes(x = 0, y = weight), outlier.shape = NA) +
    
    stat_summary(data = filtered_data,
                 aes(x = 0, 
                     # y = weight),
                     y = get(weight_variable)),
  geom = "point", fun = mean,
                 color = "red", shape = 3, size = 5) +
    ggiraph::geom_jitter_interactive(aes(x = 0, 
                                         #y = weight, 
                                         y = get(weight_variable),
                                color = respondant_name,
                                tooltip = respondant_name,
                                data_id = respondant_name,
                                text = map(
                                  paste0("<b>", `respondant_name`, "</b><br>",
                                         "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>",
                                         "<b>Weight rank: </b>", rank_weight,
                                         "<br> Standardised weight = ", weight_standardised %>% round(digits = 2)),
                                  HTML),
                                alpha = cert_weight), 
                            size = 4, shape = 16,
                            position=position_jitter(width=.1, height=0),
                            show_guide = FALSE) +
    scale_colour_manual(values = pal, name = "Respondant") +
    scale_y_continuous(limits = ylim) +
    labs(y = weight_variable,
         colour = "Respondant", title = filtered_data$sheet_name) +
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
          legend.position = "none",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm")
    ) 
  
  # PLOT all indicator weights - for all respondants, by indicator, interactive 
  
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
                           aes(x = sheet_name, 
                               #y = weight,
                               y = get(weight_variable),
                               col = weights_filtered_data$respondant_name,
                               tooltip =  paste0(
                                 respondant_name, 
                                 "<br>",sheet_name, 
                                 "<br>Weight = ", weight, 
                                 "<br>Weight rank = ", rank_weight, 
                                 "<br> Standardised weight = ", weight_standardised %>% round(), 
                                 "<br>Certainty = ", cert_weight),
                               data_id = respondant_name #,
                               # text = map(
                               #   paste0("<b>", respondant_name, "</b><br>",
                               #          "<b>Weight: </b>", weight, " (certainty ", cert_weight,")<br>"),
                               # HTML)
                           )) +
    scale_colour_manual(values = pal, name = "Respondant") +
    scale_y_continuous(limits = ylim) +
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
          legend.position = "none",
          plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
    ) 
  
  plot_grid <-   plot_grid(plot, indiv.weights, rel_widths = c(1, 2), ncol = 2, align = "h", axis = "l")


  # ggsave(filename = paste0("Figs//ind_",
  #                          formatC(ind.num, width = 2, format = "d", flag = "0"),
  #                          "_", indicator_name, "weights.png"),
  #        plot = plot_grid,
  #        width = 300, height = 150, units = "mm")
  

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