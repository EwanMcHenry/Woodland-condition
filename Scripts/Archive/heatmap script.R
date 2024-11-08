### Heatmaps

library(heatmaply)

generate_interactive_heatmap <- function(data, weight_variable) {
  # Select relevant columns and spread the data
  weights.mat <- data %>%
    dplyr::select(respondant_name, sheet_name, !!sym(weight_variable)) %>%
    spread(sheet_name, !!sym(weight_variable)) %>%
    # Make respondant_name the row names
    column_to_rownames(var = "respondant_name") %>%
    na.omit() %>% # Filter out rows with all NA
    as.matrix()
  
  # Generate interactive heatmap
  heatmaply(weights.mat,
            # Dendrogram = "row",
            xlab = "", ylab = "",
            main = "",
            scale = "column",
            margins = c(60, 100, 40, 20),
            grid_color = "white",
            grid_width = 0.00001,
            titleX = FALSE,
            hide_colorbar = TRUE,
            branches_lwd = 0.1,
            label_names = c("Respondent", "Feature:", "Value"),
            fontsize_row = 5, fontsize_col = 5,
            labCol = colnames(weights.mat),
            labRow = rownames(weights.mat),
            heatmap_layers = theme(axis.line = element_blank())
  )
}



generate_interactive_heatmap(just.one.df, "weight")

#### Ranked weightings

generate_interactive_heatmap(just.one.df, "rank_weight")
```

  ### 
  