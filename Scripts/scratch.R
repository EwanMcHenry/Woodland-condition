#scratch





i = 2
col_name = "vf_cert"
this.respondant <- completed_summary$respondent_names[[i]]

max_count = max(completed_summary[, col_name] %>% unlist() %>% as.numeric())
count <- completed_summary[i, col_name] %>% as.numeric()


create_plotly_pie_chart <- function(count, max_count) {
  remaining_text <-  paste(c("<span style='text-align:left'><b>To do:</b>", 
                           paste("<br>",has_completed$sheet_name[has_completed$respondent_names == this.respondant &
                                                         has_completed[,col_name] == F], sep = "",collapse = ""),
                           "</span>"), collapse = "")

  done_text <-  paste(c("<span style='text-align:left'><b>Done:</b>", 
                           paste("<br>",has_completed$sheet_name[has_completed$respondent_names == this.respondant &
                                                                        has_completed[,col_name] == T], sep = "",collapse = ""),
                           "</span>"), collapse = "")

  remaining <- max_count - count
  
  plotly_pie <- plot_ly(
    labels = c("Done", "Remaining"),
    values = c(count, remaining),
    type = "pie",
    text = c(remaining_text, done_text),
    hoverinfo = "text",
    textinfo = "none",
    showlegend = F
  )

  return(plotly_pie)
}


library(gridExtra)
library(grid)


create_gg_pie_chart <- function(count, max_count) {

  percentage <- count / max_count * 100
  
  pie_chart <- ggplot(data.frame(x = c("Done", "Remaining"), y = c(count, max_count - count)),
                      aes(x = "", y = y, fill = x)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "none")
  
  return(pie_chart)
  
}

gg_pie_charts <- list(NA)
bk = 1
for (i in 1:(length(completed_summary$respondent_names))){
  for (ii in 1:(dim(completed_summary)[2]-1)){
    count <- completed_summary[i,1+ii] %>% as.numeric()
    gg_pie_charts[[bk]] <-  create_gg_pie_chart(count, length(unique(df$indicator_name)))
  bk = bk+1
    }}
  

# Convert the list of pie charts to a grid using grid.arrange
pie_matrix <- grid.arrange(grobs = gg_pie_charts, nrow = length(completed_summary$respondent_names), ncol = length(colnames(completed_summary[, c("wt", "wt_cert", "vf", "vf_cert")])))
# Add row names (respondent names)
row_names <- completed_summary$respondent_names

# Add column names
col_names <- c("value funcs", "vf certs", "weights", "wt certs")

combine <- rbind(tableGrob(t(c(col_names)), theme = ttheme_minimal(), rows = ""), 
                 cbind(tableGrob(row_names, theme = ttheme_minimal()), 
                       arrangeGrob(grobs = gg_pie_charts, nrow = length(row_names), ncol = length(col_names)),  size = "last"), size = "last")
grid.newpage()
grid.draw(combine)




pl <- replicate(12, ggplot(), FALSE)

N <- length(pl)
nr <- 4
nc <- 3
combine <- rbind(tableGrob(t(c(col_names[1:4])), theme = ttheme_minimal(), rows = ""), 
                 cbind(tableGrob(row_names[1:nr], theme = ttheme_minimal()), 
                       arrangeGrob(grobs = gg_pie_charts[1:16]),  size = "last"), size = "last")
grid.newpage()
grid.draw(combine)










# Add row names and column names to the grid
row_names_grob <- textGrob(row_names, x = unit(0, "npc"), just = "right", hjust = 1,
                           gp = gpar(fontsize = 10, fontface = "bold"))
col_names_grob <- textGrob(col_names, y = unit(1, "npc"), just = "left", vjust = 1,
                           gp = gpar(fontsize = 10, fontface = "bold"))
pie_matrix_with_labels <- gridExtra::grid.arrange(col_names_grob, row_names_grob, pie_matrix,
                                                  nrow = 2, heights = unit.c(0.1, 0.9),
                                                  top = "Column Names")




# Display the grid of pie charts
pie_matrix

row_names_grob <- textGrob(completed_summary$respondent_names, x = unit(0, "npc"), just = "right", hjust = 1,
                           gp = gpar(fontsize = 10, fontface = "bold"))
pie_matrix_with_row_names <- gridExtra::grid.arrange(row_names_grob, pie_matrix, nrow = 1, heights = unit.c(0.1, 0.9))











# Create a list of pie charts using lapply
plotly_pie_charts <- lapply(completed_summary$respondent_names, 
                     function(respondent_name) {
  counts <- completed_summary[completed_summary$respondent_names == respondent_name,
                              c("wt", "wt_cert", "vf", "vf_cert")] %>% as.numeric()
  lapply(counts, 
         function(count) {
           create_plotly_pie_chart(count, length(unique(df$indicator_name)))
         })
  })


# Convert the list of pie charts to a grid using subplot()
pie_matrix <- 
  plotly::subplot(do.call(grid.arrange, pie_charts), nrows = length(completed_summary$respondent_names), margin = 0.02)

pie_chart_matrix <- subplot(
  nrows = length(completed_summary$respondent_names),
  shareX = TRUE,
  shareY = TRUE,
  titleY = F,
  titleX = F,
  plotlist = pie_charts
)

aa = pie_charts[[2]][[1]]
bb = pie_charts[[2]][[4]]
subplot(aa,bb,  nrows = 2)


# Display the pie chart matrix
pie_matrix


library(patchwork)
pie_matrix <- wrap_plots(pie_charts, ncol = length(colnames(completed_summary[, c("wt", "wt_cert", "vf", "vf_cert")])))

# Display the pie chart matrix
pie_matrix
















# Create a data frame with participant names and the four columns
participant_table <- data.frame(respondent_names = completed_summary$respondent_names,
                                wt = "",
                                wt_cert = "",
                                vf = "",
                                vf_cert = "", stringsAsFactors = FALSE)

# Iterate over each participant and update the table with the indicators not completed
for (i in 1:nrow(completed_summary)) {
  n.indicators <- has_completed$indicator_name %>% unique() %>% length()
  participant <- completed_summary[i, "respondent_names"] %>% as.character()
  not_completed_wt <- has_completed$sheet_name[has_completed$respondent_names == participant & has_completed$wt == FALSE]
  not_completed_wt_cert <- has_completed$sheet_name[has_completed$respondent_names == participant & has_completed$wt_cert == FALSE]
  not_completed_vf <- has_completed$sheet_name[has_completed$respondent_names == participant & has_completed$vf == FALSE]
  not_completed_vf_cert <- has_completed$sheet_name[has_completed$respondent_names == participant & has_completed$vf_cert == FALSE]
  
  if (length(not_completed_wt) == n.indicators) not_completed_wt <- "all"
  if (length(not_completed_wt_cert) == n.indicators) not_completed_wt_cert <- "all"
  if (length(not_completed_vf) == n.indicators) not_completed_vf <- "all"
  if (length(not_completed_vf_cert) == n.indicators) not_completed_vf_cert <- "all"
  
  
  participant_table[participant_table$respondent_names == participant, "wt"] <- paste(not_completed_wt, collapse = ", ")
  participant_table[participant_table$respondent_names == participant, "wt_cert"] <- paste(not_completed_wt_cert, collapse = ", ")
  participant_table[participant_table$respondent_names == participant, "vf"] <- paste(not_completed_vf, collapse = ", ")
  participant_table[participant_table$respondent_names == participant, "vf_cert"] <- paste(not_completed_vf_cert, collapse = ", ")
}

# Print the participant table
participant_table