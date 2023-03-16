# Ewan McHenry
##------ Fri Mar 10 12:03:15 2023 ------##
# make figures etc explaining the process for getting from measurements of woodland condition indicies to overall condition score
# se


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(htmltools) # for html text rendering in ggplot aes()
library(ggpubr)
library(RColorBrewer)

# CONDITION INDICY VALUES ---------------------------------------------

# generate data ---- 
set.seed(54-46)
con.vals.df = data.frame(ind = factor(c("Tree age distribution",
                                        "Herbivore impact",
                                        "Invasive plants (% cover)",
                                        "Number of native tree species", 
                                        "Native trees in canopy (%)",
                                        "Open space within woodland", 
                                        "Native regeneration",
                                        "Tree health",
                                        "Veteran trees",
                                        "Deadwood",
                                        "Etc…"))
)
con.vals.df$ind = factor(con.vals.df$ind, levels = unique(con.vals.df$ind))

con.vals.df$value = runif(length(con.vals.df$ind), 0,100) %>% round(0)
con.vals.df$weight = runif(length(con.vals.df$ind), 0,100) %>% round(0)
con.vals.df$measure = con.vals.df$value/con.vals.df$weight
con.vals.df$total = "Site Condition Score" # helper for stack

# plot options ---- 
colorCount = length(unique(con.vals.df$ind))
getPalette = colorRampPalette(brewer.pal(colorCount, "Paired"))
type.names = unique(con.vals.df$ind)
text.fact = 1

# plot indocator values sepearatly - p.indicator ----
p.indicator <- ggplot(con.vals.df, aes(fill = ind, x = ind, y = value, 
                                       text = map(paste0("<b>", ind, "</b>", "<br>", " Value =", value, " %"), HTML)))+
  geom_bar( stat="identity", ) +
  #ggtitle()+
  theme_pubr() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Value for this site's condition") +
  labs(fill = "Indicator") + 
  scale_fill_manual(values=rev(getPalette(colorCount)),
                    labels=(type.names)) +
  theme(text = element_text(family = "sans"), 
        legend.position = "right",
        plot.title = element_text(size = 14  *text.fact, face = "bold"),
        plot.subtitle = element_text(size = 8 *text.fact),
        legend.text =  element_text(size = 11 *text.fact),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 11 *text.fact),
        axis.text.x = element_text(size = 11 *text.fact, angle = 310, hjust = 0),
        axis.title.y =  element_text(size = 12 *text.fact),
        axis.title.x =  element_blank(),
  )
# plot stack indicator values - p.indicator.stack ----
p.indicator.stack <- ggplot(con.vals.df, aes(fill = ind, x = total, y = value, 
                                             text = map(paste0("<b>", ind, "</b>", "<br>", " Value =", value, " %"), HTML)))+
  geom_bar( stat="identity", )+
  #ggtitle()+
  theme_pubr()+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name = "Site condition score")+
  labs(fill = "Indicator")+
  scale_fill_manual(values=rev(getPalette(colorCount)),
                    labels=(type.names))+
  theme(text = element_text(family = "sans"),
        legend.position = "right",
        plot.title = element_text(size = 14  *text.fact, face = "bold"),
        plot.subtitle = element_text(size = 8 *text.fact),
        legend.text =  element_text(size = 11 *text.fact),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 11 *text.fact),
        axis.text.x = element_text(size = 11 *text.fact, angle = 310, hjust = 0),
        axis.title.y =  element_text(size = 12 *text.fact),
        axis.title.x =  element_blank(),
  )

# plot sep + stack indicator values - p.indicator.sep_stack ----


gg.bar.sep_stack <- function(
    df = con.vals.df,
    scale.sum = F, stack.alpha = 0, sep.alpha = 1 , big.y_lim = T){
    
    y_lim = c(0,100)
    if(big.y_lim){
      y_lim[2] = sum(df$value)
    }
    
    df2 <-  df
    df2$total  <-  df$ind
    df2  <-  rbind(df2,df)
    df2$total<- factor(df2$total, levels = unique(df2$total))
    
    df2$plot.val <- df$value
    if(scale.sum){
      df2$plot.val[df2$total == df$total[1]] <-  df2$plot.val[df2$total == df$total[1]]/
        dim(df)[1]
    }
    
    df2$alpha.val <-  sep.alpha
    df2$alpha.val[df2$total == df$total[1]] <- stack.alpha
    
    ggplot(df2, aes(fill = ind, x = total, y = plot.val, 
                             text = map(paste0("<b>", ind, "</b>", "<br>", " Value =", plot.val, " %"), HTML),
                             alpha = alpha.val),) +
      geom_bar( stat="identity", )+
      #ggtitle()+
      theme_pubr()+
      scale_x_discrete(name = NULL)+
      scale_y_continuous(name = "Condition value at site")+
      scale_alpha(range = c(0, 1), guide='none')+
      labs(fill = "Indicator")+
      scale_fill_manual(values=rev(getPalette(colorCount)),
                        labels=(type.names))+
      theme(text = element_text(family = "sans"),
            legend.position = "none",
            plot.title = element_text(size = 14  *text.fact, face = "bold"),
            plot.subtitle = element_text(size = 8 *text.fact),
            legend.text =  element_text(size = 11 *text.fact),
            legend.title = element_blank(),
            axis.text.y = element_text(size = 11 *text.fact),
            axis.text.x = element_text(size = 11 *text.fact, angle = 310, hjust = 0),
            axis.title.y =  element_text(size = 12 *text.fact),
            axis.title.x =  element_blank(),
            plot.margin =  margin(0, 2, 0, 0, "cm")
      )
  }
  
  gg.bar.sep_stack(df = con.vals.df,
                scale.sum = F, 
                stack.alpha = 0, sep.alpha = 1, 
                big.y_lim = T )

  
  
  