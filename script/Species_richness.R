
# Species richness ============================================================






# Species richness--------------

# Plant
box_sr_plant <- 
  ggplot(data = sr_plant, aes(x = time, y = sr)) +
    geom_boxplot(
      aes(color = time), outlier.colour = NA,
      width = 0.8, size = 0.2
      ) +
    geom_point(
      aes(color = time), size = 0.5, alpha = 0.8,
      position = position_jitterdodge(-1)
      ) +
    geom_line(aes(group = interaction(exotic, site)), 
              color = "grey50", linewidth = 0.2, alpha = 0.3) +
    facet_wrap(~exotic, ncol = 3) +
    geom_signif(
      data = p_val_wlcx_sr_time,
      aes(y_position = c(870, 830, 220), 
          xmin = c(1, 1, 1), xmax = c(2, 2, 2),
          annotations = char_pval),
      size = 0.1, textsize = 1.3, manual = TRUE, 
      tip_length = 0.05, vjust = -0.5) +
    scale_y_continuous(limits = c(0, 950)) +
    labs(
      x = "Time",
      y = "Species richness",
      fill = "Time",
      title = "Plant"
      ) +
    theme_classic(base_size = 5) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      #strip.background = element_blank(),
      strip.text = element_text(size = 5),
      axis.title.x = element_blank()
      )

box_sr_plant



## Bird
box_sr_bird <- 
  ggplot(data = sr_bird, aes(x = time, y = sr)) +
    geom_boxplot(
      aes(color = time), outlier.colour = NA,
      width = 0.8, size = 0.2
      ) + 
    geom_point(
      aes(color = time), size = 0.5, alpha = 0.8,
      position = position_jitterdodge(-1)
      ) +
    geom_line(
      aes(group = interaction(exotic, site)), 
      color = "grey50", linewidth = 0.2, alpha = 0.3
      ) +
  
    facet_wrap(~exotic, ncol = 3) +
    geom_signif(
      data = p_val_wlcx_sr_bird_time,
      aes(y_position = c(150, 145, 30), 
          xmin = c(1, 1, 1), xmax = c(2, 2, 2),
          annotations = char_pval),
      size = 0.1, textsize = 1.3, manual = TRUE, 
      tip_length = 0.05, vjust = -0.5
      ) +
    scale_y_continuous(limits = c(0, 160)) +
    labs(title = "Bird", y = "Species richness") +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme_classic(base_size = 5) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.title.x = element_blank()
      )

box_sr_bird



## Butterfly
box_sr_butterfly <- 
  ggplot(data = sr_butterfly, aes(x = time, y = sr)) +
    geom_boxplot(
      aes(color = time), outlier.colour = NA,
      width = 0.8, size = 0.2
      ) + 
    geom_point(
      aes(color = time), size = 0.5, alpha = 0.8,
      position = position_jitterdodge(-1)
      ) +
    geom_line(aes(group = interaction(exotic, site)), 
              color = "grey50", linewidth = 0.2, alpha = 0.3) +
    facet_wrap(~exotic, ncol = 2) +
    geom_signif(
      data = p_val_wlcx_sr_butterfly,
      aes(y_position = c(60, 60), 
          xmin = c(1, 1), xmax = c(2, 2),
          annotations = char_pval),
      size = 0.1, textsize = 1.3, manual = TRUE, 
      tip_length = 0.05, vjust = -0.5) +
    scale_y_continuous(limits = c(10, 70)) +
    labs(
      title = "Butterfly",
      fill = "Time",
      x = "Time",
      y = "Species richness"
    ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme_classic(base_size = 5) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank()
      )

box_sr_butterfly




## Combining
layout <- "
AAAAAA
BBBBBB
CCCC##
"

box_sr_time <- 
  (box_sr_plant / box_sr_bird / box_sr_butterfly) +
  plot_layout(design = layout) 
  


# For save
ggsave(box_sr_time, file = "output/box_sr_time.png",
       width = 80, height = 80, units = "mm", dpi = 500)






