
# Plot combination ============================================================


# Package
pacman::p_load(
  tidyverse,  # data management
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  )




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
ggsave(box_sr_time, file = "output/box_sr_time.png", width = 80, height = 80, units = "mm", dpi = 500)









# Species composition ---------------------------------------------------------

# Plant
p_nmds_plant <- 
  comp_plant %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = rep(c("Past", "Present"), 45),
    time = factor(time, levels = c("Past", "Present")),
    exotic = factor(
      exotic, 
      levels = c("All species", "Native species", "Exotic species"))
  ) %>% 
  
  ggplot(aes(x = NMDS1, y = NMDS2, color = time)) +
  geom_point(size = 0.5) +
  stat_ellipse(
    aes(group = time, fill = time), 
    alpha = 0.1, linewidth = 0.1,
    geom = "polygon") +
  
  facet_wrap(.~exotic) +
  
  
  # Stress value
  geom_text(
    data = df_label_plant, aes(label = stress), 
    x = -1.7, y = 1.7, size = 1.1, color = "grey50", hjust = 0) +
  
  # P value from Permanova
  geom_text(
    data = df_label_plant, aes(label = permanova), 
    x = -1.7, y = 1.4, size = 1.1, color = "grey50", hjust = 0) +
  
  labs(title = "Plant") + 
  scale_x_continuous(limits = c(-1.8, 1.8)) +
  scale_y_continuous(limits = c(-1.8, 1.8)) +
  #coord_fixed() +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw(base_size = 5) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
    )

p_nmds_plant



# Bird
p_nmds_bird <- 
  comp_bird %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = rep(c("Past", "Present"), 14),
    time = factor(time, levels = c("Past", "Present"))
  ) %>% 
  
  ggplot(aes(x = NMDS1, y = NMDS2, color = time, group = time)) +
  geom_point(size = 0.5) +
  stat_ellipse(
    aes(group = time, fill = time), 
    alpha = 0.1, linewidth = 0.1,
    geom = "polygon") +
  
  facet_wrap(. ~ exotic) +
  
  
  # p-value
  geom_text(
    data = df_label_bird, aes(label = stress), 
    x = -1.2, y = 1.18, hjust = 0, size = 1.1, color = "grey50") +
  
  # permanova
  geom_text(
    data = df_label_bird, aes(label = permanova), 
    x = -1.2, y = 1.0, hjust = 0, size = 1.1, color = "grey50") +
  
  
  # adjustment
  labs(title = "Bird", fill = "Time", color = "Time") + 
  #scale_x_continuous(limits = c(-1.0, 1.0)) +
  scale_y_continuous(breaks = seq(-1, 1, length = 3), 
                     limits = c(-1.2, 1.2)) +
  #coord_fixed() +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw(base_size = 5) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.background = element_blank(),
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(3, "mm"),
    ) 

p_nmds_bird




# Butterfly
p_nmds_butterfly <- 
  comp_butterfly %>% 
    dplyr::select(exotic, nmds_score) %>% 
    unnest(nmds_score) %>% 
    dplyr::mutate(
      time = rep(c("Past", "Present"), 14),
      time = factor(time, levels = c("Past", "Present"))
      ) %>% 
    
    ggplot(aes(x = NMDS1, y = NMDS2, color = time)) +
    geom_point(size = 0.5) +
    stat_ellipse(
      aes(group = time, fill = time), 
      alpha = 0.1, linewidth = 0.1,
      geom = "polygon") +
    
    facet_wrap(. ~ exotic) +
    
    # Stress value
    geom_text(
      data = df_label_butterfly, aes(label = stress), 
      x = -1.2, y = 1.15, size = 1.1, color = "grey50", hjust = 0) +
    
    # permanova
    geom_text(
      data = df_label_butterfly, aes(label = permanova), 
      x = -1.2, y = 0.98, hjust = 0, size = 1.1, color = "grey50") +
  
    labs(title = "Butterfly") + 
    #scale_x_continuous(limits = c(-1.1, 1.1)) +
    scale_y_continuous(breaks = seq(-1, 1, length = 3), limits = c(-1.2, 1.2)) +
    #coord_fixed() +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    theme_bw(base_size = 5) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank()
      )


p_nmds_butterfly



## Combining
layout_nmds <- "
AAAAAA
BBBBDD
CCCC##
"

nmds_time <- 
  p_nmds_plant + p_nmds_bird + p_nmds_butterfly + guide_area() +
  plot_layout(design = layout_nmds,  guides = "collect",
              widths = c(1, 2, 2, 1.7)) 

  

# For save
ggsave(nmds_time, file = "output/nmds_time.png", 
       width = 80, height = 85, units = "mm", dpi = 500)



