
# Rarefaction #################################################################

# Package
pacman::p_load(
  tidyverse,  # data management
  iNEXT,      # rarefaction
  patchwork, ggpubr,
  gridExtra, grid
  )


x = df_bird
x = df_butterfly

# Function of iNEXT -------------------------
est_inext <- function(x) {
  
  past <- x %>% 
    dplyr::select(site:value) %>% 
    dplyr::filter(time == "past") %>% 
    pivot_wider(names_from = site, values_from = value) %>%  
    dplyr::filter(rowSums(across(where(is.numeric))) > 0) %>% 
    column_to_rownames("species") %>% 
    dplyr::select(-time) 
  
  present <- x %>% 
    dplyr::select(site:value) %>% 
    dplyr::filter(time == "now") %>% 
    pivot_wider(names_from = site, values_from = value) %>% 
    dplyr::filter(rowSums(across(where(is.numeric))) > 0) %>% 
    column_to_rownames("species") %>% 
    dplyr::select(-time) 
  
  
  # list for inxext
  list_inext <- list("Past" = past, "Present" = present) 
  
  
  # doing 
  iNEXT(list_inext, q = 0, datatype = "incidence_raw")
  
  }



# Run a iNEXT -------------------------------------------------

## Plant
df_plant_trait %>% {
  
  # All species
  est_inext(.) ->> inext_plant
  
  # Native species
  dplyr::filter(., exotic == "Native") %>% 
    est_inext(.) ->> inext_plant_n
  
  # Exotic species
  dplyr::filter(., exotic == "Exotic") %>% 
    est_inext(.) ->> inext_plant_e
  }


## Bird
df_bird %>% {
  
  # All species
  est_inext(.) ->> inext_bird
  
  # Native species
  dplyr::filter(., exotic == "Native") %>% 
    est_inext(.) ->> inext_bird_n
  }


## Butterfly
df_butterfly %>% {
  
  # All species
  est_inext(.) ->> inext_butterfly
  
  # Native species
  dplyr::filter(., exotic == "Native") %>% 
    est_inext(.) ->> inext_butterfly_n
  }






## Plot --------------------------------------------------------

# Plant
p_inext_plant <-
  ggiNEXT(inext_plant) +
    labs(
      title = "Plant",
      x = "Number of sites",
      y = "Species richness"
      ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(limits = c(0, 30)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )


# Native plant
p_inext_plant_n <-
  ggiNEXT(inext_plant_n) +
    labs(
      title = "",
      x = "Number of sites",
      y = "Species richness"
    ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(limits = c(0, 30)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )



# Exotic plant
p_inext_plant_e <-
  ggiNEXT(inext_plant_e) +
    labs(
      title = "",
      x = "Number of sites",
      y = "Species richness"
    ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    #scale_y_continuous(limits = c(0, 1500)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )




# Bird
p_inext_bird <-
  ggiNEXT(inext_bird) +
    labs(
      title = "Bird",
      x = "Number of sites",
      y = "Species richness"
      ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(limits = c(0, 15)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )



# Native bird
p_inext_bird_n <-
  ggiNEXT(inext_bird_n) +
    labs(
      title = "",
      x = "Number of sites",
      y = "Species richness"
    ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(limits = c(0, 15)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )



# Butterfly
p_inext_butterfly <-
  ggiNEXT(inext_butterfly) +
    labs(
      title = "Butterfly",
      x = "Number of sites",
      y = "Species richness"
    ) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(limits = c(0, 15)) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      )


# Native butterfly
p_inext_butterfly_n <-
  ggiNEXT(inext_butterfly_n) +
  labs(
    title = "",
    x = "Number of sites",
    y = "Species richness"
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_x_continuous(limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
    )






## Customizing point/ line size change
custom_inext <- function(x) {

  gb <- ggplot_build(x) 
  gb$data[[1]]$size <- 3  # point size
  gb$data[[2]]$size <- 1  # line size
  
  ggplot_gtable(gb) 
}

p_inext_plant   <- custom_inext(p_inext_plant)
p_inext_plant_n <- custom_inext(p_inext_plant_n)
p_inext_plant_e <- custom_inext(p_inext_plant_e)
p_inext_bird    <- custom_inext(p_inext_bird)
p_inext_bird_n  <- custom_inext(p_inext_bird_n)
p_inext_butterfly <- custom_inext(p_inext_butterfly)
p_inext_butterfly_n <- custom_inext(p_inext_butterfly_n)




## Combining ----

# Extract a legend
p_legend <- inext_butterfly_n %>% 
  ggiNEXT() + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme(
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.7, 'cm'))

legend_inext <- p_legend %>% 
  ggpubr::get_legend() %>% 
  ggpubr::as_ggplot()


# Axis label
bottom <- grid::textGrob("Number of sites", gp = gpar(fontsize = 12))
y_left <- grid::textGrob("Species richness", gp = gpar(fontsize = 12), rot = 90)

grid.arrange(
  p_inext_plant, p_inext_plant_n, p_inext_plant_e,
  p_inext_bird, p_inext_bird_n, legend_inext,
  p_inext_butterfly, p_inext_butterfly_n,
  nrow = 3,
  left = y_left, bottom = bottom,
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 5, 6),
                        c(7, 8, NA))
  ) 



# For save
ggsave(
  grid.arrange(
    p_inext_plant, p_inext_plant_n, p_inext_plant_e,
    p_inext_bird, p_inext_bird_n, legend_inext,
    p_inext_butterfly, p_inext_butterfly_n,
    nrow = 3,
    left = y_left, bottom = bottom,
    layout_matrix = rbind(c(1, 2, 3),
                          c(4, 5, 6),
                          c(7, 8, NA))), 
  file = "output/iNEXT.pdf", 
  width = 180, height = 180, units = "mm", dpi = 600)




