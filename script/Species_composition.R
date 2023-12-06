
# Species Composition #######################################################


# data frame for species composition
df_comp <- function(x) {
  
  x %>% 
    dplyr::filter(value == 1) %>% 
    dplyr::select(site, time, exotic, species, value) %>% 
    pivot_wider(
      names_from = species, 
      values_from = value,
      values_fill = 0
    )
  
} 

df_comp_plant <- df_comp(df_plant) 
df_comp_bird <- df_comp(df_bird) 
df_comp_butterfly <- df_comp(df_butterfly) 




# NMDS & Permanova ----------------------------------------------------------

# Function of estimation for nmds and permanova
est_nmds_prmnv <- function(x) {
  
  x %>% 
    dplyr::group_by(site, time) %>% 
    dplyr::summarise(across(where(is.numeric), sum), .groups = "drop") %>% 
    dplyr::mutate(exotic = "All") %>% 
    dplyr::select(site, time, exotic, everything()) %>% 
    dplyr::bind_rows(x) %>% 
    group_nest(exotic) %>% 
    
    # if data is plant, adding a #
    dplyr::filter(exotic != "Exotic") %>% 
    
    
    dplyr::mutate(
      
      # Rename
      exotic = recode_factor(
        exotic, 
        "All" = "All species",
        "Native" = "Native species"),
      
      # Remove columns with a total of 0
      comp = map(
        data,
        ~ dplyr::select(., where(~ is.numeric(.) && sum(.) != 0))),
      
      # NMDS
      nmds = map(
        comp, 
        ~ metaMDS(., dist = "jac", k = 2, trace = F, trymax = 999)),
      
      # NMDS axis score
      nmds_score = map(nmds, ~scores(.) %>% pluck(1) %>% as.data.frame()),
      
      # NMDS stress value
      nmds_stress = map_dbl(nmds, ~.$stress),
      
      # permanova
      permanova = map(data, ~adonis2(.[,-1:-2] ~ .$time, method = "jac")),
      
      # p-value of permanova
      pval = map(permanova, ~as_tibble(.) %>% dplyr::select("Pr(>F)"))
      )
}



comp_plant <- est_nmds_prmnv(df_comp_plant) %>% 
  dplyr::mutate(
    exotic = recode_factor(
      exotic, 
      "All species" = "All species",
      "Native species" = "Native species",
      "Exotic" = "Exotic species")
    )

comp_bird <- est_nmds_prmnv(df_comp_bird)
comp_butterfly <- est_nmds_prmnv(df_comp_butterfly)








# Plot -----------------------------------------------------------------------

## Labels --------------
## Label for stress value and p-value of permanova
label_nmds <- function(x) {
  
  x %>% 
    dplyr::select(exotic, nmds_stress, pval) %>% 
    unnest(pval) %>% 
    na.omit() %>% 
    dplyr::mutate(
      across(where(is.numeric), ~round(., digits = 3)),
      time = "Past",
      stressvalue = "Stress value:",
      permanova_cha = "Permanova: p = "
    ) %>% 
    unite(stress, stressvalue, nmds_stress, remove = F, sep = "") %>% 
    unite(permanova, permanova_cha, "Pr(>F)", remove = F, sep = "") 
}

label_nmds_plant <- label_nmds(comp_plant)
label_nmds_bird <- label_nmds(comp_bird)
label_nmds_butterfly <- label_nmds(comp_butterfly)





# Plot for NMDS ----------

# Plant
p_nmds_plant <- comp_plant %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = rep(c("Past", "Present"), 45),
    time = factor(time, levels = c("Past", "Present"))
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
    data = label_nmds_plant, aes(label = stress), 
    x = -1.7, y = 1.7, size = 1.3, color = "grey50", hjust = 0) +
  
  # P value from Permanova
  geom_text(
    data = label_nmds_plant, aes(label = permanova), 
    x = -1.7, y = 1.5, size = 1.3, color = "grey50", hjust = 0) +
  
  labs(title = "Plant") + 
  scale_x_continuous(limits = c(-1.8, 1.8)) +
  scale_y_continuous(limits = c(-1.8, 1.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw(base_size = 7) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
    )

p_nmds_plant





# Bird
p_nmds_bird <- comp_bird %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = c(rep(c("Present", "Past"), 7), rep(c("Past", "Present"), 7)),
    time = factor(time, levels = c("Past", "Present"))
    ) %>% 
  
  ggplot(aes(x = NMDS1, y = NMDS2, color = time, group = time)) +
  geom_point(size = 0.5) +
  stat_ellipse(
    aes(group = time, fill = time), 
    alpha = 0.1, linewidth = 0.1,
    geom = "polygon"
    ) +
  
  facet_wrap(. ~ exotic) +
  
  # p-value
  geom_text(
    data = label_nmds_bird, aes(label = stress), 
    x = -2, y = 1.25, hjust = 0, size = 1.3, color = "grey50") +
  
  # permanova
  geom_text(
    data = label_nmds_bird, aes(label = permanova), 
    x = -2, y = 1.1, hjust = 0, size = 1.3, color = "grey50") +
  
  
  # adjustment
  labs(title = "Bird", fill = "Time", color = "Time") + 
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(-1, 1, length = 3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw(base_size = 7) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    
    legend.title = element_text(size = 8),
    legend.background = element_blank(),
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(5, "mm"),
  ) 

p_nmds_bird





# Butterfly
p_nmds_butterfly <- comp_butterfly %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = rep(c("Present", "Past"), 14),
    time = factor(time, levels = c("Past", "Present"))
  ) %>% 
  
  ggplot(aes(x = NMDS1, y = NMDS2, color = time)) +
  geom_point(size = 0.5) +
  stat_ellipse(
    aes(group = time, fill = time), 
    alpha = 0.1, linewidth = 0.1,
    geom = "polygon"
    ) +
  
  facet_wrap(. ~ exotic) +
  
  # Stress value
  geom_text(
    data = label_nmds_butterfly, aes(label = stress), 
    x = -1.2, y = 1.15, size = 1.3, color = "grey50", hjust = 0) +
  
  # permanova
  geom_text(
    data = label_nmds_butterfly, aes(label = permanova), 
    x = -1.2, y = 1.0, hjust = 0, size = 1.3, color = "grey50") +
  
  labs(title = "Butterfly") + 
  #scale_x_continuous(limits = c(-1.1, 1.1)) +
  scale_y_continuous(limits = c(-1.2, 1.2)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw(base_size = 7) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank()
    )


p_nmds_butterfly





## Combining ---------
layout_nmds <- "
AAAAAA
BBBBDD
CCCC##
"

p_nmds <- 
  p_nmds_plant + p_nmds_bird + p_nmds_butterfly + guide_area() +
  plot_layout(design = layout_nmds,  guides = "collect",
              widths = c(1, 2, 2, 1.3)) 



# Save
ggsave(p_nmds, file = "output/nmds_time.png", 
       width = 120, height = 125, units = "mm", dpi = 500)



