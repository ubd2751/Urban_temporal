
# NMDS #######################################################

# Package ----------------
pacman::p_load(
  tidyverse,  # data management
  lme4, lmerTest, broom, multcomp, #GLM
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  vegan, iNEXT, betapart, # composition
  exactRankTests  # wilcoxon test
)


# For read_csv
options(
  readr.num_columns = 0L,
  readr.show_col_types = FALSE,
  readr.show_progress = FALSE
)


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

df_comp_plant     <- df_comp(df_plant) 
df_comp_bird      <- df_comp(df_bird) 
df_comp_butterfly <- df_comp(df_butterfly) 




# NMDS & Permanova ----------------------------------------------------------

# Function of estimation for nmds, permanova, and dispersion
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
      
      # Jaccard dissimilarity
      jac = map(comp, ~vegdist(., method = "jac")),
      
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
      pval_perma = map(permanova, ~as_tibble(.) %>% dplyr::select("Pr(>F)")),
      
      
      # dispersion
      dispersion = map(data, ~betadisper(
        vegdist(.[,-1:-2], method = "jac"), .$time)
        ),
      
      # p-value of dispersion
      pval_disper = map(dispersion, ~anova(.) %>% dplyr::select("Pr(>F)"))
      )
}



comp_plant <- est_nmds_prmnv(df_comp_plant) %>% 
  dplyr::mutate(
    exotic = recode_factor(
      exotic, 
      "All species" = "All species",
      "Native species" = "Native species")
    )

comp_bird <- est_nmds_prmnv(df_comp_bird)
comp_butterfly <- est_nmds_prmnv(df_comp_butterfly)



comp_plant %>% 
  dplyr::select(pval_disper) %>% 
  unnest(pval_disper) 
  View()



## Plot -----------------------------------------------------------------------

## Label for stress value and p-value of permanova
label_nmds <- function(x) {
  
  x %>% 
    dplyr::select(exotic, nmds_stress, pval_perma) %>% 
    unnest(pval_perma) %>% 
    na.omit() %>% 
    dplyr::mutate(
      across(where(is.numeric), ~round(., digits = 3)),
      time = "Past",
      stressvalue = "Stress value:",
      permanova_cha = "Permanova: p = ",
      dispersion_cha = "Dispersion: p = "
    ) %>% 
    unite(stress, stressvalue, nmds_stress, remove = F, sep = "") %>% 
    unite(permanova, permanova_cha, "Pr(>F)", remove = F, sep = "") 
}

label_nmds_plant <- label_nmds(comp_plant)
label_nmds_bird <- label_nmds(comp_bird)
label_nmds_butterfly <- label_nmds(comp_butterfly)



# Plot for NMDS ----------

# Plant
p_nmds_plant <- 
  comp_plant %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = rep(c("Past", "Present"), 30),
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
    strip.background = element_blank(),
    
    legend.title = element_blank(),
    legend.position = c(0.9, 0.89),
    legend.text = element_text(size = 4),
    legend.key.height = unit(2.5, "mm"),
    legend.key.width = unit(2.5, "mm"),
    legend.background = element_blank()
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
    legend.position = "none"
  ) 

p_nmds_bird





# Butterfly
p_nmds_butterfly <- comp_butterfly %>% 
  dplyr::select(exotic, nmds_score) %>% 
  unnest(nmds_score) %>% 
  dplyr::mutate(
    time = c(rep(c("Present", "Past"), 7), rep(c("Past", "Present"), 7)),
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
    x = -1.2, y = 0.88, size = 1.3, color = "grey50", hjust = 0) +
  
  # permanova
  geom_text(
    data = label_nmds_butterfly, aes(label = permanova), 
    x = -1.2, y = 0.8, hjust = 0, size = 1.3, color = "grey50") +
  
  labs(title = "Butterfly") + 
  scale_x_continuous(breaks = seq(-1, 1, length = 3)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, length = 3)) +
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
p_nmds <- p_nmds_plant / p_nmds_bird /  p_nmds_butterfly


# Save
ggsave(p_nmds, file = "output/nmds_time.pdf", 
       width = 80, height = 130, units = "mm", dpi = 600)








