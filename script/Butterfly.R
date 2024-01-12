
# Butterfly ###################################################################

# Package
pacman::p_load(
  tidyverse,  # data management
  lme4, lmerTest, broom, multcomp, #GLM
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  vegan # composition
  )


# For read_csv
options(
  readr.num_columns = 0L,
  readr.show_col_types = FALSE,
  readr.show_progress = FALSE
  )



# organization of the trait data
butterfly_trait <- read_csv("data/butterfly/butterfly_trait.csv")

df_trait_butterfly <- butterfly_trait %>% 
  dplyr::filter(!is.na(OvipositionStyle)) %>% 
  rowwise() %>% 
  dplyr::mutate(
    WS = mean(c_across(starts_with("WS")), na.rm =TRUE),
    FW = mean(c_across(starts_with("WS")), na.rm =TRUE),
    value = 1
    ) %>% 
  dplyr::select(
    Species, value, FlightDuration, Voltinism, 
    WS, FW, NumberOfHostplantAccounts
    ) %>% 
  pivot_wider(
    names_from = Voltinism, 
    values_from = value,
    values_fn = list(value = max)
    ) %>% 
  dplyr::rename(hostplant = NumberOfHostplantAccounts) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(across(where(is.numeric), mean)) %>% 
  dplyr::mutate(
    voltinism = case_when(
      M == 1 ~ "Multivoltine",
      B == 1 ~ "Bivoltine",
      U == 1 ~ "Univoltine")
    ) %>% 
  dplyr::select(-M:-U)



# data frame for analysis
butterfly <- read_csv("data/butterfly/df_butterfly.csv")
butterfly_sci <- read_csv("data/butterfly/butterfly_sciname.csv")

df_butterfly <- butterfly %>% 
  dplyr::mutate(
    time = if_else(str_detect(site, "過去"), "past", "now"),
    ) %>% 
  dplyr::select(site, time, everything()) %>% 
  pivot_longer(cols = -c(site, time), names_to = "species") %>% 
  dplyr::mutate(
    site = str_sub(site, end = -3),
    species = recode(
      species,
      "ギンイチモンジ" = "ギンイチモンジセセリ",
      "スジグロチョウ" = "スジグロシロチョウ",
      "キタキチョウ" = "キチョウ",
      "キマダラヒカゲ" = "サトキマダラヒカゲ",
      "ナミアゲハ" = "アゲハチョウ",
      "ナミヒカゲ" = "ヒカゲチョウ",
      "アゲハ" = "アゲハチョウ"
      )
    ) %>%
  dplyr::filter(species != "オオスカシバ") %>% 
  left_join(butterfly_sci, by = "species") %>% 
  dplyr::mutate(
    sci_name = recode(
      sci_name,
      "Everes argiades" = "Cupido argiades",
      "Cynthia cardui" = "Vanessa cardui",
      "Narathura bazalus" = "Narathura bazalus",
      "Damora sagana" = "Argynnis sagana"),
    
    exotic = ifelse(species == "アカボシゴマダラ", "Exotic","Native")
    ) %>% 
  left_join(df_trait_butterfly, by = c("sci_name" = "Species")) 
  








# Species richness------------------------------------------------------------
sr_butterfly <-
  df_butterfly %>% 
    dplyr::filter(value == 1) %>% 
    group_by(site, time, exotic) %>% 
    dplyr::summarise(sr = n_distinct(species), .groups = "drop") %>% 
  
    pivot_wider(
      names_from = "exotic", 
      values_from = "sr",
      values_fill = list(sr = 0)
      ) %>% 
    dplyr::mutate(All = Native + Exotic) %>% 
  
    pivot_longer(
      cols = c(All, Native, Exotic), 
      values_to = "sr", 
      names_to = "exotic"
      ) %>% 
    dplyr::mutate(
      exotic = recode_factor(
        exotic, 
        "All" = "All species",
        "Native" = "Native species",
        "Exotic" = "Exotic species"),
      time = recode_factor(
        time,
        "past" = "Past",
        "now"  = "Present")
    ) %>% 
    dplyr::filter(exotic != "Exotic species") %>% 
    ungroup() 
  


## Wilcoxon test for comparing the species richness
wlcx_sr_butterfly_time <-
  sr_butterfly %>%
    nest_by(exotic) %>% 
    mutate(
      wlcx_test = map(data, ~wilcox.exact(sr ~ time, data =., paired = TRUE)),
      wlcx_summary = map(wlcx_test, ~tidy(.))
      )



## P-value of wilcoxon test for the boxplot 
p_val_wlcx_sr_butterfly <- 
  wlcx_sr_butterfly_time %>% 
    unnest(wlcx_summary) %>% 
    dplyr::select(exotic, p.value) %>% 
    dplyr::mutate(
      char = "p = ",
      p.value = round(p.value, 3)
      ) %>% 
    unite(col = char_pval, char, p.value, remove = F, sep = "") 


 

## Boxplot
box_sr_butterfly <- 
  ggplot(data = sr_butterfly, aes(x = time, y = sr)) +
  geom_boxplot(aes(fill = time)) + 
  geom_point(color = "grey50", size = 0.5, alpha = 0.5) +
  geom_line(
    aes(group = interaction(site)), 
    color = "grey50", linewidth = 0.3, alpha = 0.5) +
  facet_wrap(~exotic, ncol = 2) +
  geom_signif(
    data = p_val_wlcx_sr_butterfly,
    aes(y_position = c(58, 57), 
        xmin = c(1, 1), xmax = c(2, 2),
        annotations = char_pval),
    size = 0.2, textsize = 2, manual = TRUE) +
  labs(
    title = "Butterfly",
    x = "Time",
    y = "Species richness"
    ) +
  theme_bw(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank()
    )

box_sr_butterfly


# For save
#ggsave(box_sr_butterfly, file = "output/box_sr_butterfly.png", width = 60, height = 50, units = "mm", dpi = 500)









# Lost species ----------------------------------------------------------------

# data frame
df_butterfly_lost <- 
  df_butterfly %>% 
    pivot_wider(
      names_from = time, 
      values_from = value,
      values_fn = list(value = max)
      ) %>% 
    dplyr::mutate(lost = if_else(past == 1 & now == 0, 1, 0)) %>% 
    dplyr::left_join(env, by = "site") %>% 
    dplyr::filter(past == 1) %>% 
    dplyr::mutate(across(where(is.character), as.factor)) 


# number of lost species
df_butterfly_lost %>% 
  dplyr::group_by(site) %>% 
  dplyr::filter(lost == 1) %>% 
  dplyr::summarise(n = n_distinct(species)) %>% 
  na.omit() %>% 
  dplyr::summarise(n = mean(n)) 



# GLM
glm_butterfly_lost <-
  glm(lost ~ FlightDuration + hostplant + WS + year + area + green_rate, 
      family = "binomial",  data = df_butterfly_lost)

summary(glm_butterfly_lost)



# Partial r squared for GLM
rsq_glm_butterfly_lost <- rsq.partial(glm_butterfly_lost, adj = TRUE) 


# Table for result of GLM
tb_glm_butterfly_lost <- 
  glm_butterfly_lost %>% 
    tidy() %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(
      bind_rows(rsq_glm_butterfly_lost[3]),
      across(where(is.numeric),
             ~ifelse(. > -0.001 & . < 0.001,
                      formatC(., digits = 3, format = "e"),
                      formatC(., digits = 3, format = "fg"))
      )) %>% 
    dplyr::rename(
      "Explanatory variables" = term,
      Estimate = estimate,
      Std.error = std.error,
      Statistic = statistic
      ) 

# write.csv(tb_glm_butterfly_lost, "output/glm_lost_butterfly.csv")




# Colonized species -----------------------------------------------------------

# data frame
df_butterfly_colo <- 
  df_butterfly %>% 
    pivot_wider(
      names_from = time, 
      values_from = value,
      values_fn = list(value = max)
      ) %>% 
    dplyr::mutate(colo = if_else(past == 0 & now == 1, 1, 0)) %>% 
    dplyr::left_join(env, by = "site") %>% 
    dplyr::filter(now == 1) %>% 
    dplyr::mutate(
      across(where(is.character), as.factor),
      
      # scaling for explanatory variables
      across(c(FlightDuration:hostplant, year:green_rate), scale)
      )

# Number of colonized species
df_butterfly_colo %>% 
  dplyr::group_by(site) %>% 
  dplyr::filter(colo == 1) %>% 
  dplyr::summarise(n = n_distinct(species)) %>% 
  dplyr::summarise(n = mean(n)) 


# GLM
glm_butterfly_colo <-
  glm(colo ~ FlightDuration + WS + hostplant + year + area + green_rate, 
      family = "binomial",  data = df_butterfly_colo)

summary(glm_butterfly_colo)


# Partial r squared for GLM
rsq_glm_butterfly_colo <- rsq.partial(glm_butterfly_colo, adj = TRUE) 


# Table for result of GLM
tb_glm_butterfly_colo <- 
  glm_butterfly_colo %>% 
    tidy() %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(
      bind_rows(rsq_glm_butterfly_colo[3]),
      
      # round
      across(where(is.numeric),
             ~ifelse(. > -0.001 & . < 0.001,
                     formatC(., digits = 3, format = "e"),
                     formatC(., digits = 3, format = "fg")))
      ) %>% 
    dplyr::rename(
      "Explanatory variables" = term,
      Estimate = estimate,
      Std.error = std.error,
      Statistic = statistic
      ) 

# write.csv(tb_glm_butterfly_colo, "output/glm_colo_butterfly.csv")





# Species composition  --------------------------------------------------------

# data frame for species composition
df_comp_butterfly <- df_butterfly %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::select(site, time, exotic, species, value) %>% 
  pivot_wider(
    names_from = species, 
    values_from = value,
    values_fill = 0
    )

  


comp_butterfly <- df_comp_butterfly %>% 
  dplyr::group_by(site, time) %>% 
  dplyr::summarise(across(where(is.numeric), sum), .groups = "drop") %>% 
  dplyr::mutate(exotic = "All") %>% 
  dplyr::select(site, time, exotic, everything()) %>% 
  dplyr::bind_rows(df_comp_butterfly) %>% 
  nest_by(exotic) %>%
  dplyr::filter(exotic != "Exotic") %>% 
  ungroup() %>% 
  dplyr::mutate(
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
    
    
    # p-value from permanova
    pval = map(permanova, ~as_tibble(.) %>% dplyr::select("Pr(>F)")),
    
    
    exotic = recode_factor(
      exotic, 
      "All" = "All species",
      "Native" = "Native species")
    
  ) 


df_label_butterfly <-
  comp_butterfly %>% 
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





# Plot of NMDS
p_nmds_butterfly <- 
  comp_butterfly %>% 
    dplyr::select(exotic, nmds_score) %>% 
    unnest(nmds_score) %>% 
    dplyr::mutate(
      time = rep(c("Past", "Present"), 14),
      time = factor(time, levels = c("Past", "Present"))
    ) %>% 
    
    ggplot(aes(x = NMDS1, y = NMDS2, color = time)) +
    geom_point() +
    stat_ellipse(
      aes(group = time, fill = time), 
      alpha = 0.1,
      geom = "polygon") +
    
    facet_wrap(. ~ exotic) +
    
    # Stress value
    geom_text(
      data = df_label_butterfly, aes(label = stress), 
      x = 0.35, y = -0.45, size = 2, color = "grey50", hjust = 0) +
    
    # permanova
    geom_text(
      data = df_label_butterfly, aes(label = permanova), 
      x = 0.35, y = -0.5, hjust = 0, size = 2, color = "grey50") +
    
    theme_bw(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.13, 0.9),
      legend.title = element_blank(),
      legend.background = element_blank(),
      strip.background = element_blank()
      )


p_nmds_butterfly

  
  
  
  
  
# save
ggsave(p_nmds_butterfly, file = "output/plot_nmds_butterfly.png", width = 90, height = 90, units = "mm", dpi = 500)















