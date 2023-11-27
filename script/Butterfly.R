
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

df_butterfly <- 
  butterfly %>% 
  dplyr::mutate(time = if_else(str_detect(site, "過去"), "past", "now")) %>% 
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
      "Damora sagana" = "Argynnis sagana"
      )
    ) %>% 
  left_join(df_trait_butterfly, by = c("sci_name" = "Species")) %>% 
  dplyr::filter(species != "アカボシゴマダラ")
  








# Species richness------------------------------------------------------------
sr_butterfly <-
  df_butterfly %>% 
  dplyr::filter(value == 1) %>% 
  group_by(site, time) %>% 
  dplyr::summarise(sr = n_distinct(species), .groups = "drop") 
  


## Wilcoxon test for comparing the species richness
wlcx_sr_time_butterfly <-
  wilcox.test(sr ~ time, data = sr_butterfly, paired = TRUE) 



## Boxplot
box_sr_butterfly <- 
  ggplot(data = sr_butterfly, aes(x = time, y = sr)) +
  geom_boxplot(aes(fill = time)) + 
  geom_point(color = "grey50", size = 0.5, alpha = 0.5) +
  geom_line(
    aes(group = interaction(site)), 
    color = "grey50", linewidth = 0.3, alpha = 0.5) +
  geom_signif(
    y_position = 58, xmin = 1, xmax = 2, annotations = "p = 0.297",
    size = 0.2, textsize = 2
    ) +
  labs(
    x = "Time",
    y = "Species richness"
    ) +
  theme_bw(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
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
comp_butterfly <- df_butterfly %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::select(site, time, species, value) %>% 
  pivot_wider(
    names_from = species, 
    values_from = value,
    values_fill = 0
    ) 



# NMDS
nmds_butterfly <- comp_butterfly %>% 
  dplyr::select(-site, -time) %>% 
  vegan::metaMDS(trace = FALSE)
  


# Permanova
permanova_butterfly <- 
  adonis2(comp_butterfly[,-1:-2] ~ comp_butterfly$time, method = "jac")





# Plot of NMDS
p_nmds_butterfly <- 
  nmds_butterfly %>% 
  scores() %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  dplyr::mutate(time = rep(c("Past", "Present"), 7)) %>% 
  
  ggplot(aes(x = NMDS1, y = NMDS2, color = time)) +
  geom_point() +
  stat_ellipse(
    aes(group = time, fill = time), 
    alpha = 0.1,
    geom = "polygon") +
  
  # Stress value
  annotate(
    "text", x = 0.35, y = -0.45, size = 2, color = "grey50", hjust = 0,
    label = paste0("Stress value : ", 
                   format(nmds_butterfly$stress, digits = 3)
                   )
    ) +
  
  # permanova
  annotate(
    "text", x = 0.35, y = -0.5, hjust = 0, size = 2, color = "grey50",
    label = paste0("Permanova : p=", 
                   permanova_butterfly$`Pr(>F)`[1])
    ) +
  
  theme_classic(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.13, 0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
    strip.background = element_blank()
    )


# save
ggsave(p_nmds_butterfly, file = "output/plot_nmds_butterfly.png", width = 90, height = 90, units = "mm", dpi = 500)















