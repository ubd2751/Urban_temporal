
# Partitioning of beta diversity

# Package ----------------
pacman::p_load(
  tidyverse,  # data management
  lme4, lmerTest, broom, multcomp, #GLM
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  vegan, iNEXT, betapart, # composition
  exactRankTests  # wilcoxon test
)



# Function of estimate for partitioning of beta diversity
est_betapart <- function(x) {
  
  x %>%
    dplyr::filter(value == 1) %>% 
    dplyr::select(site, time, species, value) %>% 
    pivot_wider(
      names_from = species, 
      values_from = value,
      values_fill = 0
      ) %>%  
    dplyr::select(-time) %>% 
    dplyr::group_nest(site) %>% 
    dplyr::mutate(
      betapart = map(data, ~beta.pair(., index.family="jac")),
      tidy_beta = map(betapart, ~bind_rows(.))
    ) %>% 
    
    dplyr::select(site, tidy_beta) %>% 
    tidyr::unnest(tidy_beta) 
}


# Partitioning a beta diversity 
# Plant
df_betapart_plant <- bind_rows(
  
  # all species
  est_betapart(df_plant),
  
  # native species
  df_plant %>% 
    dplyr::filter(exotic == "Native") %>% 
    est_betapart(),
  
  # exotic species
  df_plant %>% 
    dplyr::filter(exotic == "Exotic") %>% 
    est_betapart()
  
  ) %>% 
  dplyr::mutate(
    species = "Plant",
    exotic = rep(c("All","Native","Exotic"), each = 15),
    across(starts_with("beta"), ~as.numeric(.))
  ) %>% 
  left_join(env, by = "site")
  
  


# Bird
df_betapart_bird <- bind_rows(
  
  # All species
  est_betapart(df_bird),
  
  # Native species
  df_bird %>% 
    dplyr::filter(exotic == "Native") %>% 
    est_betapart()
  
  ) %>% 
  dplyr::mutate(
    species = "Bird",
    exotic = rep(c("All","Native"), each = 7),
    across(starts_with("beta"), ~as.numeric(.))
  ) %>% 
  left_join(env, by = "site")





# Butterfly
df_betapart_butterfly <- bind_rows(
  
  # All species
  est_betapart(df_butterfly),
  
  # Native species
  df_butterfly %>% 
    dplyr::filter(exotic == "Native") %>% 
    est_betapart()
  
  ) %>% 
  dplyr::mutate(
    species = "Butterfly",
    exotic = rep(c("All","Native"), each = 7),
    across(starts_with("beta"), ~as.numeric(.))
  ) %>% 
  left_join(env, by = "site")


df_betapart <- bind_rows(
  df_betapart_plant, df_betapart_bird, df_betapart_butterfly) %>% 
  pivot_longer(cols = starts_with("beta")) 




# Boxplot ----------------------

## Wilcoxon test -------------------------------------

wlcx_betapart <- df_betapart %>% 
  dplyr::filter(name != "beta.sim") %>% 
  dplyr::group_nest(species, exotic) %>% 
  dplyr::mutate(
    test = map(data, ~wilcox.exact(value ~ name, data =., paired = TRUE)),
    summary = map(test, ~tidy(.))
    ) %>% 
  
  unnest(summary) %>% 
  dplyr::select(species, exotic, p.value) %>% 
  dplyr::mutate(
    char = "p = ",
    p.value = round(p.value, 3)
    ) 
  unite(col = char_pval, char, p.value, remove = F, sep = "") 
  




box_betepart <- df_betapart %>% 
  dplyr::filter(name != "beta.sor") %>% 
  dplyr::mutate(
    species = recode_factor(
      species,
      "Plant" = "Plant",
      "Bird" = "Bird",
      "Butterfly" = "Butterfly"
      ),
    exotic = recode_factor(
      exotic,
      "All" = "All species",
      "Native" = "Native species",
      "Exotic" = "Exotic species"
      ),
    name = recode_factor(
      name,
      "beta.sim" = "Turnover",
      "beta.sne" = "Nestedness"
      )
    ) %>% 
  ggplot(aes(x = name, y = value, color = name)) +
    geom_boxplot() +
    facet_grid(exotic ~ species, switch = "y") +
    
    geom_signif(
      test = "wilcox.test", 
      comparisons = list(c("Turnover", "Nestedness")),
      map_signif_level = T,
      textsize = 2,
      size = 0.2,
      color = "grey60"
      ) +
    
    labs(
      x = "Beta diversity component",
      y = "Beta diversity"
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(breaks = seq(0, 0.8, 0.2), limits = c(0, 0.7))+
    theme_bw(base_size = 6) +
    theme(
      legend.position = "none",
      strip.placement = "outsite",
      strip.background = element_blank(),
      panel.grid = element_blank()
      )

box_betepart

ggsave(box_betepart, file = "output/box_betapart.png", 
       width = 90, height = 90, units = "mm", dpi = 500)





# GLM ---------------------------

glm_betapart <- df_betapart %>% 
  dplyr::group_nest(species, exotic, name) %>% 
  dplyr::mutate(
    model = map(data, ~lm(value ~ year + area + green_rate, data = .)),
    summary = map(model, ~tidy(.)),
    predict = map(model, ~ggpredict(., terms = "year")),
    
    species = factor(species, levels = c("Plant", "Bird", "Butterfly")),
    exotic = factor(exotic, levels = c("All", "Native", "Exotic")),
    beta =  fct_recode(name, 
                       "Beta diversity" = "beta.jac",
                       "Turnover" = "beta.jtu",
                       "Nestedness" = "beta.jne")
    ) 




## Table for GLM
tb_glm_betapart <- glm_betapart %>% 
  dplyr::select(-data, -model, -predict) %>% 
  tidyr::unnest(summary) %>% 
  dplyr::filter(term != "(Intercept)" & name == "beta.jac") %>% 
  dplyr::mutate(
      # round
      across(where(is.numeric),
             ~ if_else(abs(.) <= 0.001,
                       formatC(., digits = 3, format = "e"),
                       formatC(., digits = 3, format = "fg")))
    ) %>% 
    dplyr::rename(
      "Explanatory variables" = term,
      Estimate = estimate,
      Std.error = std.error,
      Statistic = statistic
      ) 


#write.csv(tb_glm_betapart, "./output/table_glm_betapart.csv")




## Plot for GLM of beta diversity

plot_glm_tempbeta <- 
  ggplot() + 
  geom_point(
    data = glm_betapart %>% 
      unnest(data) %>% 
      dplyr::filter(exotic != "Exotic" & name == "beta.jac"),
    aes(year, value, color = exotic, fill = exotic)
    ) +
  geom_line(
    data = glm_betapart %>% 
      dplyr::filter(
          species == "Bird" & exotic != "Exotic" & name == "beta.jac"
          ) %>% 
      unnest(predict),
    aes(x, predicted, color = exotic), size = 1
    ) +
  geom_ribbon(
    data = glm_betapart %>% 
      dplyr::filter(
        species == "Bird" & exotic != "Exotic" & name == "beta.jac"
        ) %>% 
      unnest(predict),
    aes(x, ymin = conf.low, ymax = conf.high, fill = exotic),
    alpha = 0.1
    ) +
  facet_grid(. ~ species, switch = "y") +
  scale_y_continuous(breaks = seq(0, 0.9, length = 4))+
  scale_color_npg(labels = c(All = "All species",
                             Native = "Native species")) +
  scale_fill_npg(labels = c(All = "All species",
                            Native = "Native species")) +

  labs(
    x = "Years between surveys",
    y = "Temporal beta diversity"
    ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(5, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid = element_blank()
    )


ggsave(plot_glm_tempbeta, file = "output/plot_glm_tempbeta.png",
       width = 170, height = 80, units = "mm", dpi = 600)

