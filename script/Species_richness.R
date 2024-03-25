
# Species richness ============================================================


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



## Estimate a species richness--------------

# Function of estimation for species richness 
est_sr <- function(x) {
  x %>% 
    dplyr::filter(value == 1 & (exotic == "Native" | exotic == "Exotic")) %>% 
    dplyr::group_by(site, time, exotic) %>% 
    dplyr::summarise(sr = n_distinct(species), .groups = "drop") %>% 
    pivot_wider(
      names_from = "exotic", 
      values_from = "sr",
      values_fill = list(sr = 0)
      ) %>% 
    dplyr::mutate(All = Native + Exotic) %>% 
    pivot_longer(c(All, Native, Exotic), 
                 values_to = "sr", names_to = "exotic") %>% 
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
      ) 
}

# Estimate a species richness
sr_plant <- est_sr(df_plant)
sr_bird <- est_sr(df_bird)
sr_butterfly <- est_sr(df_butterfly)




# Wilcoxon test -------------------------------------

# Function of wilcocon test
test_wicoxon <- function(x) {
  
  # For compare a species richness
  wlcx <- x %>%
    dplyr::group_nest(exotic) %>% 
    dplyr::mutate(
      test = map(data, ~wilcox.exact(sr ~ time, data =., paired = TRUE)),
      summary = map(test, ~tidy(.))
      )
  
  
  # Exclude a P-value for boxplots
  wlcx %>% 
    unnest(summary) %>% 
    dplyr::select(exotic, p.value) %>% 
    dplyr::mutate(
      char = "p = ",
      p.value = round(p.value, 3)
      ) %>%
    unite(col = char_pval, char, p.value, remove = F, sep = "") 
  } 

# Compare a species richness between time
wlcx_sr_plant <- test_wicoxon(sr_plant) 

wlcx_sr_bird  <- 
  test_wicoxon(sr_bird) %>% 
  dplyr::filter(exotic != "Exotic species")

wlcx_sr_butterfy <- 
  test_wicoxon(sr_butterfly) %>% 
  dplyr::filter(exotic != "Exotic species")










# Boxplot ---------------------------------------------------------------

## Plant
box_sr_plant <- 
  ggplot(data = sr_plant, aes(x = time, y = sr)) +
    geom_boxplot(
      aes(color = time), outlier.colour = NA,
      width = 0.8, size = 0.5
      ) +
    facet_wrap(~exotic, ncol = 3) +
    geom_signif(
      data = wlcx_sr_plant,
      aes(y_position = c(870, 830, 220), 
          xmin = c(1, 1, 1), xmax = c(2, 2, 2),
          annotations = char_pval),
      size = 0.1, textsize = 2.5, manual = TRUE, 
      tip_length = 0.05, vjust = -0.5) +
    scale_y_continuous(limits = c(0, 950)) +
    labs(
      x = "Time",
      y = "Species richness",
      fill = "Time",
      title = "Plant"
      ) +
    theme_classic(base_size = 8) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      #strip.background = element_blank(),
      strip.text = element_text(size = 8),
      axis.title.x = element_blank()
      )




## Bird
box_sr_bird <- 
  sr_bird %>% 
    dplyr::filter(exotic != "Exotic species") %>% 
    ggplot(aes(x = time, y = sr)) +
      geom_boxplot(
        aes(color = time), outlier.colour = NA,
        width = 0.8, size = 0.5
        ) + 
      facet_wrap(~exotic, ncol = 3) +
      geom_signif(
        data = wlcx_sr_bird ,
        aes(y_position = c(150, 145), 
            xmin = c(1, 1), xmax = c(2, 2),
            annotations = char_pval),
        size = 0.1, textsize = 2.5, manual = TRUE, 
        tip_length = 0.05, vjust = -0.5
        ) +
      scale_y_continuous(limits = c(0, 160)) +
      labs(title = "Bird", y = "Species richness") +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      theme_classic(base_size = 8) +
      theme(
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank()
        )



## Butterfly
box_sr_butterfly <- 
  sr_butterfly %>%
    dplyr::filter(exotic != "Exotic species") %>% 
    ggplot(aes(x = time, y = sr)) +
      geom_boxplot(
        aes(color = time), outlier.colour = NA,
        width = 0.8, size = 0.5
        ) + 
      facet_wrap(~exotic, ncol = 2) +
      geom_signif(
        data = wlcx_sr_butterfy,
        aes(y_position = c(60, 60), 
            xmin = c(1, 1), xmax = c(2, 2),
            annotations = char_pval),
        size = 0.1, textsize = 2.5, manual = TRUE, 
        tip_length = 0.05, vjust = -0.5) +
      scale_y_continuous(limits = c(10, 70)) +
      labs(
        title = "Butterfly",
        fill = "Time",
        x = "Time",
        y = "Species richness"
      ) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      theme_classic(base_size = 8) +
      theme(
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()
        )






## Combining
layout <- "
AAAAAA
BBBB##
CCCC##
"

box_sr_time <- 
  (box_sr_plant / box_sr_bird / box_sr_butterfly) +
  plot_layout(design = layout) 
  
box_sr_time

# For save
ggsave(box_sr_time, file = "output/box_sr_time.png",
       width = 140, height = 140, units = "mm", dpi = 600)








## GLM ------------------------------------------------------------

### Model -----------
# Plant
glm_sr_plant <- sr_plant %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(tem_sr = Present - Past) %>% 
  
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(tem_sr ~ year + green_rate + log(area), data = .)),
    summary = map(model, ~tidy(.)),
    predict = map(model, ~ggpredict(., terms = "year"))
    )


# Bird
glm_sr_bird <- sr_bird %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(tem_sr = Present - Past) %>% 
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(tem_sr ~ year + green_rate + log(area), data = .)),
    summary = map(model, ~tidy(.))
    )


# Butterfly
glm_sr_butterfly <- sr_butterfly %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(tem_sr = Present - Past) %>% 
  
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(tem_sr ~ year + green_rate + log(area), data = .)),
    summary = map(model, ~tidy(.))
  )




### Table for GLM -----
tidy_table <- function(x) {
  
  x %>% 
    dplyr::select(-data, -model) %>% 
    tidyr::unnest(summary) %>% 
    dplyr::filter(term != "(Intercept)") %>% 
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
}

tb_glm_sr <- bind_rows(
  tidy_table(glm_sr_plant) %>% dplyr::mutate(species = "Plant"),
  tidy_table(glm_sr_bird) %>% dplyr::mutate(species = "Bird"),
  tidy_table(glm_sr_butterfly) %>% dplyr::mutate(species = "Butterfly")
  ) %>% 
  dplyr::select(species, everything(), -predict)


# write.csv(tb_glm_sr, "./output/table_glm_sr.csv")





### Plot for GLM -----

# Plant
plot_glm_tempSR_plant <- 
  ggplot() + 
  geom_point(
    data = glm_sr_plant %>% unnest(data)  %>% 
      dplyr::filter(exotic != "Exotic species"),
    aes(year, tem_sr, color = exotic, fill = exotic)
    ) +
  geom_smooth(
    data = glm_sr_plant %>% unnest(predict) %>% 
      dplyr::filter(exotic != "Exotic species"),
    aes(x, predicted, color = exotic, fill = exotic)
    ) +
  geom_ribbon(
    data = glm_sr_plant %>% unnest(predict) %>% 
      dplyr::filter(exotic != "Exotic species"),
    aes(x, ymin = conf.low, ymax = conf.high, fill = exotic),
    alpha = 0.2
    ) +
  
  scale_color_simpsons() +
  scale_fill_simpsons() +
  theme_classic() +
  labs(
    title = "Plant",
    y = "Temporal changes in species richness"
    ) +
  theme(
    legend.position = c(0.3, 0.2),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(5, "mm"),
    axis.title.x = element_blank()
  )



# Bird
plot_glm_tempSR_bird <- 
  ggplot() + 
  geom_point(
    data = glm_sr_bird %>% unnest(data) %>% 
      dplyr::filter(exotic != "Exotic species"),
    aes(year, tem_sr, color = exotic, fill = exotic)
  ) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
  theme_classic() +
  labs(
    title = "Bird",
    x = "Years between surveys") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )


# Butterfly
plot_glm_tempSR_butterfly <- 
  ggplot() + 
  geom_point(
    data = glm_sr_butterfly %>% unnest(data) %>% 
      dplyr::filter(exotic != "Exotic species"),
    aes(year, tem_sr, color = exotic, fill = exotic)
  ) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
  theme_classic() +
  labs(title = "Butterfly") +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

  

plot_glm_tempSR <- 
  plot_glm_tempSR_plant + plot_glm_tempSR_bird + plot_glm_tempSR_butterfly



# For save
ggsave(plot_glm_tempSR, file = "output/plot_glm_tempSR.png",
       width = 180, height = 80, units = "mm", dpi = 600)




