
# Species richness ============================================================


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

# Estimate a species richnesss
sr_plant <- est_sr(df_plant)
sr_bird <- est_sr(df_bird)
sr_butterfly <- est_sr(df_butterfly)




# Boxplot ------------------------------------------------------------------

## Plant
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
    #geom_signif(
    #  data = p_val_wlcx_sr_time,
    #  aes(y_position = c(870, 830, 220), 
    #      xmin = c(1, 1, 1), xmax = c(2, 2, 2),
    #      annotations = char_pval),
    #  size = 0.1, textsize = 1.3, manual = TRUE, 
    #  tip_length = 0.05, vjust = -0.5) +
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








## GLM ------------------------------------------------------------

# Plant
glm_sr_plant <- sr_plant %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(sr = Past - Present) %>% 
  
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(sr ~ year + area + green_rate, data = .)),
    summary = map(model, ~tidy(.))
    )


# Bird
glm_sr_bird <- sr_bird %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(sr = Past - Present) %>% 
  
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(sr ~ year + area + green_rate, data = .)),
    summary = map(model, ~tidy(.))
  )


# Butterfly
glm_sr_butterfly <- sr_butterfly %>% 
  left_join(env, by = "site") %>% 
  pivot_wider(names_from = time, values_from = sr) %>% 
  dplyr::mutate(sr = Past - Present) %>% 
  
  group_nest(exotic) %>% 
  dplyr::mutate(
    model = map(data, ~ glm(sr ~ year + area + green_rate, data = .)),
    summary = map(model, ~tidy(.))
  )



### Table for GLM
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
  dplyr::select(species, everything())


#write.csv(tb_glm_sr, "./output/table_glm_sr.csv")


