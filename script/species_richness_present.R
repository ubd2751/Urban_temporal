
# Species richness at present ##########################

# Package ----------------
pacman::p_load(
  tidyverse,  # data management
  lme4, lmerTest, broom, multcomp, #GLM
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  vegan, # composition
  exactRankTests  # wilcoxon test
)


# For read_csv
options(
  readr.num_columns = 0L,
  readr.show_col_types = FALSE,
  readr.show_progress = FALSE
)


# Estimate a species richness ---------------------------------

## Function of estimation for species richness 
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


## Estimate a species richness 
sr_present_plant <- df_plant %>% 
    dplyr::filter(time == "now") %>% 
    est_sr()

sr_present_bird <- df_bird %>% 
  dplyr::filter(time == "now") %>% 
  est_sr()

sr_present_butterfly <- df_butterfly %>% 
  dplyr::filter(time == "now") %>% 
  est_sr()


## Combine data frame for species richness at each taxa
sr_present <- 
  bind_rows(sr_present_plant, sr_present_bird, sr_present_butterfly) %>% 
    dplyr::mutate(
      taxa = rep(c("Plant", "Bird", "Butterfly"),
                  c(nrow(sr_present_plant), 
                    nrow(sr_present_bird),
                    nrow(sr_present_butterfly))
                  ),
      taxa = factor(taxa, levels = c("Plant", "Bird","Butterfly"))
    ) %>% 
  left_join(env, by = "site")



# GLM --------------------

## Model
glm_sr_present <- sr_present %>% 
  group_nest(taxa, exotic) %>% 
  dplyr::mutate(
    model = map(data, ~glm(sr ~ green_rate + year + offset(area), 
                           family = "poisson", data=.)),
    summary = map(model, ~tidy(.)),
    predict = map(model, ~ggpredict(., terms = "green_rate"))
    ) 
  



## Table for result of GLM
tb_glm_sp_present <- glm_sr_present %>% 
  dplyr::select(-data, -model, -predict) %>% 
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


# write.csv(tb_glm_sp_present, "./output/table_glm_sr_presentcsv")



## Plot for GLM

  ggplot() + 
  geom_point(
    data = sr_present %>% dplyr::filter(taxa == "Plant"),
    aes(green_rate, sr, color = exotic, fill = exotic)
  ) 
  geom_smooth(
    data = glm_sr_present %>% unnest(predict) %>% 
      dplyr::filter(taxa == "Plant"),
    aes(x, predicted, color = exotic, fill = exotic)
  ) +
  geom_ribbon(
    data = glm_sr_present %>% unnest(predict) %>% 
      dplyr::filter(taxa == "Plant"),
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




