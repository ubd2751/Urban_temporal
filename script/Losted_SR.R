


# Package 
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




# Plant ------------------------------------------------------------------

## SR at growh form --------------------------------

## Calculate losted species richness
df_plant_lost <- df_plant %>% 
  pivot_wider(names_from = time, values_from = value) %>% 
  dplyr::mutate(lost = if_else(past == 1 & now == 0, 1, 0)) %>% 
  dplyr::filter(past == 1) %>% 
  dplyr::left_join(env, by = "site")


## GLM for lost plant
glm_lost_plant <- 
  df_plant_lost %>% 
  dplyr::filter(exotic == "Native") %>% 
  glm(lost ~ year + log(area) + green_rate +
        growth + disper + 
        year:growth + year:disper,
      family = "binomial", data = .) 


    




# Birds ------------------------------------------------------------------

## Calculate losted species richness and total traits
sr_lost_bird <- df_bird %>% 
  pivot_wider(names_from = time, values_from = value) %>% 
  dplyr::mutate(lost = if_else(past == 1 & now == 0, 1, 0)) %>% 
  dplyr::filter(past == 1) %>% 
  dplyr::left_join(env, by = "site")


  
## GLM for lost bird species
glm_lost_bird <- sr_lost_bird %>% 
  dplyr::filter(exotic == "Native") %>% 
  rename(HWI = "Hand-Wing.Index",
         Beak = "Beak.Length_Culmen") %>% 
  glm(lost ~ year + log(area) + green_rate +
        Mass + Beak + HWI +
        year:Mass + year:Beak + year:HWI,
      family = "binomial", data = .) 

  
  
  
  
# Butterfly ------------------------------------------------------------------
  
## Calculate losted species richness and total traits
df_lost_butterfly <- df_butterfly %>% 
  pivot_wider(names_from = time, values_from = value) %>% 
  dplyr::mutate(lost = if_else(past == 1 & now == 0, 1, 0)) %>% 
  dplyr::filter(past == 1) %>% 
  dplyr::left_join(env, by = "site")

  
## GLM for lost species
glm_lost_butterfly <- df_lost_butterfly %>% 
  glm(lost ~ year + log(area) + green_rate +
        FlightDuration + FW + hostplant + 
        year:FlightDuration + year:FW + year:hostplant,
      family = "binomial", data = .) 
    






# Table for GLM -----
tidy_table_lost <- function(x) {
  
  x %>% 
    tidy() %>%   
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

tb_glm_lost <- 
  bind_rows(
  tidy_table_lost(glm_lost_plant) %>% dplyr::mutate(species = "Plant"),
  tidy_table_lost(glm_lost_bird) %>% dplyr::mutate(species = "Bird"),
  tidy_table_lost(glm_lost_butterfly) %>% dplyr::mutate(species = "Butterfly")
  ) %>% 
  dplyr::select(species, "Explanatory variables", Estimate, p.value)



write.csv(tb_glm_lost, "./output/tb_glm_lost.csv")


