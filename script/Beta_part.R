
# Partitioning of beta diversity

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
      betapart = map(data, ~beta.pair(.)),
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

df_betapart %>% 
  pivot_longer(cols = starts_with("beta")) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
    geom_boxplot() +
    facet_grid(species ~ exotic)



# GLM ---------------------------

glm_betapart <- df_betapart %>% 
  #dplyr::filter(name != "beta.sim") %>% 
  
  dplyr::group_nest(species, exotic, name) %>% 
  dplyr::mutate(
    model = map(data, ~lm(value ~ year + area + green_rate, data = .)),
    summary = map(model, ~tidy(.))
    ) 



## Table for GLM
tb_glm_betapart <- glm_betapart %>% 
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


#write.csv(tb_glm_betapart, "./output/table_glm_betapart.csv")





