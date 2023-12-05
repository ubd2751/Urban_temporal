
# Data-frame for bird  ###########################################


## Loading data -------------------------------------------------------

# survey data
bird <- read_csv("data/bird_survey.csv")

# scientific name
bird_sciname <- read_csv("data/bird_sciname.csv")

# trait 
#bird_trait <- read_csv("data/bird_trait_avonet.csv")
bird_trait_life   <- read_csv("data/bird_trait_birdlife.csv")
bird_trait_ebird  <- read_csv("data/bird_trait_ebird.csv")
bird_trait_tree   <- read_csv("data/bird_trait_birdtree.csv")
bird_trait_avonet <- read_csv("data/bird_trait_avonet.csv")

# exotic
bird_exotic <- read_csv("data/bird_exotic.csv")

# Environmental variables
env <- read_csv("data/Environment.csv")




## Trait ----------------------------------------------------------
df_bird_trait <- bind_rows(
  
  # birdlife
  bird_trait_life %>% 
    dplyr::select(Species1, Beak.Length_Culmen:Mass) %>% 
    dplyr::rename(sciname = Species1),
    
  # ebird
  bird_trait_ebird %>% 
    dplyr::select(Species2, Beak.Length_Culmen:Mass) %>% 
    dplyr::rename(sciname = Species2),
    
  # birdtree
  bird_trait_tree%>% 
    dplyr::select(Species3, Beak.Length_Culmen:Mass) %>% 
    dplyr::rename(sciname = Species3),
    
  # Avonet
  bird_trait_avonet %>% 
    dplyr::select(Species1_BirdLife, Beak.Length_Culmen:Tail.Length) %>% 
    dplyr::mutate(Mass = NA) %>% 
    dplyr::rename(sciname = Species1_BirdLife)
  
  ) %>% 
  group_by(sciname) %>% 
  dplyr::summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)))





## Combining ---------------------------------------------------------
df_bird <- bird %>% 
  as_tibble() %>% 
  dplyr::mutate(
    time = if_else(str_detect(site, "過去"), "past", "now"),
    site = str_sub(site, end = -3)
    ) %>% 
  dplyr::select(site, time, everything()) %>% 
  pivot_longer(-c(site, time), names_to = "species") %>% 
  
  # combine scientific name + Exotic 
  left_join(
    bird_sciname %>% 
      dplyr::select(species, sciname) %>% 
      dplyr::mutate(exotic = "Native") %>% 
      dplyr::filter(species != "カササギ") %>% 
      bind_rows(bird_exotic), 
    by = "species") %>% 
  
  
  # combine trait data
  left_join(df_bird_trait, by = "sciname") 


