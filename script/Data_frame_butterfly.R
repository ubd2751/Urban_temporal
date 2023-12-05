
# Data-frame for Butterfly  ###########################################

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



## Trait -------------------------------------------------------
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



## Data frame for butterfly ----
butterfly <- read_csv("data/butterfly/df_butterfly.csv")
butterfly_sci <- read_csv("data/butterfly/butterfly_sciname.csv")

df_butterfly <- butterfly %>% 
  dplyr::mutate(
    time = if_else(str_detect(site, "過去"), "past", "now")
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
  group_by(site, time, species) %>% 
  dplyr::summarise(value = sum(value), .groups = "drop") %>% 
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
  
  # combining a trait data
  left_join(df_trait_butterfly, by = c("sci_name" = "Species")) 



