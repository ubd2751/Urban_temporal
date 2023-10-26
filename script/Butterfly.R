
# Package
pacman::p_load(
  tidyverse,  # data management
  lme4, lmerTest, broom, multcomp, #GLM
  ggeffects, patchwork, ggsci, ggsignif,  # ggplot
  vegan # composition
  )


# load a data frame
butterfly <- read_csv("data/butterfly/df_butterfly.csv")
butterfly_trait <- read_csv("data/butterfly/butterfly_trait.csv")
butterfly_sci <- read_csv("data/butterfly/butterfly_sciname.csv")




# data frmae for butterfly
df_butter <- butterfly %>% 
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
  left_join(butterfly_trait, by = c("sci_name" = "Species"))
  
  
  
  



