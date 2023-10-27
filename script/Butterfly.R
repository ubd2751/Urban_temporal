
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


# load a data frame
butterfly <- read_csv("data/butterfly/df_butterfly.csv")

butterfly_sci <- read_csv("data/butterfly/butterfly_sciname.csv")



# organization of the trait data
butterfly_trait <- read_csv("data/butterfly/butterfly_trait.csv")

#df_trait_butterfly <-
  butterfly_trait %>% 
  dplyr::select(
    -Family, -Genus, -verbatimSpecies,
    -WS_L_Fem:-WS_U_Mal, -FW_L_Fem:-FW_U_Mal, -Jan:-Dec,
    -NumberOfHostplantFamilies:-EqualHostplantFamily, -DateCreated
    ) %>% 
  dplyr::filter(!is.na(OvipositionStyle))
  dplyr::mutate(
    WS = mean(c(WS_L, WS_U)),
    FW = mean(c(FW_L, FW_U))
    ) 
  




# data frmae for butterfly
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
  left_join(butterfly_trait, by = c("sci_name" = "Species")) %>% 
  dplyr::select(-Jan:-Dec, -DateCreated)
  


df_butterfly %>% 
  summary()
  



