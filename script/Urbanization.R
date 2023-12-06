
# Urbanization rate #####################

# Package
pacman::p_load(
  tidyverse,       # data management
  rtry, austraits  # trait
)

# For read_csv
options(
  readr.num_columns = 0L,
  readr.show_col_types = FALSE,
  readr.show_progress = FALSE
)






# Urbanization rate ----
raw_buffer_LU <- read_csv("data/buffer_LU.csv")

urban_rate <- raw_buffer_LU %>% 
  dplyr::group_by(buffer_size, sitename, DN) %>% 
  dplyr::summarise(area = sum(area)) %>% 
  
  dplyr::group_by(buffer_size, sitename) %>% 
  dplyr::mutate(
    total_area = sum(area),
    rate = area/total_area
  ) %>% 
  dplyr::filter(DN == 2) %>% 
  ungroup() %>% 
  dplyr::select(-DN, -area, -total_area) %>% 
  dplyr::rename(site = sitename) %>% 
  dplyr::mutate(
    site = recode(
      site,
      "fuzisawa1" = "藤沢市1",
      "fuzisawa2" = "藤沢市2",
      "fuzisawa3" = "藤沢市3",
      "hukamisiminnnomori" = "大和市1",
      "maborisizennkyouikuenn" = "馬堀自然教育園",
      "nagareyama1" = "流山市1",
      "nagareyama2" = "流山市2",
      "nagareyama3" = "流山市3",
      "niiharusiminnnomori" = "新治市民の森",
      "oomatisizennkannsatuenn" = "大町公園自然観察園",
      "sizennkyouikuenn" = "自然教育園",
      "tamasinnrinnkagakuenn" = "多摩森林科学園",
      "toukyounougyoudaigakuatugi" = "東京農業大学",
      "yokohamakokuritudaigaku" = "横浜国立大学",
      "yokohamasizennkannsatunomori" = "横浜自然観察の森"
    )
  ) 
