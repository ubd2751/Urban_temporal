

# Plant
df_plant %>% 
  dplyr::select(-height:-la, -species) %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::mutate(
    site = recode(
      site,
      "自然教育園" = "SK",
      "多摩森林科学園" = "TS",
      "東京農業大学" = "TA",
      "横浜国立大学" = "YU",
      "横浜自然観察の森" = "YS",
      "新治市民の森" = "NC",
      "馬堀自然教育園" = "UN",
      "藤沢市1" = "F1",
      "藤沢市2" = "F2",
      "藤沢市3" = "F3",
      "大和市1" = "YA",
      "大町公園自然観察園" = "OS",
      "流山市1" = "N1",
      "流山市2" = "N2",
      "流山市3" = "N3"
      )
    ) %>% 
  write.csv("./output/df_plant_output.csv")





# Bird
df_bird %>% 
  dplyr::select(site, time, sciname, value, exotic, species,
                Beak.Length_Culmen, "Hand-Wing.Index", Mass) %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::mutate(
    site = recode(
      site,
      "自然教育園" = "SK",
      "多摩森林科学園" = "TS",
      "東京農業大学" = "TA",
      "横浜国立大学" = "YU",
      "横浜自然観察の森" = "YS",
      "新治市民の森" = "NC",
      "馬堀自然教育園" = "UN",
      "藤沢市1" = "F1",
      "藤沢市2" = "F2",
      "藤沢市3" = "F3",
      "大和市1" = "YA",
      "大町公園自然観察園" = "OS",
      "流山市1" = "N1",
      "流山市2" = "N2",
      "流山市3" = "N3"
      ),
    sciname = if_else(species == "アイガモ", 
                      "Anas platyrhynchos var.domesticus",
                      sciname)
    ) %>% 
  dplyr::select(-species) %>% 
  write.csv("./output/df_bird_output.csv")




# Butterfly
df_butterfly %>% 
  dplyr::select(site, time, sci_name, value, exotic, 
                FlightDuration, WS, hostplant) %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::mutate(
    site = recode(
      site,
      "自然教育園" = "SK",
      "多摩森林科学園" = "TS",
      "東京農業大学" = "TA",
      "横浜国立大学" = "YU",
      "横浜自然観察の森" = "YS",
      "新治市民の森" = "NC",
      "馬堀自然教育園" = "UN",
      "藤沢市1" = "F1",
      "藤沢市2" = "F2",
      "藤沢市3" = "F3",
      "大和市1" = "YA",
      "大町公園自然観察園" = "OS",
      "流山市1" = "N1",
      "流山市2" = "N2",
      "流山市3" = "N3"
    ) 
  ) %>% 
  write.csv("./output/df_butterfly_output.csv")



# Environment
env %>% 
  ungroup() %>% 
  dplyr::mutate(
    site = recode(
      site,
      "自然教育園" = "SK",
      "多摩森林科学園" = "TS",
      "東京農業大学" = "TA",
      "横浜国立大学" = "YU",
      "横浜自然観察の森" = "YS",
      "新治市民の森" = "NC",
      "馬堀自然教育園" = "UN",
      "藤沢市1" = "F1",
      "藤沢市2" = "F2",
      "藤沢市3" = "F3",
      "大和市1" = "YA",
      "大町公園自然観察園" = "OS",
      "流山市1" = "N1",
      "流山市2" = "N2",
      "流山市3" = "N3"
      )
  ) %>% 
  write.csv("./output/df_env_output.csv")

    