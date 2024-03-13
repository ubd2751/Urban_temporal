
# Data-frame for plant  ###########################################

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





# Trait ------------

## Leaf and seed-----

### Try 

# Plant height
try_height1 <- rtry_import("data/try/try_height1.txt", showOverview = FALSE)
try_height2 <- rtry_import("data/try/try_height2.txt", showOverview = FALSE)

try_height <- bind_rows(try_height1, try_height2) %>% 
  dplyr::filter(UnitName == "m" & !is.na(TraitID)) %>% 
  dplyr::mutate(height = as.numeric(StdValue)) %>% 
  dplyr::group_by(AccSpeciesName) %>% 
  dplyr::summarise(height = mean(height)) 


# Seed mass & Life form
try_seed1   <- rtry_import("data/try/try_seed1.txt", showOverview = FALSE)
try_seed2   <- rtry_import("data/try/try_seed2.txt", showOverview = FALSE)

try_seed <- bind_rows(try_seed1, try_seed2) %>% 
  dplyr::filter(str_detect(TraitName, "Seed")) %>% 
  dplyr::filter(!is.na(StdValue)) %>% 
  dplyr::group_by(AccSpeciesName) %>% 
  dplyr::summarise(seed_mass = mean(StdValue))


# Specific leaf area
try_sla1    <- rtry_import("data/try/try_SLA1.txt", showOverview = FALSE)
try_sla2    <- rtry_import("data/try/try_SLA2.txt", showOverview = FALSE)

try_sla <- bind_rows(try_sla1, try_sla1) %>% 
  dplyr::filter(str_detect(TraitName, "area")) %>% 
  dplyr::filter(!is.na(StdValue)) %>% 
  dplyr::group_by(AccSpeciesName) %>% 
  dplyr::summarise(sla = mean(StdValue))


# Leaf area
try_la1 <- rtry_import("data/try/try_LA1.txt", showOverview = FALSE)
try_la2 <- rtry_import("data/try/try_LA2.txt", showOverview = FALSE)

try_la <- bind_rows(try_la1, try_la1) %>% 
  dplyr::filter(str_detect(TraitName, "area")) %>% 
  dplyr::filter(!is.na(StdValue)) %>% 
  dplyr::group_by(AccSpeciesName) %>% 
  dplyr::summarise(la = mean(StdValue))


### Austraits 
austraits <- load_austraits(version = "3.0.2", path = "data/austraits")

aust_trait <- 
  extract_trait(
    austraits, 
    c("leaf_area", "specific_leaf_area", "seed_mass", "plant_height")
  ) %>%
  pluck(1) %>% 
  dplyr::group_by(taxon_name, trait_name) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  pivot_wider(names_from = trait_name, values_from = value) %>% 
  dplyr::rename(
    sciname = taxon_name,
    sla = specific_leaf_area,
    height = plant_height,
    la = leaf_area 
  )


### Combining the leaf and seed trait data
df_leaf <- try_height %>% 
  dplyr::left_join(try_seed, by = "AccSpeciesName") %>% 
  dplyr::left_join(try_sla, by = "AccSpeciesName") %>% 
  dplyr::left_join(try_la, by = "AccSpeciesName") %>% 
  dplyr::rename(sciname = AccSpeciesName) %>% 
  
  bind_rows(aust_trait) %>% 
  dplyr::group_by(sciname) %>% 
  dplyr::summarise(across(where(is.numeric), mean)) 





## Growth form -------

### Try
try_life <- bind_rows(try_seed1, try_seed2) %>% 
  dplyr::filter(str_detect(TraitName, "longevity")) %>% 
  dplyr::mutate(
    
    # Annual
    annual = case_when(
      str_detect(
        OrigValueStr, 
        paste(c("annual","Annual","annuel", "> 1 year",
                "Biennial","biennial", "<2", "1-2", "1,2","Short"), 
              collapse = "|")) ~ 1,
      OriglName == "Annual" ~ 1,
      OriglName == "Biennial" ~ 1,
      OriglName == "annper" ~ 1,
      OriglName == "Plant phenology: Annual" ~ 1,
      OriglName == "Plant phenology: Biennial" ~ 1,
      OrigValueStr == "1" ~ 1,
      OrigValueStr == "2" ~ 1,
      OrigValueStr == "SL" ~ 1,
      UnitName == "year" & StdValue < 3 ~ 1),
    annual = if_else(is.na(annual), 0, 1),
    
    # Perennial
    perennial = case_when(
      str_detect(
        OrigValueStr, 
        paste(c("perennial", "Perennial",">10","-5","5-25","2 - 5",
                "Moderate","Long"), 
              collapse = "|")) ~ 1,
      OriglName == "Plant phenology: Perennial" ~ 1,
      OriglName == "Perennial" ~ 1,
      UnitName == "year" & StdValue > 2 & StdValue < 50 & StdValue != 12 ~ 1),
    perennial = if_else(is.na(perennial), 0, 1),
    
    
    # Tree
    tree = case_when(
      str_detect(
        OrigValueStr, 
        paste(c("tree", "woody"), 
              collapse = "|")) ~ 1,
      UnitName == "year" & StdValue >= 50 ~ 1),
    tree = if_else(is.na(tree), 0, 1)
  ) %>%
  
  dplyr::group_by(AccSpeciesName) %>% 
  dplyr::summarise(across(annual:tree, max)) %>% 
  dplyr::rename(sciname = AccSpeciesName)




### Austrait
aust_life <- extract_trait(austraits, "life_history") %>% 
  pluck(1) %>% 
  dplyr::inner_join(
    extract_trait(austraits, "plant_growth_form") %>% 
      pluck(1),
    by = "taxon_name"
  ) %>% 
  dplyr::select(taxon_name, value.x, value.y) %>% 
  dplyr::rename(life = value.x, wood = value.y) %>% 
  dplyr::mutate(
    annual = if_else(str_detect(life, "annual|biennial"), 1, 0),
    perennial = if_else(str_detect(life, "perennial"), 1, 0),
    tree = if_else(str_detect(wood, "tree|shrub"), 1,0),
    
    perennial = if_else(tree == 1, 0, perennial),
    annual = if_else(tree == 1, 0, annual)
  ) %>% 
  dplyr::select(-wood, -life) %>% 
  dplyr::filter(rowSums(across(where(is.numeric))) > 0) %>% 
  dplyr::group_by(taxon_name) %>% 
  dplyr::summarise(across(where(is.numeric), max)) %>% 
  dplyr::rename(sciname = taxon_name)



### Picture book
pic_data <- read_csv("data/SpeciesList_vegetation.csv")

pic_growth <- pic_data %>% 
  dplyr::select(species, sciname_ylist, life) %>% 
  dplyr::filter(!is.na(life) & life != 0 & life != "-") %>% 
  dplyr::mutate(
    annual = if_else(str_detect(life, "1年|2年"), 1, 0),
    perennial = if_else(str_detect(life, "多年"), 1, 0),
    tree = if_else(str_detect(life, "木|本"), 1,0)
  ) %>% 
  dplyr::distinct(sciname_ylist, .keep_all = TRUE) %>% 
  dplyr::filter(sciname_ylist != "#N/A") %>% 
  dplyr::rename(sciname = sciname_ylist) %>% 
  dplyr::select(-life, -species)



### Combining the growth form data
df_growth <- 
  dplyr::bind_rows(try_life, aust_life, pic_growth) %>% 
  dplyr::group_by(sciname) %>% 
  dplyr::summarise(across(where(is.numeric), sum)) %>% 
  
  # Deciding a growth form at each species
  dplyr::mutate(growth = as.factor(dplyr::case_when(
    tree >= 1 ~ "tree",
    perennial >= 1 & tree == 0 ~ "perennial",
    annual >= 1 & tree == 0 ~ "annual",
    perennial > annual ~ "perennial",
    annual > perennial ~ "annual"))
  ) %>% 
  dplyr::select(sciname, growth) 


## Dispersal type ------

# Function of divide to dispersal category from words

fun_dispersal <- function(df) {
  
  df %>% dplyr::mutate(
    wind = if_else(
      str_detect(
        dispersal,
        paste(c("wind","Wind","anemo","Anemo","herpochor",
                "meteorochor","boleochor","chamaechor","semachor","風"),
              collapse = "|")
        ),
        1, 0),
      
      animal = if_else(
        str_detect(
          dispersal,
          paste(c("bird","Bird","cow","Animal","animal","Mammal","mammal",
                  "zoochor","eate","Zoo","vertebrate", "goat",
                  "Cattle","cattle","Ant","ants","pig","horse","mouse","donkey",
                  "Vertebrate","vertebrate","cow","sheep","deer","rabbit",
                  "dysochor","buffalo","snail","marten","wild","fish",
                  "アリ","食","myrmecochory","dog", "insect","epizoochor",
                  "endozoochor", "Sheep", "Civet", "Iridomyrmex sp.", "elaiosome",
                  "earthworm", "chamois",
                  
                  # human dispersal
                  "man","vehicle","agochor","ethelochor","commerce",
                  "machinery","hemerochor","clothe","speirochor",
                  "contamination", "cars", "mobile",
                  "付着","体","adhesion"
          ),
          collapse = "|")),
        1, 0),
      
      water = if_else(
        str_detect(
          dispersal,
          paste(c("Water","water","hydro","rain","ombrochor","ombochor",
                  "海","水","雨"),
                collapse = "|")),
        1, 0),
      
      gravity = if_else(
        str_detect(
          dispersal,
          paste(c("Unassisted","drop","accidentally","Dispersal no",
                  "unassist","unspecialise","ballochor",
                  "Autocho","autocho","Barochory","barochory",
                  "Restricted","ballistic","blastochor",
                  "重力","自動","gravity"),
                collapse = "|")),
        1, 0)
    ) %>% 
    dplyr::mutate(
      animal = if_else(dispersal == "ant", 1, animal),
      gravity = if_else(dispersal == "Baro", 1, gravity),
      water = if_else(dispersal == "H", 1, water),
      wind = if_else(dispersal == "W", 1, wind),
      animal = if_else(dispersal == "Z", 1, animal),
      gravity = if_else(dispersal == "G", 1, gravity)
    ) %>% 
    
    as_tibble() 
}




### Try
try_disper1 <- rtry_import("data/try/25827.txt", showOverview = FALSE)
try_disper2 <- rtry_import("data/try/25828.txt", showOverview = FALSE)


try_disper <- bind_rows(try_disper1, try_disper2) %>% 
  dplyr::filter(
    str_detect(TraitName, "Dispersal") & 
      OriglName != "Seed Weight Notes"
    ) %>% 
  group_by(AccSpeciesName) %>% 
  distinct(DataName, OrigValueStr, OriglName) %>% 
  dplyr::rename(dispersal = OrigValueStr) %>% 
  fun_dispersal() %>% 
  dplyr::mutate(
    wind = if_else(str_detect(DataName, "wind"), 1, wind),
    animal = if_else(str_detect(DataName, "animal"), 1, animal),
    gravity = if_else(str_detect(DataName, "gravity"), 1, gravity),
    water = if_else(str_detect(DataName, "water"), 1, water),
    animal = if_else(str_detect(DataName, "biotic"), 1, animal),
    
    gravity = if_else(OriglName == "DispMode" & str_detect(dispersal,"G"),
                      1, gravity),
    wind = if_else(OriglName == "DispMode" & str_detect(dispersal,"W"),
                   1, wind),
    water = if_else(OriglName == "DispMode" & str_detect(dispersal,"H"),
                    1, water),
    animal = if_else(
      OriglName == "DispMode" & 
        str_detect(dispersal, 
                   paste(c("Z","P","M","N","O"), collapse = "|")),
      1, animal),
    sum = rowSums(across(where(is.numeric)))
  ) %>% 
  dplyr::filter(sum != 0) %>% 
  dplyr::rename(sciname = AccSpeciesName) %>% 
  #left_join(select(plant_sci, species, sciname), by = "sciname") %>% 
  #filter(!is.na(species)) %>% 
  dplyr::select(-DataName:-dispersal, -sum) %>% 
  
  dplyr::group_by(sciname) %>% 
  dplyr::summarise(across(where(is.numeric), max))



### Picture book
pic_data <- read_csv("data/SpeciesList_vegetation.csv")

pic_disper <- pic_data %>% 
  dplyr::select(species, sciname_ylist, dispersal) %>% 
  dplyr::filter(!str_detect(dispersal, c("-|不|sheet"))) %>% 
  dplyr::mutate(
    species = recode(
      species,
      "トゲヂシャ" = "トゲチシャ",
      "ヒナタイノコズチ" = "ヒナタイノコヅチ",
      "マハナシ" = "マメナシ",
      "マルバイチヤクソウ" = "マルバノイチヤクソウ",
      "アケビ" = "ゴヨウアケビ"
    )
  ) %>% 
  dplyr::distinct(sciname_ylist, .keep_all = TRUE) %>% 
  filter(sciname_ylist != "#N/A") %>% 
  fun_dispersal() %>% 
  dplyr::rename(sciname = sciname_ylist) %>% 
  dplyr::select(-dispersal)


### Austraits
austraits <- load_austraits(version = "3.0.2", path = "data/austraits")

aust_disper <- extract_trait(austraits, "dispersal_syndrome") %>% 
  pluck(1) %>% 
  dplyr::rename(dispersal = value) %>% 
  fun_dispersal() %>% 
  dplyr::mutate(sum = wind + animal + water + gravity) %>% 
  dplyr::filter(sum != 0) %>% 
  dplyr::select(taxon_name, wind:gravity) %>% 
  dplyr::rename(sciname = taxon_name)


### leda
leda <- read_csv("data/leda.csv")

leda_disper <- leda %>% 
  dplyr::select("SBS name", "dispersal type") %>% 
  dplyr::rename(sciname = "SBS name", dispersal = "dispersal type") %>% 
  dplyr::filter(dispersal != "") %>% 
  fun_dispersal() %>% 
  mutate(sum = wind + animal + water + gravity) %>% 
  filter(sum != 0) %>% 
  dplyr::select(-dispersal, -sum)


### Combining the all dispersal type data
df_disper <- bind_rows(try_disper, pic_disper, aust_disper, leda_disper) %>% 
  dplyr::group_by(sciname) %>% 
  dplyr::summarise(across(where(is.numeric), sum)) %>% 
  dplyr::mutate(disper = colnames(.)[-1][max.col(across(-sciname))]) %>%
  dplyr::select(-wind:-gravity)


# .-------




# Exotic species -------
YList <- read_csv("data/YList.csv")

df_ylist <- YList %>% 
  dplyr::select(和名,生態)　%>% 
  dplyr::rename(species = 和名,  exotic = 生態) %>% 
  dplyr::mutate(exotic = if_else(is.na(exotic), "Native", "Exotic")) %>% 
  dplyr::filter(species != "") %>% 
  dplyr::distinct(species, .keep_all = TRUE) %>% 
  dplyr::mutate(exotic = if_else(exotic == "","Native", exotic)) 




# Scientific name -----

# Data frame for scientific name and japanese name
plantsci <- read_csv("data/plant_japan_sciname.csv")

plant_sci <- plantsci %>%  
  dplyr::select("common name", "scientific name without author") %>% 
  dplyr::rename(
    sciname = "scientific name without author",
    species = "common name"
  ) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  arrange(species) %>% 
  dplyr::filter(!str_detect(species, "no_named"))


# Combining all trait data based on scientific name
df_trait <- plant_sci %>% 
  dplyr::left_join(df_leaf, by = "sciname") %>% 
  dplyr::left_join(df_growth, by = "sciname") %>% 
  dplyr::left_join(df_disper, by = "sciname") %>% 
  dplyr::left_join(df_ylist, by = "species")





# Data frame for plant ---------------
plant <- read_csv("data/df_plant.csv")
env   <- read_csv("data/Environment.csv")

df_plant <- plant %>% 
  dplyr::mutate(
    time = if_else(str_detect(site, "過去"), "past", "now"),
    site = str_sub(site, end = -3)
    ) %>% 
  dplyr::select(site, time, everything()) %>% 
  pivot_longer(cols = -c(site, time), names_to = "species") %>% 
  
  # Combine the growth, dispersal, and exotic data
  dplyr::inner_join(df_trait, by = "species") 





# Species richness at each time 
df_plant %>% 
  dplyr::filter(value == 1 & (exotic == "Native" | exotic == "Exotic")) %>% 
  dplyr::group_by(exotic) %>% 
  dplyr::summarise(sr = n_distinct(species), .groups = "drop") 


