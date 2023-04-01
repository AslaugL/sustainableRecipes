devtools::load_all(path = '.')

library(viridis)
library(viridisLite)
library(shadowtext)

# Article
# Setup ----
#Empty list to fill with plots and plot legends
plots <- list()
plot_legends <- list() #For shared legends
plot_titles <- list() #For shared titles
plots$final <- list() #For plots that needs extra tweaking   

#GGplot theme, allows for markdown syntax in strip.text ggplot elements
theme_set(theme_bw()) +theme_replace(strip.text = ggtext::element_markdown(), strip.text.y = ggtext::element_markdown(angle = 270))

#Read data
data_recipes <- readRDS('./Data/output/recipes_for_analysis.Rds') %>% #Nutrient content/sustainability indicators pr 100g recipe
  #Remove non-dinner recipes
  filter(!sample_id %in% c('Baklava (Turkey)', 'Sausage Lunch', 'Half-fermented trout', 'Straight trout', 'Fruit Package')) %>%
  arrange(group) %>% #Set order of countries for plot legends
  #Change US to USA in line with updated article text
  mutate(group = str_replace(group, "US", "USA")) %>%
  #remove Source from df
  select(-Source)

data_ingredients <- readRDS('./Data/output/ingredients_for_analysis.Rds') %>% #Nutrient content/sustainability indicators for every ingredient pr 100g recipe
  rename(Foodgroup = L1) %>%
  select(-FoodEx2) %>%
  #Remove non-dinner recipes
  filter(!sample_id %in% c('Baklava (Turkey)', 'Sausage Lunch', 'Half-fermented trout', 'Straight trout', 'Fruit Package')) %>%
  arrange(group) %>% #Set order of countries for plot legends
  #Change US to USA in line with updated article text
  mutate(group = str_replace(group, "US", "USA")) %>%
  #remove Source from df
  select(-Source)

#Turn tidy
tidy_recipes <- data_recipes %>%
  pivot_longer(.,
               cols = -c(sample_id, group),
               names_to = 'feature',
               values_to = 'value')

tidy_ingredients <- data_ingredients %>%
  pivot_longer(.,
               cols = -c(Ingredients, sample_id, group, Foodgroup, Amounts),
               names_to = 'feature',
               values_to = 'value') %>%
  #Rename some foodgroups
  mutate(Foodgroup = Foodgroup %>%
           str_replace('Fish, seafood, amphibians, reptiles and invertebrates', 'Seafood') %>%
           str_replace('Legumes, nuts, oilseeds and spices', 'Legumes, nuts, seeds') %>%
           str_replace('Meat and meat products', 'Meat and\nmeat products') %>%
           str_replace('Milk and dairy products', 'Dairy') %>%
           str_replace('Animal and vegetable fats and oils and primary derivatives thereof', 'Fats and oils') %>%
           str_replace('Eggs and egg products', 'Eggs') %>%
           str_replace('Starchy roots or tubers and products thereof, sugar plants', 'Roots and tubers') %>%
           str_replace('Products for non-standard diets, food imitates and food supplements', 'Food imitates') %>%
           str_replace('Grains and grain-based products', 'Grains and grain\nbased products') %>%
           str_replace('Sugar and similar, confectionery and water-based sweet desserts', 'Sugar and\n confectionary') %>%
           str_replace('Fruit and vegetable juices and nectars \\(including concentrates\\)', 'Fruit/vegetable juice\n and nectar') %>%
           str_replace('Fruit and fruit products', 'Fruit and\nfruit products') %>%
           str_replace('Vegetables and vegetable products', 'Vegetables and\nvegetable products') %>%
           str_replace('Seasoning, sauces and condiments', 'Seasoning, sauces\nand conditments') %>%
           str_replace('Water and water-based beverages', 'Water-based\nbeverages') %>%
           str_replace('Coffee, cocoa, tea and infusions', 'Coffee, cocoa, tea\nand infusions')
  ) %>% replace_na(list(Foodgroup = 'Unknown'))

#Recipe metadata
metadata <- readRDS('./Data/output/recipe_metadata.Rds') %>% arrange(group) %>%
  #Change US to USA in line with updated article text
  mutate(group = str_replace(group, "US", "USA"))

#Various to keep environment clean
various <- list(
  
  'minerals' = c('Calcium', 'Iron', 'Zinc', 'Magnesium', 'Potassium', 'Selenium', 'Iodine', 'Sodium',
                 'Phosphorus', 'Copper'),
  'energy_contributing' = c('Kilojoules', 'Kilocalories', 'Fat', 'SatFa', 'PuFa', 'MuFa', 'Carbo',
                            'Sugar', 'Mono+Di', 'Starch', 'Dietary fibre', 'Alcohol', 'EPA', 'DHA',
                            'DPA', 'Protein'),
  'vitamins' = c('Vitamin A', 'Retinol', 'Beta-carotene', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin B6',
                 'Folate', 'Vitamin B12', 'Vitamin C', 'Vitamin D', 'Vitamin E'),
  'sustainability' = c('CO2', 'Landuse'),
  'health' = c('inverted_nutriscore', 'nutriscore_letter', 'inverted_traffic_score', 'who_score', 'nnr_score', 'keyhole_certified')
)
#Select colors for the different countries for plots where color is not pre-coded
various$country_colors <- groupColors(tidy_recipes)

# Date exploration----
## Data completeness----
temp <- readRDS('./Data/output/missing_data2.Rds') #Read data from cleanup script

#Fix some column names
data_completeness <- list(
  'no_amounts' = temp$no_amounts,
  
  'no_nutrient_info' = temp$no_nutrient_info %>% rename(value = pct_of_full_recipe) %>%
    #If any recipe has more than one ingredient missing nutrient values, sum the amounts together
    group_by(sample_id) %>% summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
    
    #Add country/group and the recipes with 100% nutrient info
    full_join(., metadata) %>%
    replace_na(list(value = 0)) %>% #Recipes added have 0 nutrient information missing %>%
    
    #Make it so that value is the % of ingredients mapped to database, not the % of ingredients NOT mapped to db
    mutate(value = 100-value) %>%
    filter(sample_id != 'Fresh mackerel'), #Remove the Fresh mackerel that has no ingredients
  
  'no_env_info' = temp$no_sustainability_indicators %>% rename(value = pct_of_full_recipe) %>%
    #If any recipe has more than one ingredient missing sustainability indicators, sum the amounts together
    group_by(sample_id) %>% summarise(value = sum(value, na.rm = TRUE)) %>% ungroup() %>%
    #Add country/group and the recipes with 100% sustainability indicators calculates
    full_join(., metadata) %>%
    replace_na(list(value = 0)) %>% #Recipes added have 0 sustainability indicators missing
    
    #Make it so that value is the % of ingredients mapped to database, not the % of ingredients NOT mapped to db
    mutate(value = 100-value) %>%
    filter(sample_id != 'Fresh mackerel') #Remove the Fresh mackerel that has no ingredients
)

#Plot
plots$data_completeness <- list(
  
  'nutrients' = plotViolinBox(data_completeness$no_nutrient_info) +
    scale_color_manual(values = various$country_colors$sample_group) +
    labs(
      color = 'Country',
      x = 'Country',
      y = '% of recipe weight with nutrient information'
    )  +
    #Set y label to percent
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%")),
  'environment' = plotViolinBox(data_completeness$no_env_info) +
    scale_color_manual(values = various$country_colors$sample_group) +
    labs(
      color = 'Country',
      x = 'Country',
      y = '% of recipe weight with env. impact indicators'
    ) +
    #Set y label to percent
    scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%"))
  
)
#Capture legends
plot_legends$data_completeness <- get_legend(
  plots$data_completeness$nutrients +  
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
#Final plot
#Title
plots$titles$data_completeness <- ggdraw() + 
  draw_label(
    "Data completeness",
    x = 0,
    hjust = -0.32
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
#Without legend
temp <- plot_grid(plots$data_completeness$nutrients + theme(legend.position  = 'none'),
                  plots$data_completeness$environment + theme(legend.position = 'none'),
                  
                  #Labels, number of rows and relative width of plots
                  labels = c('A', 'B'),
                  nrow = 1,
                  rel_widths = c(1,1))
#With legend
plots$final$data_completeness <- plot_grid(plots$titles$data_completeness, temp, plot_legends$data_completeness,
                                           nrow = 3,
                                           rel_heights = c(1,10,1))
#Save
save_plot('./thesis/images/data_completenes_plot.png', plots$final$data_completeness,
          base_height = 5, base_width = 12)


## Calculate health indicator scores----
temp <- list(
  
  'nutriscore' = tidy_ingredients %>% calculateNutritionScore_nutriscore() %>%
    inner_join(metadata %>% select(sample_id, group)),
  
  'multiple_traffic_light' = data_recipes %>%
    select(-group) %>%
    calculateNutritionScore_trafficlights() %>%
    inner_join(metadata %>% select(sample_id, group)),
  
  'who_score' = data_recipes %>%
    calculateNutritionScore_who() %>%
    inner_join(metadata %>% select(sample_id, group)),
  
  'nnr_score' = data_recipes %>%
    calculateNutritionScore_nnr() %>%
    inner_join(metadata %>% select(sample_id, group))
)

health_indicators <- temp %>% reduce(full_join, by = c('sample_id', 'group')) %>%
  #Turn tidy except for nutriscore letters and keyhole classification
  pivot_longer(
    cols = -c(sample_id, group, nutriscore_letter),
    names_to = 'feature',
    values_to = 'value'
  ) %>%
  #Add back Source metadata
  inner_join(., metadata) %>%
  #arrange
  arrange(group)

#Save score in wide format
write_csv(health_indicators %>%
            select(-nutriscore_letter) %>% pivot_wider(., names_from = feature, values_from = value),
          './Supplementary/SupTable6.csv')

#Raw scores for supplementary materials
temp <- list(
  
  #% of fruits, veg and legumes from Nutriscore
  'nutriscore' = tidy_ingredients %>% calculateNutritionScore_nutriscore(raw_scores = TRUE),
  
  #Pct of energy for dietary guidelines
  'energy_pct' = data_recipes %>% calculateEnergypercentDensity(.) %>%
    #Add unit
    mutate(unit = case_when(
      str_detect(feature, 'fibre') ~ 'g/Megajoule',
      TRUE ~ 'Energy percent'
    )) %>%
    #Unite with feature column to use as column names later
    unite(., col = 'temp', c(feature, unit)) %>%
    #Extract values used
    mutate(value = case_when(
      str_detect(temp, 'fibre') ~ densityMJ,
      TRUE ~ energy_percent
    )) %>%
    #Turn wide
    select(sample_id, temp, value) %>%
    pivot_wider(.,
                names_from = temp,
                values_from = value),
  
  #Grams per 100 g for multiple traffic lights and nutriscore
  'grams' = data_recipes %>% select(sample_id, group, Kilojoules, Fat, Salt, SatFa, Sodium, Sugar, `Dietary fibre`) %>%
    #Pivot longer to add units
    pivot_longer(.,
                 cols = -c(sample_id, group),
                 names_to = 'feature',
                 values_to = 'value') %>%
    #Add units
    mutate(unit = case_when(
      feature == 'Kilojoules' ~ 'kJ/100g',
      feature == 'Sodium' ~ 'mg/100g',
      TRUE ~ 'g/100g'
    )) %>%
    #Unite with feature column and turn wide
    unite(., col = 'temp', c(feature, unit)) %>%
    pivot_wider(.,
                names_from = temp,
                values_from = value)
)
#change names of nutriscore % fruit veg column
temp$nutriscore$raw_scores <- temp$nutriscore$raw_scores %>%
  filter(feature == 'nutriscore_amounts_pct') %>%
  mutate(feature = str_replace(feature, 'nutriscore_amounts_pct', '% of fruit, vegetables,\noils, legumes and nuts')) %>%
  select(-nutriscore_raw) %>% pivot_wider(names_from = feature, values_from = value)

#Add all together and save for supplementary
temp2 <- full_join(temp$nutriscore$raw_scores, temp$energy_pct) %>% full_join(., temp$grams)

write_csv(temp2, './Supplementary/SupTable4.csv')

# Statistical analyses----
## Format nutrients----
#Turn micronutrients into % of RDI for a woman 18-30 yo
various$RDI <- tibble(
  'feature' = c('Vitamin A', 'Retinol', 'Beta-carotene',
                'Vitamin D','Vitamin E', 'Thiamin',
                'Riboflavin','Niacin', 'Vitamin B6',
                'Folate', 'Vitamin B12', 'Vitamin C',
                'Calcium', 'Iron', 'Sodium',
                'Potassium', 'Magnesium', 'Zinc',
                'Selenium', 'Copper', 'Phosphorus',
                'Iodine'),
  'rdi' = c(700, 700, 8400,
            10, 8, 1.1,
            1.3, 15, 1.2,
            400, 2, 75,
            800, 15, 2300,
            3100, 280, 7,
            50, 0.9, 600,
            150)
)

#% of RDI for the various micronutrients that can be found in each recipe
various$with_RDI <- tidy_recipes %>%
  #Join the two df's together and calculate the % of rdi in each recipe
  inner_join(., various$RDI) %>%
  mutate(pct_rdi = round(value/rdi*100, 0)) %>%
  rename(raw_value = value,
         value = pct_rdi) %>%
  select(-c(rdi, raw_value))

#Turn wide and save
temp2 <- various$with_RDI %>%
  #Add RDI to featurenames
  mutate(feature = paste0(feature, '_RDI/100g')) %>%
  #Turn diwde
  pivot_wider(.,
              names_from = feature,
              values_from = value)

write_csv(temp2, './Supplementary/SupTable5.csv')

#Energy percentage of macronutrients in each recipe
various$with_energy_pct_densityMJ <- data_recipes %>%
  #Energy percent for macros, density pr MJ for fibre
  calculateEnergypercentDensity() %>%
  mutate(value = case_when(
    feature != 'Dietary fibre' ~ energy_percent,
    feature == 'Dietary fibre' ~ densityMJ
  )) %>% select(-c(densityMJ, energy_percent))

## Df for stats----
#Macros and micros in E% and % of RDI
run_stats <- bind_rows(various$with_RDI, various$with_energy_pct_densityMJ) %>%
  #Add kilocalories per 100g
  full_join(data_recipes %>% select(sample_id, group, Kilocalories) %>% pivot_longer(., cols = Kilocalories, names_to = 'feature', values_to = 'value')) %>%
  #Add health indicators, except nutriscore letters
  full_join(health_indicators %>% select(-c(nutriscore_letter))) %>%
  #Add sustainability indicators
  full_join(tidy_recipes %>% filter(feature %in% c('CO2', 'Landuse'))) %>%
  #Add country and where missing
  group_by(sample_id) %>% fill(group, .direction = "downup") %>% fill(Source, .direction = "downup") %>% ungroup()

## Descriptive stats----
### Per country----
descriptive_stats_country <- run_stats %>%
  group_by(group, feature) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            q3 = quantile(value, 0.75),
            max = max(value),
            mean = mean(value))

### Total----
descriptive_stats_total <- run_stats %>%
  group_by(feature) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            q3 = quantile(value, 0.75),
            max = max(value),
            mean = mean(value)) %>%
  mutate(IQR = paste0('(', round(q1, 1), ', ', round(q3, 1), ')'),
         median = round(median, 1))

### Missing data----
temp <- data_completeness$no_nutrient_info %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

temp <- data_completeness$no_env_info %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

## Counts----
#Count the number of recipes in each healthiness category
temp <- health_indicators %>%
  select(-Source) %>%
  filter(feature != 'inverted_nutriscore') %>%
  mutate(value = as.character(value)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  pivot_longer(.,
               cols = -c(sample_id, group),
               names_to = 'feature',
               values_to = 'value') %>%
  group_by(feature, value, group) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  #Calculate pct, add total number of recipes
  inner_join(., data_recipes %>% select(sample_id, group) %>% group_by(group) %>% summarise(sum = n()) %>% ungroup()) %>%
  #Add totals for all countries for each indicator
  bind_rows(., health_indicators %>%
              select(-Source) %>%
              filter(feature != 'inverted_nutriscore') %>%
              mutate(value = as.character(value)) %>%
              pivot_wider(.,
                          names_from = feature,
                          values_from = value) %>%
              pivot_longer(.,
                           cols = -c(sample_id, group),
                           names_to = 'feature',
                           values_to = 'value') %>%
              group_by(feature, value) %>%
              summarise(n = n()) %>%
              ungroup() %>% mutate(group = 'All countries',
                                   sum = nrow(data_recipes))) %>%
  mutate(pct = n/sum*100) %>%
  mutate(pct = sprintf(pct, fmt = '%#.1f')) %>%
  mutate(pct = paste0(pct, ' %')) %>%
  #Format for thesis
  mutate(feature = feature %>%
           str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
           str_replace('nnr_score', 'Nordic Nutrition\nRecommendations') %>%
           str_replace('who_score', 'World Health Organization\nRecommendations') %>%
           str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model') %>%
           str_replace('nutriscore_letter', 'Nutriscore')) %>%
  select(feature, group, value, pct) %>%
  #Arrange for table
  #Without UK recipes
  mutate(group = factor(group, c('Norway', 'USA', 'All countries'))) %>%
  # With UK Recipes
  #mutate(group = factor(group, c('Norway', 'UK', 'USA', 'All countries'))) %>%
  #Split on feature to format each indicator separately
  group_by(feature) %>% group_split() %>%
  #Pivot wider
  lapply(., function(x) {x %>% pivot_wider(., names_from = c(group, feature), values_from = pct)}) %>%
  #Set order of countries
  lapply(., function(x) {x %>% select(value, starts_with('Norway'), starts_with('UK'), starts_with('US'), starts_with('All'))})

#Add 0 % to other indicators where no recipes has that score, and order from best to lowest score
temp <- lapply(temp, function(x) {x %>% replace(is.na(.), '0.0 %')})

#Add empty rows to nutriscore before col_bind
temp[[3]] <- temp[[3]] %>%
  # replace(is.na(.), '0.0 %') %>%
  # #Replace NA with empty space, add % for UK's E scores
  # mutate(across(everything(), case_when(
  #   is.na(.) ~ '0.0 %',
  #   TRUE ~ .
  # ))) %>%
  #Add two empty rows to have the same number of rows as the other tables
  add_row() %>%
  add_row() %>%
  replace(is.na(.), ' ')

#Order by descending score
temp[[2]] <- temp[[2]] %>%
  arrange(desc(value))
temp[[4]] <- temp[[4]] %>%
  arrange(desc(value))
temp[[1]] <- temp[[1]] %>%
  mutate(value = factor(value, levels = c(12, 11, 10, 9, 8 , 7, 6))) %>%
  arrange(value)

pct_recipes_healthiness_scores <- bind_cols(temp) %>%
  #Fix column names
  #With UK data
  # rename(
  #   `Score_Inverted Multiple\nTraffic Light Model` = value...1,
  #   `Score_Nordic Nutrition\nRecommendations` = value...6,
  #   `Score_Nutriscore` = value...11,
  #   `Score_World Health Organization\nRecommendations` = value...16
  # ) %>%
  #Without UK Data
    rename(
      `Score_Inverted Multiple\nTraffic Light Model` = value...1,
      `Score_Nordic Nutrition\nRecommendations` = value...5,
      `Score_Nutriscore` = value...9,
      `Score_World Health Organization\nRecommendations` = value...13
    ) %>%
  select(ends_with('Inverted Multiple\nTraffic Light Model'), ends_with('Nutriscore'), ends_with('Nordic Nutrition\nRecommendations'), ends_with('World Health Organization\nRecommendations'))

# Save table for source files
#With UK data
# write_csv(pct_recipes_healthiness_scores[,1:10], "./Supplementary/Source_ED_Table1.csv")
# write_csv(pct_recipes_healthiness_scores[,11:20], "./Supplementary/Source_ED_Table2.csv")

#Save tables as jpg
#Front of pack labels
pct_recipes_healthiness_scores %>%
  select(c(ends_with('Traffic Light Model'), ends_with('Nutriscore'))) %>%
  mutate(across(everything(), ~str_replace(., "%", "\\%"))) %>%
  
  kbl(booktabs = TRUE, linesep = "", escape = FALSE,
      
      #Set column names and align
      #With UK data
      # col.names = rep(c("Score", "Norway", "UK", "US", "All countries"), 2),
      #Without UK data
      col.names = rep(c("Score", "Norway", "US", "All countries"), 2),
      #caption = "The percentage of recipes that received a specific score on each front-of-pack-label indicator.",
      #With UK data
      # align = rep(c("l", "c", "c", "c", "c"), 2)) %>%
      #Without UK data
      align = rep(c("l", "c", "c", "c"), 2)) %>%
  
  #With UK data
  # #Add header for each indicator
  # add_header_above(c("Inverted Multiple Traffic Light Model" = 5, "Nutriscore" = 5)) %>%
  # #Add header to distinguish FOPL and guidelines
  # add_header_above(c("Front of Pack Labels" = 10)) %>%
  #Without UK data
  #Add header for each indicator
  add_header_above(c("Inverted Multiple Traffic Light Model" = 4, "Nutriscore" = 4)) %>%
  #Add header to distinguish FOPL and guidelines
  add_header_above(c("Front of Pack Labels" = 8)) %>%
  
  #Make score columns bold
  #With UK data
  # column_spec(., c(1, 6), bold = TRUE) %>%
  # column_spec(., c(2:5, 7:10)) %>%
  #Without UK data
  column_spec(., c(1, 5), bold = TRUE) %>%
  column_spec(., c(2:4, 6:8)) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"), full_width = FALSE) %>% save_kable(., "./Supplementary/Angelsen_ED_Table1.jpg")

#Dietary guidelines
pct_recipes_healthiness_scores %>%
  select(ends_with('Recommendations')) %>%
  mutate(across(everything(), ~str_replace(., "%", "\\%"))) %>%
  
  kbl(booktabs = TRUE, linesep = "", escape = FALSE,
      
      #Set column names and align
      #With UK data
      #col.names = rep(c("Score", "Norway", "UK", "US", "All countries"), 2),
      #Without UK data
      col.names = rep(c("Score", "Norway", "US", "All countries"), 2),
      #caption = "The percentage of recipes that received a specific score on the dietary guideline indicators.",
      #With UK data
      #align = rep(c("l", "c", "c", "c", "c"), 2)) %>%
      #Without UK data
      align = rep(c("l", "c", "c", "c"), 2)) %>%
  
  #With UK data
  #Add header for each indicator
  # add_header_above(c("Nordic Nutritional Recommendations" = 5, "World Health Organization Recommendations" = 5)) %>%
  # #Add header to distinguish FOPL and guidelines
  # add_header_above(c("Dietary guidelines" = 10)) %>%
  # Without UK data
  add_header_above(c("Nordic Nutritional Recommendations" = 4, "World Health Organization Recommendations" = 4)) %>%
  #Add header to distinguish FOPL and guidelines
  add_header_above(c("Dietary guidelines" = 8)) %>%
  
  #Make score columns bold
  #With UK data
  # column_spec(., c(1, 6), bold = TRUE) %>%
  # column_spec(., c(2:5, 7:10)) %>%
  #Without UK data
  column_spec(., c(1, 5), bold = TRUE) %>%
  column_spec(., c(2:4, 6:8)) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"), full_width = FALSE) %>% save_kable(., "./Supplementary/Angelsen_ED_Table2.jpg")

## Kruskal Wallis and dunn----
### Kruskal wallis----
# stats <- list(
#   
#   'kruskal_wallis' = run_stats %>%
#     group_by(feature) %>%
#     kruskal_test(., value ~ group) %>%
#     adjust_pvalue(., method = 'BH'),
#   
#   'kruskal_wallis_effectsize' = run_stats %>%
#     group_by(feature) %>%
#     kruskal_effsize(value ~ group)#,
#   # ci = TRUE) #Include to calculate effect sizes used in final stat table, takes time
# )
# 
# #Save with confidence intervals
# saveRDS(stats, 'stats.Rds')
#Read
stats <- readRDS('stats.Rds')


### Dunn----
#Filter out features that were significantly different in the kruskal wallis
temp <- stats$kruskal_wallis %>%
  filter(p.adj <0.05) %>%
  select(feature) %>% inner_join(run_stats)

stats$dunn_test <- temp %>%
  group_by(feature) %>%
  dunn_test(value ~ group, detailed = TRUE) %>%
  adjust_pvalue(., method = 'BH') %>%
  #Add ** markings for significance levels, and y position to create p-value brackets in plots later
  add_significance() %>%
  add_y_position(scales = 'free_y')

#Dunn test with all variables
stats$dunn_test_all <- run_stats %>%
  group_by(feature) %>%
  dunn_test(value ~ group, detailed = TRUE) %>%
  adjust_pvalue(., method = 'BH')

## Correlation analysis----
plots$correlations <- list()

#Wide df with the variables
temp <- run_stats %>%
  #Make feature names more presentable
  mutate(feature = feature %>%
           str_replace('inverted_nutriscore', 'Inv. Nutriscore') %>%
           str_replace('inverted_traffic_score', 'Inv. Traffic Light') %>%
           str_replace('who_score', 'WHO Score') %>%
           str_replace('nnr_score', 'NNR Score') %>%
           str_replace('CO2', 'CO<sub>2</sub>') %>%
           str_replace('Landuse', 'm<sup>2</sup>')
  ) %>%
  inner_join(., metadata) %>%
  pivot_wider(., names_from = 'feature', values_from = 'value')

#Save for source data
write_csv(temp[,c(26, 31, 4, 14, 29, 30, 9, 25,  33:38)], './Supplementary/SourceFig2-3.csv')

theme_set(theme_bw())

#New upper plot with colored background
myCorrelations_textsize2 <- function(data,mapping,...){
  
  #Get the data to use for correlation analysis
  data2 = data
  data2$x = as.numeric(data[,as_label(mapping$x)])
  data2$y = as.numeric(data[,as_label(mapping$y)])
  data2$group = data[,as_label(mapping$colour)]
  
  #Split into separate datasets based on group and run corr.test each + all groups combined
  to_correlate <- data2 %>%
    select(group, x, y) %>%
    #Split into individual df's and remove group column as it's numeric
    split(f = as.factor(.$group)) %>%
    lapply(., select, ... = -group)
  #Add the df with all groups to the list  
  to_correlate$`Overall Corr` <- data2 %>%
    select(x,y)
  
  #Run correlation analysis on each list element, and turn into a single df before plotting
  correlations <- lapply(to_correlate, corr.test, method = 'spearman', adjust = 'BH') %>%
    #Get the estimate, p.value and create a column with symbols that show stat.sig. og p value
    lapply(., collectCorr) %>%
    #Bind together
    bind_rows(., .id = 'group') %>%
    mutate(group = str_replace(group, "\bUS\b", "USA")) %>%
    #Add y values for geom_rect
    mutate(
      from = case_when(
        group == 'Overall Corr' ~ 0.25,
        group == "USA" ~ 1.5,
        group == "UK" ~ 2.5,
        group == "Norway" ~ 3.5),
      to = case_when(
        group == 'Overall Corr' ~ 1.5,
        group == "USA" ~  2.5,
        group == "UK" ~ 3.5,
        group == "Norway" ~ 4.75)
    )
  
  #Create a color scale to use for the correlation values using viridis
  #color_scale <- viridisLite::viridis(100)
  #From red to blue
  color_scale <- grDevices::colorRampPalette(c("blue","white","red"))(100)
  
  #Add fill value to correlations_df, and a character estimate value for better alignment of geom_text
  correlations <- correlations %>%
    mutate(
      fill_color = color_scale[findInterval(estimate, seq(-1, 1, length=100))],
      text_estimate = case_when(
        estimate >= 0 ~ paste0("", as.character(estimate)),
        TRUE ~ as.character(estimate)
      )
    )
  
  #Plot
  ggplot(data = correlations,
         aes(x = factor(1), y = factor(group, levels = c('Overall Corr', 'USA', 'UK', 'Norway')), color = group)) +
    geom_rect(data = correlations,
              #X axis position of the rectangle
              xmin = as.numeric(factor(1))+.12, xmax = 2,
              #xmin = -Inf, xmax = Inf, 
              #Y axis position based on group
              ymin = correlations$from, ymax = correlations$to, 
              #Colors
              fill = correlations$fill_color, color = NA, inherit.aes = FALSE, alpha = 0.95) +
    #Add text
    geom_richtext(#data = correlations, aes(x = 0.5, y = factor(group, levels = c('Overall Corr', 'US', 'UK', 'Norway')),
      #group = group,
      aes(color = group,
          label=paste0("<b>", group, ":</b> ")),
      #size = 1.95, # Size for health vs sustainability plot
      size = 1.933, # Size for other nutrients
      fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt"), # remove padding and align to the left
      #Adjust text position
      hjust = 0,
      position = position_nudge(x = -0.57) # Position for health vs sustainability plot
    ) +
    geom_shadowtext(
      aes(
        group = group,
        label = paste0(text_estimate, pvalue_star)), color = "white",
      #size = 2, #Size for health and sustainability plot
      size = 1.95, #Size for other nutrients
      fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt"), # remove padding and align to the left
      #inherit.aes = FALSE, #Use new aes as geom_rect changes plot values,
      #Adjust text position
      hjust = 0,
      #position = position_nudge(x = 0.16), #Position for helath vs sustainability plot
      position = position_nudge(x = 0.14) #Position for helath vs sustainability plot
    )
}

### Health indicators with sustainability indicators----
plots$correlations$healthVSsustainability <- ggpairs(temp %>% select(-sample_id) %>% rename(GHGE = `CO<sub>2</sub>`) %>% mutate(group = str_replace(group, "\bUS\b", "USA")),
                                                     mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                     #title = "Spearman's correlations between recipe healthiness and environmental sustainability",
                                                     columns = 32:37,
                                                     upper = list(continuous = myCorrelations_textsize2),
                                                     lower = list(continuous = wrap("smooth", alpha = 0.6, size=0.1, se = FALSE, linewidth = 0.1))
) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#change axis breaks for density in upper left corner and reduce spacing between grids and text size on y axis strip text
plots$correlations$healthVSsustainability$plots[[1]] <- plots$correlations$healthVSsustainability$plots[[1]] +
  scale_y_continuous(breaks = c(0, 0.05, 0.1), labels = c("0", ".05", ".10"))

#Save
save_plot('./correlations_healthVSsustainability_article.pdf',
          plots$correlations$healthVSsustainability +
            #Change text size and margins
            theme(
              axis.text = element_text(size = 5),
              strip.text.x = element_markdown(size = 5, margin = unit(c(0.08, 0.08, 0.08, 0.08), "cm")),
              strip.text.y = element_markdown(size = 5, margin = unit(c(0.08, 0.08, 0.08, 0.08), "cm")),
              #Change the spacing between grids
              panel.spacing=grid::unit(0.1,"lines")
            ),
          #ncol = 2.2, nrow = 2.2
          base_height = 88, base_width = 150, units = "mm"
)

### Other nutrients----
plots$correlations$selected_nutrients <- ggpairs(temp %>% select(-sample_id) %>% rename(Fibre = `Dietary fibre`, GHGE = `CO<sub>2</sub>`) %>%
                                                   mutate(group = str_replace(group, "\bUS\b", "USA")),
                                                 mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                 #title = "Spearman's correlation between recipe nutrient content and environmental sustainability",
                                                 columns = c(25, 30, 3, 13, 28, 29, 8, 24, 36,37),
                                                 upper = list(continuous = myCorrelations_textsize2),
                                                 lower = list(continuous = wrap("smooth", alpha = 0.85, size=0.3, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

#change axis breaks for density in upper left corner and reduce spacing between grids
plots$correlations$selected_nutrients$plots[[1]] <- plots$correlations$selected_nutrients$plots[[1]] +
  scale_y_continuous(breaks = c(0, 0.01, 0.02), labels = c("0", ".01", ".02")) 

#Save
save_plot('./correlations_article_reworked2.pdf', plots$correlations$selected_nutrients +
            #Change text size and margins
            theme(
                #change axis tick length and distance to axis text
                axis.ticks.length = unit(.05, "cm"),
                axis.text.x = element_text(size = 5, margin = margin(t = 0.05, unit = "cm")),
                axis.text.y = element_text(size = 5),
                strip.text.x = element_markdown(size = 5, margin = unit(c(0.08, 0.08, 0.08, 0.08), "cm")),
                strip.text.y = element_markdown(size = 5, margin = unit(c(0.08, 0.08, 0.08, 0.08), "cm")),
                #Change the spacing between grids
                panel.spacing=grid::unit(0.1,"lines"),
                plot.margin = unit(c(.07,.07,.07,.07), "cm")
                ),
          #ncol = 2.8, nrow = 2.8
          base_width = 225, base_height = 150, units = "mm"
)

#Create a legend for the colors
#Viridis
#color_scale <- viridisLite::viridis(100) 
#Blue and red
color_scale <- grDevices::colorRampPalette(c("blue","white","red"))(100)

fill_legend <- tibble(
  rho = seq(from = -1, to = 1, by = 0.01)
) %>%
  mutate(
    fill_color = color_scale[findInterval(rho, seq(-1, 1, length=100))])

#Legend
rho_legend <- ggplot(fill_legend, aes(y = rho, x = 1, fill = rho)) + geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) + labs(fill = "Rho") 

#Legend for Figure 2
save_plot("./rho_legend_Figure2.pdf",
          rho_legend + 
            theme(legend.key.width = unit(0.5, 'cm'),
                  legend.key.height = unit(2, 'cm'),
                  legend.position = "left") +
            guides(fill=guide_colourbar(title.hjust=0.2)))

#Legend for Figure 3
save_plot("./rho_legend_Figure3.pdf",
          rho_legend + 
            theme(legend.key.width = unit(2, 'cm'),
                  legend.key.height = unit(0.5, 'cm'),
                  legend.position = "bottom") +
            guides(fill=guide_colourbar(title.vjust=0.9)))

# Violin boxplots----
plots$violinbox <- list()
## Extended Data figure 2-3----
plots$raw_scores <- list()

# Nutriscore
#Get the raw Nutriscore points for each category
temp <- tidy_ingredients %>% calculateNutritionScore_nutriscore(., raw_score = TRUE)
temp <- temp$raw_scores %>%
  #Add metadata for the recipes
  inner_join(., metadata) %>%
  #Clean up names and set order of values on the x axis of the plot
  mutate(feature = feature %>%
           str_replace('nutriscore_amounts_pct', '% of fruit, vegetables,\noils, legumes and nuts') %>%
           str_replace('SatFa', 'Saturated\nFat') %>%
           str_replace('Dietary fibre', 'Dietary\nfibre')) %>%
  #Create facets based on if features are qualifying or disqualifying
  mutate(quali = case_when(
    str_detect(feature, 'legumes|fibre|Protein') ~ 'Qualifying',
    TRUE ~ 'Disqualifying'
    
  )) %>%
  #Factor and releven
  mutate(feature = factor(feature, levels = c('Kilojoules', 'Sugar', 'Saturated\nFat', 'Sodium', '% of fruit, vegetables,\noils, legumes and nuts', 'Dietary\nfibre', 'Protein')),
         group = factor(group, levels = c('Norway', 'UK', 'USA'))) %>%
  arrange(group)

#Save datafile as source data for the plot
write_csv(temp, "./Supplementary/Source_ED_Fig2.csv")

#Diqualyfing 
plots$raw_scores$nutriscore_disq <- ggplot(temp %>% filter(quali == 'Disqualifying'), aes(x = feature, y = nutriscore_raw, color = group)) +
  geom_boxplot(position = position_dodge(1)) +
  #geom_half_boxplot(side = 'r') + geom_half_violin() +
  scale_color_manual(values = various$country_colors$sample_group) +
  
  #Fix labs and axis
  labs(
    y = 'Points',
    color = 'Country'
  ) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + facet_wrap(~quali)

#Qualifying
plots$raw_scores$nutriscore_qual <- ggplot(temp %>% filter(quali == 'Qualifying'), aes(x = feature, y = nutriscore_raw, color = group)) +
  geom_boxplot(position = position_dodge(1)) +
  #geom_half_boxplot(side = 'r') + geom_half_violin() +
  scale_color_manual(values = various$country_colors$sample_group, breaks = c('Norway', 'UK', 'USA'), labels = c('Norway', 'UK', 'USA')) +
  
  #Fix labs and axis
  labs(
    y = 'Points',
    color = 'Country'
  ) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + facet_wrap(~quali)

#Together
#Create a mock grid to make qualifying plot half of diqualifying in height
temp <- plot_grid(plots$raw_scores$nutriscore_qual + theme(axis.title.y = element_blank()),
                  NULL,
                  ncol = 1,
                  rel_heights = c(3/4, 1/4))

#Title
plots$titles$nutriscore <- ggdraw() + draw_label("Nutriscore raw points",
                                                 x = 0.065,
                                                 hjust = 0.1)

#Whole plot
plots$final$raw_nutriscore <- plot_grid(
  #plots$titles$nutriscore, #Add title or not
  plot_grid(plots$raw_scores$nutriscore_disq + theme(legend.position = 'none'),
            temp,
            nrow = 1,
            rel_widths = c(1.66,1)),
  nrow = 1,
  rel_heights = 1)#c(0.1,0.9)) #Change depending on including title or not

plots$final$raw_nutriscore

#Save
save_plot('./thesis/images/raw_nutriscores.png', plots$final$raw_nutriscore,
          ncol = 1.7)
#Save for article extended data section
save_plot('./Supplementary/Angelsen_ED_Fig2.jpg', plots$final$raw_nutriscore,
          ncol = 1.7)

#Multiple traffic light
temp <- data_recipes %>% select(-group) %>% calculateNutritionScore_trafficlights(., raw_scores = TRUE)
temp <- temp$raw_scores %>% inner_join(., metadata) %>% #Add metadata
  #Rename and fix order of features
  mutate(feature = str_replace(feature, 'SatFa', 'Saturated\nFat')) %>%
  mutate(feature = factor(feature, levels = c('Fat', 'Saturated\nFat', 'Sugar', 'Salt'))) %>%
  mutate(group = factor(group, levels = c("Norway", "UK", "USA")) 
  )

#Save datafile as source data for the plot
write_csv(temp %>% select(-Source), "./Supplementary/Source_ED_Fig3.csv")

plots$raw_scores$mtl <- ggplot(temp, aes(x = feature, y = inverted_traffic_light_rating, color = group)) +
  geom_boxplot(position = position_dodge(1)) +
  #geom_half_boxplot(side = 'r') + geom_half_violin() +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group) +
  
  #Fix labs and axis
  labs(
    y = 'Points',
    color = 'Country',
    title = "Food Standard Agency Multiple Traffic Light raw scores"
  ) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(0, 3, by = 1))

save_plot('./thesis/images/raw_scores_mtl.png', plots$raw_scores$mtl, ncol = 1.7)
save_plot('./Supplementary/Angelsen_ED_Fig3.jpg', plots$raw_scores$mtl, ncol = 1.7)

#Guidelines
#WHO
temp <- data_recipes %>% calculateNutritionScore_who(., raw_scores = TRUE)
temp <- temp$raw %>% inner_join(., metadata) %>%
  #Calculate the percentage of recipes from the different countries that get scores for the different categories
  #Count the number of recipes that get a point for each category
  group_by(group, feature, who_recommendation) %>%
  summarise(n = n()) %>% ungroup() %>%
  pivot_wider(.,
              names_from = who_recommendation,
              values_from = n) %>%
  #Calculate percentage
  mutate(pct = round(`1`/(`0` + `1`)*100, 1)) %>%
  #Set order and change names
  mutate(feature = feature %>%
           str_replace('Carbo', 'Carbohydrates') %>%
           str_replace('SatFa', 'Saturated\nFat') %>%
           str_replace('Dietary fibre', 'Dietary\nFibre')) %>%
  mutate(feature = factor(feature, levels = c('Carbohydrates', 'Sugar', 'Dietary\nFibre', 'Fat', 'Saturated\nFat', 'Protein'))) %>%
  mutate(group = factor(group, levels = c("Norway", "UK", "USA")) 
  )

#Save datafile as source data for the plot
write_csv(temp, "./Supplementary/Source_ED_Fig1.csv")

#Barplot
plots$raw_scores$who <- ggplot(temp, aes(x = feature, y = pct, fill = group)) +
  geom_bar(stat="identity", position = position_dodge(0.95)) +
  
  #Fix colors and labs
  scale_fill_manual(values = various$country_colors$sample_group) +
  labs(fill = 'Country',
       y = 'Percentage of recipes that fulfill criteria') + theme(axis.title.x = element_blank()) +
  #Add % to y axis, and increase y axis to allow for text on top of bars
  scale_y_continuous(limits = c(0,103), labels = function(x) paste0(x, "%")) +
  #Add text to show the percentage for each country
  geom_text(position = position_dodge(0.95), aes(label = paste0(pct, ' %')), vjust = -0.7, size = 3)

#NNR
temp <- data_recipes %>% calculateNutritionScore_nnr(., raw_scores = TRUE)
temp <- temp$raw %>% inner_join(., metadata) %>%
  #Calculate the percentage of recipes from the different countries that get scores for the different categories
  #Count the number of recipes that get a point for each category
  group_by(group, feature, nnr_recommendation) %>%
  summarise(n = n()) %>% ungroup() %>%
  pivot_wider(.,
              names_from = nnr_recommendation,
              values_from = n) %>%
  #Calculate percentage
  mutate(pct = round(`1`/(`0` + `1`)*100, 1)) %>%
  #Set order and change names
  mutate(feature = feature %>%
           str_replace('Carbo', 'Carbohydrates') %>%
           str_replace('SatFa', 'Saturated\nFat') %>%
           str_replace('Dietary fibre', 'Dietary\nFibre')) %>%
  mutate(feature = factor(feature, levels = c('Carbohydrates', 'Sugar', 'Dietary\nFibre', 'Fat', 'Saturated\nFat', 'Protein'))) %>%
  mutate(group = factor(group, levels = c("Norway", "UK", "USA")) 
  )

#Barplot
plots$raw_scores$nnr <- ggplot(temp, aes(x = feature, y = pct, fill = group)) +
  geom_bar(stat="identity", position = position_dodge(0.95)) +
  
  #Fix colors and labs
  scale_fill_manual(values = various$country_colors$sample_group) +
  labs(fill = 'Country',
       y = 'Percentage of recipes that fulfill criteria') + theme(axis.title.x = element_blank()) +
  #Add % to y axis, and increase y axis to allow for text on top of bars
  scale_y_continuous(limits = c(0,103), labels = function(x) paste0(x, "%")) +
  #Add text to show the percentage for each country
  geom_text(position = position_dodge(0.95), aes(label = paste0(pct, ' %')), vjust = -0.7, size = 3)

#The two guidelines together, with shared y axis
plots$final$raw_guidelines <- plot_grid(plots$raw_scores$who + theme(legend.position = 'none') + labs(y=' ', title = 'World Health Organization dietary guidelines'),
                                        plots$raw_scores$nnr + theme(legend.position = 'bottom') + labs(y=' ', title = 'Nordic Nutrition Recommendations dietary guidelines'),
                                        ncol = 1,
                                        rel_heights = c(1,1.2)) +
  #Shared label
  draw_label("Percentage of recipes that fulfill criteria", x =  0, y =0.5, vjust= 1.5, angle=90)

save_plot('./thesis/images/raw_scores_guidelines.png', plots$final$raw_guidelines, nrow = 1.7, ncol = 1.7)
save_plot('./Supplementary/Angelsen_ED_Fig1.jpg', plots$final$raw_guidelines, nrow = 1.7, ncol = 1.7)


## Figure 1A-C----
### Figure 1A----
#Number of recipes with the ingredients
#Classify by type of ingredients
various$recipe_protein_source <- tidy_ingredients %>%
  select(Ingredients, group, Foodgroup, sample_id) %>% unique() %>%
  
  #Separate into beef, lamb, pork, poultry, lean fish, oily fish, shellfish, vegetarian, vegan
  #Animal sourced foods
  mutate(protein = case_when(
    
    str_detect(Ingredients, 'beef|meatball') & !str_detect(Ingredients, 'fund|broth|gravy|elk|deer|venison') ~ 'beef',
    str_detect(Ingredients, 'reindeer|elk|rabbit|grouse|roe deer|venison') ~ 'game',
    str_detect(Ingredients, 'lamb') & !str_detect(Ingredients, 'broth|salad') ~ 'lamb',
    str_detect(Ingredients, 'pork|bacon|sausage|ham|salami') & !str_detect(Ingredients, 'lard|beef|burger') ~ 'pork',
    str_detect(Ingredients, 'chicken|turkey|duck|goose') & !str_detect(Ingredients, 'broth|fat|sauce') ~ 'poultry',
    
    str_detect(Ingredients, 'pollock|cod|anglerfish|fish cakes coarse|haddock|grouper|catfish|sea bass|halibut|tuna') ~ 'lean fish', #The fishcakes are made out of mostly haddock
    str_detect(Ingredients, 'salmon|trout|arctic char|mackerel|herring|sardine|anchovy') & !str_detect(Ingredients, 'roe') ~ 'oily fish',
    str_detect(Ingredients, 'squid|prawn|shrimp|mussel|crab|lobster|shellfish|scampi|clam|scallop') & !str_detect(Ingredients, 'paste') ~ 'shellfish'
    
  )) %>%
  #Vegetarian or vegan foods
  group_by(group, sample_id, Foodgroup) %>%
  mutate(
    protein = case_when(
      
      !any(Foodgroup %in% c('Seafood', 'Eggs', 'Dairy', 'Meat and\nmeat products')) &
        !str_detect(Ingredients, 'lamb|game|fish|beef|chicken|turkey|shrimp|cheese|condensed cream|egg\\b|duck|honey|pork|mayonnaise|barbeque|oyster|worcestershire|sausage|shortening') &
        !Ingredients %in% c('butter', 'unsalted butter', 'butter clarified ghee', 'butter for cooking',
                            'buttermilk', 'refrigerated buttermilk biscuit dough', 'spice butter', 'puff pastry',
                            'shop-bought shortcrust pastry') ~ 'plants',
      !any(Foodgroup %in% c('Seafood', 'Meat and\nmeat products')) & !str_detect(Ingredients, 'condensed cream|beef|fish|shrimp|duck|sausage|shortening') ~ 'vegetarian',
      
      TRUE ~ protein),
    vegetarian = case_when(
      protein %in% c('plants', 'vegetarian') ~ 'vegetarian',
      TRUE ~ protein
    )
  ) %>% group_by(group, sample_id) %>%
  #Count the number of times the different protein sources are used by country
  summarise(type = case_when(
    any(protein == 'beef') ~ 'Beef',
    any(protein == 'game') ~ 'Game',
    any(protein == 'lamb') ~ 'Lamb',
    any(protein == 'poultry') ~ 'Poultry',
    any(protein == 'lean fish') ~ 'Lean fish',
    any(protein == 'oily fish') ~ 'Oily fish',
    any(protein == 'shellfish') ~ 'Shellfish',
    any(protein == 'pork') ~ 'Pork',
    all(protein == 'plants') ~ 'Vegan',
    all(vegetarian == 'vegetarian') ~ 'Vegetarian'
  )) %>% ungroup()

#Save for source data
write_csv(various$recipe_protein_source, './Supplementary/SourceFig1A.csv')

#Calculate percentage of recipes from each country
temp <- various$recipe_protein_source %>%
  group_by(group, type) %>%
  summarise(n = n()) %>% ungroup() %>% drop_na(type) %>%
  
  #Calculate % of recipes
  #Add number of recipes from each country
  inner_join(., metadata %>% group_by(group) %>% summarise(number_of_recipes = n()) %>% ungroup) %>%
  mutate(pct = n/number_of_recipes*100) %>%
  #Add empty 'game' row for US to use in ggplot
  add_row(group = 'USA', type = 'Game', pct = 0) %>%
  #Set order for plot
  mutate(type = factor(type, levels = c('Beef', 'Lamb', 'Game', 'Pork', 'Poultry', 'Lean fish', 'Oily fish', 'Shellfish', 'Vegan', 'Vegetarian')))

#Plot
plots$barplots <- list()

plots$barplots$protein_source <- ggplot(temp, aes(x = type, fill = group, y = pct)) +
  geom_bar(stat = 'Identity', position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label = paste0(round(pct, 0), '%')), position = position_dodge2(width = 0.9, preserve = "single"), vjust = -0.2, size = 2) +
  scale_fill_manual(values = various$country_colors$sample_group) +
  #Set y label to percent
  scale_y_continuous(limits = c(0,50), labels = function(x) paste0(x, "%")) +
  labs(
    fill = 'Country',
    x = '',
    y = 'Percentage of recipes',
    title = 'Protein sources used in the recipes') +
  theme(legend.position = c(0.1,0.85), 
        legend.background = element_rect(color = "black"))

save_plot('./thesis/images/protein_source_bar.png', plots$barplots$protein_source, nrow = 1.7, ncol = 1.7)

### Figure 1B----
plots$violinbox$foodgroups <- list()

#Calculate
temp <- tidy_ingredients %>%
  filter(Foodgroup == 'Meat and\nmeat products') %>%
  #pivot wider to get Amounts column
  pivot_wider(names_from = feature,
              values_from = value) %>%
  group_by(group, sample_id, Foodgroup) %>%
  summarise(value = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
  #Turn into grams, not fraction per 100 g
  mutate(value = value*100)

#Save for source materials
write_csv(temp, './Supplementary/SourceFig1B.csv')

#Plot
plots$violinbox$foodgroups$meat_products <- ggplot(temp, aes(x = Foodgroup, y = value, color = group)) +
  geom_half_violin() + 
  geom_half_boxplot(side = 'r', outlier.shape = NA) +
  geom_quasirandom(alpha = 0.4, dodge.width = 0.75, size = 0.5) +
  #Make nicer
  scale_color_manual(values = various$country_colors$sample_group) +
  labs(y = 'Percentage of recipe weight') +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  #Set y axis scale to percent
  #Add n below violin/boxplot
  geom_text(data = temp %>% group_by(group, Foodgroup) %>% summarise(n = n()) %>% mutate(value = -7, n = paste0("n = ", n)),
            aes(label = n), position = position_dodge2(width = 0.75), size = 2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

plots$violinbox$foodgroups$meat_products
### Figure 1C----
temp <- various$recipe_protein_source %>%
  inner_join(tidy_recipes %>% filter(feature %in% c('CO2', 'Landuse'))) %>%
  #Add a fake US recipe with game to get the right width of the quasirandom points
  add_row(group = 'US', sample_id = 'fake', type = 'Game', feature = 'CO2', value = 3) %>%
  add_row(group = 'US', sample_id = 'fake', type = 'Game', feature = 'Landuse', value = 5) %>%
  #Set type order for plot and use nicer feature names
  mutate(type = factor(type, levels = c('Beef', 'Lamb', 'Game', 'Pork', 'Poultry', 'Lean fish', 'Oily fish', 'Shellfish', 'Vegan', 'Vegetarian')),
         feature = feature %>%
           str_replace('CO2', 'Kilo CO<sub>2</sub> equivalents/100g') %>%
           str_replace('Landuse', 'm<sup>2</sup>/100g'),
         #Make a column to set alpha levels for plot, fake data will be invisible
         alpha_level = case_when(
           sample_id == 'fake' ~ 'Invisible',
           TRUE ~ 'Visible')
  ) %>% arrange(group)

#Save for source data
write_csv(temp, './Supplementary/SourceFig1C.csv')

plots$violinbox$recipe_protein_sources <- ggplot() +
  #Boxplot
  geom_boxplot(data = temp %>% filter(!sample_id == 'fake'), aes(x = type, y = value, color = group),
               position = position_dodge(preserve = "single"), outlier.shape = NA) +
  scale_color_manual(values = various$country_colors$sample_group) +
  #Points
  geom_quasirandom(data = temp, aes(x = type, y = value, color = group, alpha = alpha_level),
                   dodge.width = 0.75, size = 0.5, varwidth = FALSE) +
  #Add n below violin/boxplot
  geom_text(
    #Count n and add an arbitrary y value
    data = temp %>% select(-c(feature, value)) %>% distinct() %>%
      group_by(group, type) %>% summarise(n = n()) %>% mutate(value = -0.5, n = paste0("n =\n", n)),
            aes(label = n, x = type, y = value, color = group), position = position_dodge2(width = 0.75), size = 2, lineheight = 0.9) +
  scale_alpha_discrete(range = c(0, 0.5)) + #Set fake data "invisible" to have zero alpha
  facet_wrap(~feature, scales = 'free', nrow = 2, labeller = as_labeller(c(
    `Kilo CO<sub>2</sub> equivalents/100g` = "Greenhouse gas emissions in kilo CO<sub>2</sub> equivalents/100g",
    `m<sup>2</sup>/100g` = "Land use in m<sup>2</sup>/100g"))) +
  labs(y = ' ',
       x = 'Protein source',
       title = "Environmental impact of recipes by their source of protein",
       color = 'Country') +
  theme(legend.position = 'bottom') +
  #Remove alpha legend
  guides(alpha = "none")

## Whole Figure 1---
plots$article <- list()

#Add label to embedded plot with amount of meat per 100 g
temp <- plot_grid(plots$violinbox$foodgroups$meat_products +
                    labs(y = 'Recipe weight') +
                    scale_x_discrete(position = "top") +
                    #Change text sizes
                    theme(
                      #Text size
                      #Fake title
                      axis.text.x  = element_text(size = 7),
                      #Fake y axis
                      title = element_text(size = 6),
                      axis.text.y = element_text(size = 5)) +
                    #Set y limit
                    ylim(-12, NA),
                  labels = 'b')
plots$article$protein_sources <- plot_grid(plots$barplots$protein_source + theme(legend.position = c(0.075, 0.8),
                                                                                 #Text size
                                                                                 title = element_text(size = 7),
                                                                                 legend.text = element_text(size = 5),
                                                                                 legend.title = element_text(size = 6),
                                                                                 axis.text = element_text(size = 5),
                                                                                 #Reduce legend key size
                                                                                 legend.key.size = unit(0.4, 'cm'),
                                                                                 #Reduce margin
                                                                                 plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm")
                                                                                 ) +
                                             annotation_custom(ggplotGrob(temp),
                                                               xmin = 6.2, xmax = 9.8, ymin = 18, ymax = 53),
                                           plots$violinbox$recipe_protein_sources + theme(legend.position = 'none',
                                                                                          #Axis text
                                                                                          strip.text.x = element_markdown(
                                                                                            size = 6,
                                                                                            margin = margin(0.1, 0, 0.1, 0, "cm")),
                                                                                          title = element_text(size = 7),
                                                                                          axis.text = element_text(size = 5),
                                                                                          #Reduce margin
                                                                                          plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm")
                                                                                          ) +
                                             #Set y limit
                                             ylim(-0.7, NA),
                                           nrow = 2,
                                           labels = c('a', 'c'),
                                           rel_heights = c(3.5,6)
)

plots$article$protein_sources

save_plot('./Figure1.pdf', plots$article$protein_sources, base_width = 180, base_height = 185, units = "mm")

## Tables----
# Stats----
#Significantly differences from dunn test
temp <- stats$dunn_test %>% filter(p.adj <0.05)

#Format the relevant data
stat_table <- list(
  
  'descriptive_stats' = descriptive_stats_country %>% filter(feature %in% temp$feature) %>%
    #Create one row for each feature, with the median + interquartile range for each country
    mutate(median_iqr = paste0(round(median, 1), ' (', round(q1, 1), ', ', round(q3, 1), ')')) %>%
    select(feature, group, median_iqr) %>%
    pivot_wider(.,
                names_from = group,
                values_from = median_iqr),
  
  # #Get the p value and effect size for each feature
  # 'kruskal_wallis' = stats$kruskal_wallis %>% filter(feature %in% temp$feature) %>%
  #   #Add effect size
  #   inner_join(., stats$kruskal_wallis_effectsize, by = 'feature') %>%
  #   #Select columns to keep
  #   select(feature, effsize, conf.low, conf.high, p.adj) %>%
  #   #Use <0.05, <0.01 and <0.001 from p value
  #   mutate(p.adj = case_when(
  #     p.adj < 0.001 ~ '<0.001',
  #     p.adj < 0.01 ~ '<0.01',
  #     p.adj < 0.05 ~ '<0.05',
  #     TRUE ~ paste0(as.character(round(p.adj, digits = 2)))),
  #     `Effect size \n (95% ci)` = paste0(round(effsize, digits = 2), ' (', round(conf.low, 2), '-', round(conf.high, 2), ')')
  #     ) %>%
  #   rename(`Adj. p-value` = p.adj) %>%
  #   select(-c(effsize, conf.low, conf.high)),
  
  #Get the pairwise comparison and adjusted p value for each feature
  'dunn' = stats$dunn_test %>% filter(p.adj <0.05) %>%
    #Include all pairwise compairsons for the significant different features on kruskal wallis for revised table
    select(feature) %>% unique() %>% left_join(., stats$dunn_test) %>%
    mutate(Pairwise = paste0(group1, ' - ', group2),
           
           #For revised table, use * instead of the adjusted p values
           Pairwise = case_when(
             
             p.adj < 0.001 ~ paste0("<strong>", Pairwise, "***</strong>"),
             p.adj < 0.01 ~ paste0("<strong>", Pairwise, "**</strong>"),
             p.adj < 0.05 ~ paste0("<strong>", Pairwise, "*</strong>"),
             TRUE ~ Pairwise
             
           )) %>%
    select(feature, Pairwise) %>% #, p.adj) %>%
    #Use <0.05, <0.01 and <0.001 from p value
    # mutate(p.adj = case_when(
    #   p.adj < 0.001 ~ '<0.001',
    #   p.adj < 0.01 ~ '<0.01',
    #   p.adj < 0.05 ~ '<0.05'
    # )) %>%
    # rename(`Adj. p-value` = p.adj) %>%
    #One row for each feature
    group_by(feature) %>%
    summarise(Pairwise = paste0(Pairwise, collapse = '<br><br>')) %>%#,
    #`Adj. p-value` = paste0(`Adj. p-value`, collapse = '<br><br>')) %>%
    ungroup()
  
) %>%
  #Finished table
  reduce(full_join, by = 'feature') %>%
  #Clean up some names and reorder for the finished table
  mutate(feature = factor(feature, levels = c(
    #Environmental impact
    'CO2', 'Landuse',
    #Healthiness indicators
    'inverted_nutriscore', 'inverted_traffic_score', 'nnr_score', 'who_score',
    #Macros
    'Protein', 'Dietary fibre', 'Sugar',
    #Vitamins
    'Vitamin D', 'Vitamin C', 'Thiamin', 'Niacin', 'Folate', 'Vitamin B12',
    #Minerals
    'Copper', 'Iodine', 'Iron', 'Potassium', 'Selenium', 'Zinc'))) %>%
  arrange(feature) %>%
  rename(Feature = feature) %>%
  mutate(Feature = Feature %>%
           str_replace('inverted_nutriscore', 'Inv. Nutriscore') %>%
           str_replace('inverted_traffic_score', 'Inv. Traffic Light') %>%
           str_replace('who_score', 'WHO Score') %>%
           str_replace('nnr_score', 'NNR Score') %>%
           str_replace('Carbo', 'Carbohydrates E%') %>%
           str_replace('Sugar', 'Sugar E%') %>%
           str_replace('Fat', 'Fat E%') %>%
           str_replace('SatFa', 'Saturated Fat E%') %>%
           str_replace('Protein', 'Protein E%') %>%
           str_replace('Dietary fibre', 'Dietary fibre g/MJ') %>%
           str_replace('Kilocalories', 'Kilocalories/100g') %>%
           str_replace('CO2', 'kg CO2 equivalents') %>%
           str_replace('Landuse', 'Landuse m2/year')) %>%
  mutate(Feature = case_when(
    Feature %in% c(#Vitamins
      'Vitamin D', 'Vitamin C', 'Thiamin', 'Niacin', 'Folate', 'Vitamin B12',
      #Minerals
      'Copper', 'Iodine', 'Iron', 'Potassium', 'Selenium', 'Zinc') ~ paste0(Feature, ' % of RDI'),
    TRUE ~ Feature
  ))

#All features
#Format the relevant data
all_stats_table <- list(
  
  'descriptive_stats' = descriptive_stats_country %>%
    #Create one row for each feature, with the median + interquartile range for each country
    mutate(median_iqr = paste0(round(median, 1), ' (', round(q1, 1), ', ', round(q3, 1), ')')) %>%
    select(feature, group, median_iqr) %>%
    pivot_wider(.,
                names_from = group,
                values_from = median_iqr),
  
  #Get the p value and effect size for each feature
  'kruskal_wallis' = stats$kruskal_wallis %>%
    #Add effect size
    inner_join(., stats$kruskal_wallis_effectsize, by = 'feature') %>%
    #Select columns to keep
    select(feature, effsize, conf.low, conf.high, p.adj) %>%
    #Use <0.05, <0.01 and <0.001 from p value
    mutate(p.adj = case_when(
      p.adj < 0.001 ~ '<0.001',
      p.adj < 0.01 ~ '<0.01',
      p.adj < 0.05 ~ '<0.05',
      TRUE ~ paste0(as.character(round(p.adj, digits = 2)))),
      `Effect size \n (95% ci)` = paste0(round(effsize, digits = 2), ' (', round(conf.low, 2), '-', round(conf.high, 2), ')')
    ) %>%
    rename(`Adj. p-value` = p.adj) %>%
    select(-c(effsize, conf.low, conf.high)),
  
  #Get the pairwise comparison and adjusted p value for each feature
  'dunn' = stats$all_dunn_test %>%
    mutate(Pairwise = paste0(group1, ' - ', group2)) %>%
    select(feature, Pairwise, p.adj) %>%
    #Use <0.05, <0.01 and <0.001 from p value
    mutate(p.adj = case_when(
      p.adj < 0.001 ~ '<0.001',
      p.adj < 0.01 ~ '<0.01',
      p.adj < 0.05 ~ '<0.05',
      TRUE ~ paste0(as.character(round(p.adj, digits = 2))))
    ) %>%
    rename(`Adj. p-value` = p.adj) %>%
    #One row for each feature
    group_by(feature) %>%
    summarise(Pairwise = paste0(Pairwise, collapse = '<br><br>'),
              `Adj. p-value` = paste0(`Adj. p-value`, collapse = '<br><br>')) %>% ungroup()
) %>%
  
  #Finished table
  reduce(full_join, by = 'feature') %>%
  #Clean up some names and reorder for the finished table
  mutate(feature = factor(feature, levels = c(
    #Environmental impact
    'CO2', 'Landuse',
    #Healthiness indicators
    'inverted_nutriscore', 'inverted_traffic_score', 'nnr_score', 'who_score',
    #Macros
    'Fat', 'SatFa', 'Protein', 'Carbo', 'Dietary fibre', 'Sugar',
    #Vitamins
    'Vitamin A', 'Retinol', 'Beta-carotene', 'Vitamin D', 'Vitamin E', 'Vitamin C', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin B6', 'Folate', 'Vitamin B12',
    #Minerals
    'Calcium', 'Copper', 'Iodine', 'Iron', 'Magnesium', 'Phosphorus', 'Potassium', 'Selenium', 'Sodium', 'Zinc',
    #DIV
    'Kilocalories'))) %>%
  arrange(feature) %>%
  rename(Feature = feature) %>%
  mutate(Feature = Feature %>%
           str_replace('inverted_nutriscore', 'Inv. Nutriscore') %>%
           str_replace('inverted_traffic_score', 'Inv. Traffic Light') %>%
           str_replace('who_score', 'WHO Score') %>%
           str_replace('nnr_score', 'NNR Score') %>%
           str_replace('Carbo', 'Carbohydrates E%') %>%
           str_replace('Sugar', 'Sugar E%') %>%
           str_replace('Fat', 'Fat E%') %>%
           str_replace('SatFa', 'Saturated Fat E%') %>%
           str_replace('Protein', 'Protein E%') %>%
           str_replace('Dietary fibre', 'Dietary fibre g/MJ') %>%
           str_replace('Kilocalories', 'Kilocalories/100g') %>%
           str_replace('CO2', 'kg CO2 equivalents') %>%
           str_replace('Landuse', 'Landuse m2/year')) %>%
  mutate(Feature = case_when(
    Feature %in% c(
      #Vitamins
      'Vitamin A', 'Retinol', 'Beta-carotene', 'Vitamin D', 'Vitamin E', 'Vitamin C', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin B6', 'Folate', 'Vitamin B12',
      #Minerals
      'Calcium', 'Copper', 'Iodine', 'Iron', 'Magnesium', 'Phosphorus', 'Potassium', 'Selenium', 'Sodium', 'Zinc'
    ) ~ paste0(Feature, ' % of RDI'),
    TRUE ~ Feature
  ))

#Format table and save for supplementary
t <- all_stats_table %>%
  #Escape % for LaTeX
  mutate(Feature = Feature %>%
           str_replace("%", "\\\\%") %>%
           str_replace("CO2", "CO\\\\textsubscript{2}") %>%
           str_replace("m2", "m\\\\textsuperscript{2}"),
         `Adj. p-value.y` = str_replace_all(`Adj. p-value.y`, '<br><br>', '\n'),
         Pairwise = str_replace_all(Pairwise, '<br><br>', '\n')
  ) %>%
  mutate(`Adj. p-value.y` = linebreak(`Adj. p-value.y`),
         Pairwise = linebreak(Pairwise)) %>%
  #Format table
  kbl(booktabs = TRUE, linesep = "", align = c("l", "c", "c", "c", "c", "c", "l", "l"), valign = "b",
      col.names = c(" ", "Norway", "UK", "USA", "Adj. \\textit{p}-value", "Effect size (95\\% ci)", "Pairwise", "Adj. \\textit{p}-value"),
      caption = "Kruskal Wallis and 
      Dunn test results.", escape = FALSE, format = "latex") %>%
  kable_styling(full_width = FALSE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Median (IQR)" = 3, "Kruskal-Wallis test, BH corrected" = 2, "Dunn test, BH corrected" = 2)) %>%
  column_spec(7, width = "3cm", latex_valign = "p") %>%
  column_spec(8, width = "2.5cm", latex_valign = "p") %>%
  #Group rows together
  pack_rows(., 'Environmental impact', 1, 2) %>%
  pack_rows(., 'Healthiness indicators', 3, 6) %>%
  pack_rows(., 'Macronutrients', 7, 12) %>%
  pack_rows(., 'Vitamins', 13, 24) %>%
  pack_rows(., 'Minerals', 25, 34) %>%
  pack_rows(., 'Energy', 35, 35) %>%
  #Add footnote explaining abreviations
  footnote(., general = "Abbreviations used: Inv = Inverted, NNR = Nordic Nutrition Recommendations, WHO = World Health Organization, E% = Percentage of energy, MJ = Megajoule, RDI = Recommended daily intake.", threeparttable = TRUE)

save_kable(t, 'SupTable11.pdf')

## Other data ----
#Guidelines, nutriscore and trafficlights
#Guidelines and traffic lights
guidelines_trafficlights <- read_csv2('./Data/health_indicators/guidelines_trafficlights.csv') %>%
  #rename(Feature = X1) %>%
  replace(is.na(.), ' ')

#Save as source data
write_csv(guidelines_trafficlights, "./Supplementary/Supplementary_Table9.csv")

#Nutriscore
nutriscore_points <- read_csv2('./Data/health_indicators/nutriscore_points.csv') %>%
  #select(-Points_1) %>%
  replace(is.na(.), ' ')

#Save as source data
write_csv(nutriscore_points, "./Supplementary/Supplementary_Table10.csv")

#Revised latex table 
stat_table  %>%
  #Escape % for LaTeX
  mutate(Feature = Feature %>%
           str_replace("%", "\\\\%") %>%
           str_replace("CO2", "CO\\\\textsubscript{2}") %>%
           str_replace("m2", "m\\\\textsuperscript{2}"),
         Pairwise = Pairwise %>%
           str_replace_all("<strong>", "\\\\textbf{") %>%
           str_replace_all("</strong>", "}") %>%
           str_replace_all('<br><br>', '\n')
  ) %>%
  mutate(Pairwise = linebreak(Pairwise)) %>%
  #Format table
  kbl(booktabs = TRUE, linesep = "", align = c("l", "c", "c", "c", "l"), valign = "b",
      col.names = c(" ", "Norway", "UK", "USA", "Pairwise"),
      caption = "Dunn test results.", escape = FALSE, format = "latex") %>%
  kable_styling(full_width = FALSE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Median (IQR)" = 3, "Dunn test, BH corrected" = 1)) %>%
  #column_spec(7, width = "3cm", latex_valign = "p") %>%
  column_spec(5, width = "2.5cm", latex_valign = "p") %>%
  #Group rows together
  pack_rows(., 'Environmental impact', 1, 2) %>%
  pack_rows(., 'Healthiness indicators', 3, 5) %>%
  pack_rows(., 'Macronutrients', 6, 8) %>%
  pack_rows(., 'Vitamins', 9, 14) %>%
  pack_rows(., 'Minerals', 15, 19) %>%
  #Add footnote explaining abreviations
  footnote(., general = "Abbreviations used: Inv = Inverted, NNR = Nordic Nutrition Recommendations, WHO = World Health Organization, E% = Percentage of energy, MJ = Megajoule, RDI = Recommended daily intake.", threeparttable = TRUE)   


