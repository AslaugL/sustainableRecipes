devtools::load_all(path = '.')

# install 'arcdiagram' (development version)
devtools::install_github('gastonstat/arcdiagram')

library(arcdiagram)

## Setup ----
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
  arrange(group) #Set order of countries for plot legends

data_ingredients <- readRDS('./Data/output/ingredients_for_analysis.Rds') %>% #Nutrient content/sustainability indicators for every ingredient pr 100g recipe
  rename(Foodgroup = L1) %>%
  select(-FoodEx2) %>%
  #Remove non-dinner recipes
  filter(!sample_id %in% c('Baklava (Turkey)', 'Sausage Lunch', 'Half-fermented trout', 'Straight trout', 'Fruit Package')) %>%
  arrange(group) #Set order of countries for plot legends

#Turn tidy
tidy_recipes <- data_recipes %>%
  pivot_longer(.,
               cols = -c(sample_id, group, Source),
               names_to = 'feature',
               values_to = 'value')

tidy_ingredients <- data_ingredients %>%
  pivot_longer(.,
               cols = -c(Ingredients, sample_id, group, Source, Foodgroup, Amounts),
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
metadata <- readRDS('./Data/output/recipe_metadata.Rds') %>% arrange(group)
#remove Source from df
data_recipes <- data_recipes %>% select(-Source)

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
  #Without legend
  temp <- plot_grid(plots$data_completeness$nutrients + theme(legend.position  = 'none'),
                    plots$data_completeness$environment + theme(legend.position = 'none'),
                    
                    #Labels, number of rows and relative width of plots
                    labels = c('A', 'B'),
                    nrow = 1,
                    rel_widths = c(1,1))
  #With legend
  plots$final$data_completeness <- plot_grid(temp, plot_legends$data_completeness,
                                             nrow = 2,
                                             rel_heights = c(9,1))
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
            './Supplementary/healthiness_indicator_scores.csv')

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
    #Unite with deature column to use as column names later
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

write_csv(temp2, './Supplementary/calculating_healthiness_indicator_values.csv')

## Statistical analyses----
#Prep
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

write_csv(temp2, './Supplementary/rdi_micronutrients_100g.csv')

#Energy percentage of macronutrients in each recipe
various$with_energy_pct_densityMJ <- data_recipes %>%
  #Energy percent for macros, density pr MJ for fibre
  calculateEnergypercentDensity() %>%
  mutate(value = case_when(
    feature != 'Dietary fibre' ~ energy_percent,
    feature == 'Dietary fibre' ~ densityMJ
  )) %>% select(-c(densityMJ, energy_percent))

#Dataframe with the variables to do stats analyses on
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


# Some descriptive stats----
descriptive_stats_country <- run_stats %>%
  group_by(group, feature) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            q3 = quantile(value, 0.75),
            max = max(value),
            mean = mean(value))

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

#Descriptive stats of missing data
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

#Count the number of recipes in each healthiness category
temp <- health_indicators %>%
  filter(feature != 'inverted_nutriscore') %>%
  mutate(value = as.character(value)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  pivot_longer(.,
               cols = -c(sample_id, group, Source),
               names_to = 'feature',
               values_to = 'value') %>%
  group_by(feature, value, group) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  #Calculate pct, add total number of recipes
  inner_join(., data_recipes %>% select(sample_id, group) %>% group_by(group) %>% summarise(sum = n()) %>% ungroup()) %>%
  #Add totals for all countries for each indicator
  bind_rows(., health_indicators %>%
               filter(feature != 'inverted_nutriscore') %>%
               mutate(value = as.character(value)) %>%
               pivot_wider(.,
                           names_from = feature,
                           values_from = value) %>%
               pivot_longer(.,
                            cols = -c(sample_id, group, Source),
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
           str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
           str_replace('who_score', 'World Health Organization\nRecommendations') %>%
           str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model') %>%
           str_replace('nutriscore_letter', 'Nutriscore')) %>%
  select(feature, group, value, pct) %>%
  #Split on feature to format each indicator separately
  group_by(feature) %>% group_split() %>%
  #Pivot wider
  lapply(., function(x) {x %>% pivot_wider(., names_from = c(group, feature), values_from = pct)})

#Add empty rows to nutriscore before col_bind
temp[[3]] <- temp[[3]] %>%
  add_row() %>%
  add_row() %>%
  #Replace NA with empty space, add % for UK's E scores
  mutate(UK_Nutriscore = case_when(
    value == 'E' ~ '0.0 %',
    TRUE ~ UK_Nutriscore
  )) %>%
  replace(is.na(.), ' ')

#Order by descending score
temp[[2]] <- temp[[2]] %>%
  arrange(desc(value))
temp[[4]] <- temp[[4]] %>%
  arrange(desc(value))
temp[[1]] <- temp[[1]] %>%
  mutate(value = factor(value, levels = c(12, 11, 10, 9, 8 , 7, 6))) %>%
  arrange(value)

#Add 0 % to other indicators where no recipes has that score, and order from best to lowest score
temp <- lapply(temp, function(x) {x %>% replace(is.na(.), '0.0 %')})

pct_recipes_healthiness_scores <- bind_cols(temp) %>%
  #Fix column names
  rename(
    `Score_Inverted Multiple\nTraffic Light Model` = value...1,
    `Score_Nordic Nutritional\nRecommendations` = value...6,
    `Score_Nutriscore` = value...11,
    `Score_World Health Organization\nRecommendations` = value...16
  ) %>%
  select(ends_with('Inverted Multiple\nTraffic Light Model'), ends_with('Nutriscore'), ends_with('Nordic Nutritional\nRecommendations'), ends_with('World Health Organization\nRecommendations'))


#Count the number of recipes that are a source of nutrients (>15% of RDI for micronutrients, >12% of energy for protein) and a good source (>30% of RDI for micronutrients, >20 % energy from protein)
source_of_nutrients_table <- various$with_RDI %>%
  #Add protein
  bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
  #fill missing country and source information
  group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
  #Find sources and good sources
  mutate(nutrient_source = case_when(
    #Protein
    feature == 'Protein' & value > 20 ~ 'good_source',
    feature == 'Protein' & value > 12 ~ 'source',
    #Fibre
    feature == 'Dietary fibre' & value > 6 ~ 'good_source',
    feature == 'Dietary fibre' & value > 3 ~ 'source',
    #Micronutrients
    feature != 'Protein' & value >= 30 ~ 'good_source',
    feature != 'Protein' & value >= 15 ~ 'source'
  )) %>%
  #Filter non-source nutrients
  filter(!is.na(nutrient_source)) %>%
  #Count the number of recipes that are either sources or good sources
  group_by(group, feature, nutrient_source) %>%
  summarise (n = n()) %>%
  #Format to have the percent of recipes that are sources with the percent of recipes that are good sources in ()
  ungroup() %>%
  pivot_wider(., names_from = nutrient_source, values_from = n) %>%
  rename(temp = source) %>%
  replace_na(list(temp = 0, good_source = 0)) %>%
  mutate(source = temp + good_source) %>% #Total number of recipes that are sources of the nutrient
  select(-temp) %>%
  #Add the total number of recipes from each country
  inner_join(., data_recipes %>% select(sample_id, group) %>% group_by(group) %>% summarise(n = n()) %>% ungroup()) %>%
  #Add the number of recipes for all countries that are sources
  bind_rows(., various$with_RDI %>%
              #Add protein
              bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
              #fill missing country and source information
              group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
              #Find sources and good sources
              mutate(nutrient_source = case_when(
                #Protein
                feature == 'Protein' & value > 20 ~ 'good_source',
                feature == 'Protein' & value > 12 ~ 'source',
                #Fibre
                feature == 'Dietary fibre' & value > 6 ~ 'good_source',
                feature == 'Dietary fibre' & value > 3 ~ 'source',
                #Micronutrients
                feature != 'Protein' & value >= 30 ~ 'good_source',
                feature != 'Protein' & value >= 15 ~ 'source'
              )) %>%
              #Filter non-source nutrients
              filter(!is.na(nutrient_source)) %>%
              #Count the number of recipes that are either sources or good sources
              group_by(feature, nutrient_source) %>%
              summarise (n = n()) %>%
              #Format to have the percent of recipes that are sources with the percent of recipes that are good sources in ()
              ungroup() %>%
              pivot_wider(., names_from = nutrient_source, values_from = n) %>%
              rename(temp = source) %>%
              replace_na(list(temp = 0, good_source = 0)) %>%
              mutate(source = temp + good_source,
                     group = 'All countries',
                     n = nrow(data_recipes)) %>%
              select(-temp)) %>%
  #Calculate %
  mutate(pct_source = round(source/n*100),
         pct_good_source = round(good_source/n*100)) %>%
  #Add this information to one column and pivot wider
  mutate(final = paste0(pct_source, '% (', pct_good_source, '%)')) %>%
  select(group, feature, final) %>%
  pivot_wider(names_from = group,
              values_from = final) %>%
  rename(Nutrient = feature) %>%
  #Reorder for table
  mutate(Nutrient = factor(Nutrient, levels = c(
    #Macros
    'Protein', 'Dietary fibre',
    #Fat soluble vitamins
    'Vitamin A', 'Retinol', 'Beta-carotene', 'Vitamin D', 'Vitamin E',
    #Water soluble vitamins
    'Vitamin C', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin B6', 'Folate', 'Vitamin B12',
    #Minerals
    'Calcium', 'Iron', 'Zinc', 'Magnesium', 'Potassium', 'Selenium', 'Iodine', 'Sodium',
    'Phosphorus', 'Copper'))) %>%
  arrange(Nutrient)

# Kruskal Wallis and dunn----
stats <- list(
  
  'kruskal_wallis' = run_stats %>%
    group_by(feature) %>%
    kruskal_test(., value ~ group) %>%
    adjust_pvalue(., method = 'BH'),
  
  'kruskal_wallis_effectsize' = run_stats %>%
    group_by(feature) %>%
    kruskal_effsize(value ~ group)#,
                    #ci = TRUE) #Include to calculate effect sizes used in final stat table
  )

#Filter out features that were significantly different in the kruskal wallis
temp <- stats$kruskal_wallis %>%
  filter(p.adj <0.05) %>%
  select(feature) %>% inner_join(run_stats)

stats$dunn_test <- temp %>%
    group_by(feature) %>%
    dunn_test(value ~ group, detailed = TRUE) %>%
    adjust_pvalue(., method = 'BH') %>%
    #Add ** markings for significance leven, and y position to create p-value brackets in plots later
    add_significance() %>%
    add_y_position(scales = 'free_y')
  

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
           str_replace('keyhole_certified', 'Keyhole') %>%
           str_replace('CO2', 'CO<sub>2</sub>') %>%
           str_replace('Landuse', 'm<sup>2</sup>')
  ) %>%
  inner_join(., metadata) %>%
  pivot_wider(., names_from = 'feature', values_from = 'value')

#Health indicators with sustainability indicators
plots$correlations$healthVSsustainability <- ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = 32:37,
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)
  
  #Save
  save_plot('./thesis/images/healthVSsustainability.png', plots$correlations$healthVSsustainability,
            ncol = 2.2, nrow = 2.2)

#Sustainability vs energy contributing macros
plots$correlations$energyVSsustainability <- ggpairs(temp %>% select(-sample_id) %>%
          #Rename some column names
          rename(
            `Carbs E%` = Carbo,
            `Sugar E%` = Sugar,
            `Protein E%` = Protein,
            `Sat. Fat E%` = SatFa,
            `Fat E%` = Fat,
            `Fibre g/MJ` = `Dietary fibre`,
            `kcal/100g` = Kilocalories
          ),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(25:31, 36,37),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

  #Save
  save_plot('./thesis/images/energyVSsustainability.png', plots$correlations$energyVSsustainability,
             ncol = 2.3, nrow = 2.3)

#Sustainability vs mineral content, split in two 
plots$correlations$mineralsVSsustainability1 <- ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(4,5, 7:9, 36,37),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

plots$correlations$mineralsVSsustainability2 <- ggpairs(temp %>% select(-sample_id),
                                                        mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                        columns = c(11, 12, 15, 16, 24, 36,37),
                                                        upper = list(continuous = myCorrelations_textsize),
                                                        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

  #Save
  save_plot('./thesis/images/mineralsVSsustainability1.png', plots$correlations$mineralsVSsustainability1,
             ncol = 2.2, nrow = 2.2)
  save_plot('./thesis/images/mineralsVSsustainability2.png', plots$correlations$mineralsVSsustainability2,
            ncol = 2.2, nrow = 2.2)


#Sustainability vs vitamin content, split in two
plots$correlations$vitaminsVSsustainability1 <- ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = c(18,3,13,22,23, 21, 36,37),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

plots$correlations$vitaminsVSsustainability2 <- ggpairs(temp %>% select(-sample_id),
                                                        mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                        columns = c(17,14,10,20,6,19, 36,37),
                                                        upper = list(continuous = myCorrelations_textsize),
                                                        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)


  #Save
  save_plot('./thesis/images/vitaminsVSsustainability1.png', plots$correlations$vitaminsVSsustainability1,
            ncol = 2.2, nrow = 2.2)
  save_plot('./thesis/images/vitaminsVSsustainability2.png', plots$correlations$vitaminsVSsustainability2,
            ncol = 2.2, nrow = 2.2)
  
  
  #Correlations discussed in article----
  plots$correlations$selected_nutrients <- ggpairs(temp %>% select(-sample_id) %>% rename(Fibre = `Dietary fibre`),
                                                   mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                   columns = c(25, 30, 3, 13, 28, 29, 8, 24, 36,37),
                                                   upper = list(continuous = myCorrelations_textsize),
                                                   lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
    scale_color_manual(values = various$country_colors$sample_group) +
    scale_fill_manual(values = various$country_colors$sample_group)
  
  #Save
  save_plot('./thesis/images/correlations_article.png', plots$correlations$selected_nutrients,
            ncol = 2.5, nrow = 2.5)

## Principal Component Analysis----
plots$pca <- list()
#Create a feature_anno column to color features by
temp <- run_stats %>%
    mutate(feature_anno = case_when(
      feature %in% c("Kilojoules", "Kilocalories") ~ "Energy",
      feature %in% minerals ~ 'Minerals',
      feature %in% vitamins ~ 'Vitamins',
      feature %in% energy_contributing ~ 'Macronutrient',
      feature %in% sustainability ~ 'Sustainability indicators',
      feature %in% health ~ 'Health indicators'
    ))

#Score plot
plots$pca$scores <- createPCA(temp %>% filter(sample_id != 'Homemade stick meat'),
                        interesting_samples = c(
                          #Samples driving pc1
                          'Calf liver with bulgur',
                          'Liver and bacon, onion gravy, smashed potato, dressed greens', "Pig's liver with sage and onions")) +
  #Legend at bottom
  theme(legend.position = 'bottom') %>% changeGGplotTxtSize(., 10) +
  #Fix lab
  labs(color = 'Country') #%>% changeGGplotTxtSize(.)

#Loadings plot
plots$pca$loadings <- createPCA(temp, plots = 'loadings', cutoff = 0.06) +
  theme(legend.position = 'bottom') %>% changeGGplotTxtSize(., 10)

plots$final$pca <- plot_grid(plots$pca$scores,
            plots$pca$loadings,
            #Labels, relative width/height of plots and number of grid columns
            labels = "AUTO",
            ncol = 1,
            rel_heights = c(1,1.1))

plots$final$pca

  #Save
  save_plot('./thesis/images/pca_plots.png', plots$final$pca,
            nrow = 2.5)

## Violin boxplots----
plots$violinbox <- list()

  #Color-free plotviolinbox function with outlier allowed
  plotViolinBox2 <- function(df, x = 'group', color = TRUE, r_statix = NULL) {
    
    if(!is.null(r_statix)){
      data <- inner_join(df, extractStats(r_statix, filterp = 'no'))
    }else{data <- df}
    
    #Color by color column if color is a column name in df, the mean-bars are positioned for when there is three groups
    if(is.character(color) & color %in% names(df)){
      
      #Color manual to color group by
      color_manual <- colorManuals(df = df, group = TRUE, feature_anno = FALSE)
      
      plot <- ggplot(data, aes(x = !!ensym(x), y = value, color = !!ensym(color))) +
        scale_color_manual(values = color_manual$sample_group) +
        
        #Add mock linetypes for the legend
        #geom_line(aes(linetype = 'dashed')) +
        geom_line(aes(linetype = 'solid')) +
        
        #Build the jitterplot or dotplot, half violin and half boxplot
        geom_half_violin(side = 'l') +
        geom_half_boxplot(side = 'r', outlier.size = 1) +
        #geom_quasirandom(width = 0.2, alpha = 0.5) +
        
        #Add the stippled line for the mean
        #stat_summary(fun = mean, geom = 'errorbar',
        #             aes(ymax = ..y.., ymin = ..y..),
        #             linetype = 'dashed',
        #             width = 0.7,
        #             position = position_dodge2(padding = 0.5, width = 15)) + #Add dashed line to indicate mean
        
        #Make it nicer
        labs(
          #x = !!ensym(x),
          y = 'Value',
        ) +
        
        scale_linetype_manual(name = 'Boxplot summary',
                              values = c('solid', 'dashed'),
                              labels = c('Median', 'Mean')) +
        
        #Set legend to the right
        theme(legend.position = 'right') +
        
        labs(
          color = color
        )
      
    } else if(is.character(color) & !color %in% names(df)){
      
      stop("Color must be a column in the dataframe")
      
    } else {
      
      plot <- ggplot(data, aes(x = !!ensym(x), y = value)) +
        
        #Add mock linetypes for the legend
        geom_line(aes(linetype = 'dashed')) +
        geom_line(aes(linetype = 'solid')) +
        
        #Build the jitterplot or dotplot, half violin and half boxplot
        geom_half_violin(side = 'l') +
        geom_half_boxplot(side = 'r', outlier.size = 1) +
        #geom_quasirandom(width = 0.2, alpha = 0.5) +
        
        #Add the stippled line for the mean
        stat_summary(fun = mean, geom = 'errorbar',
                     aes(ymax = ..y.., ymin = ..y..),
                     linetype = 'dashed',
                     width = 0.38, position = position_nudge(x = 0.185)) + #Add dashed line to indicate mean
        
        #Make it nicer
        labs(
          #x = !!ensym(x),
          y = 'Value',
        ) +
        
        scale_linetype_manual(name = 'Boxplot summary',
                              values = c('solid', 'dashed'),
                              labels = c('Median', 'Mean')) +
        
        #Set legend to the right
        theme(legend.position = 'right')
      
    }
    
    #Facet wrap if using r_statix results as names for each facet
    if(!is.null(r_statix)){
      plot <- plot + facet_wrap(~string, scales = 'free_y')
    }else{plot}
  }
  
# Differences between countries----
  # Environmental sustainability
  plots$violinbox$env_impact <- plotViolinBox(run_stats %>%
                  filter(feature %in% various$sustainability) %>%
                  mutate(feature = feature %>%
                           str_replace('CO2', 'Kilo CO<sub>2</sub> equivalents\nper 100g') %>%
                           str_replace('Landuse', 'm<sup>2</sup> per year\nper 100g'))) +
    facet_wrap(~feature, scale = 'free', ncol = 2) +
    labs(
      color = 'Country',
      x = 'Country',
      y = ''
    ) +
    #Add line and p value significance
    stat_pvalue_manual(stats$dunn_test %>%
                         filter(feature %in% various$sustainability) %>%
                         mutate(feature = feature %>%
                                  str_replace('CO2', 'Kilo CO<sub>2</sub> equivalents\nper 100g') %>%
                                  str_replace('Landuse', 'm<sup>2</sup> per year\nper 100g')) %>%
                         #Make some adjustments to the position of the pvalues
                         mutate(y.position = case_when(
                                    group1 == 'UK' & feature == 'm<sup>2</sup> per year\nper 100g' ~ y.position + 0.35,
                                    group1 == 'UK' & feature == 'Kilo CO<sub>2</sub> equivalents\nper 100g' ~ y.position + 0.4,
                                    group1 == 'Norway' & group2 == 'US' & feature == 'Kilo CO<sub>2</sub> equivalents\nper 100g' ~ y.position + 0.2,
                                    TRUE ~ y.position
                                    )),
                     label = "p.adj.signif", tip.length = 0.01, hide.ns = TRUE, bracket.nudge.y = 0.2) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    
    #Place legend at the bottom
    theme(legend.position = "bottom")
  
    #Save
    save_plot('./thesis/images/violinbox_env_impact.png', plots$violinbox$env_impact,
              ncol = 1.7,
              nrow = 1.5)

  #Per foodgroup
  temp <- tidy_ingredients %>%
    filter(feature %in% c('CO2', 'Landuse')) %>%
    group_by(sample_id, group, feature, Foodgroup) %>%
    summarise(value = sum(value, na.rm = TRUE))
  #CO2
  plotViolinBox(temp %>% filter(feature == 'CO2')) +
    facet_wrap(~Foodgroup, scale = 'free') +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Kilo CO2 equivalents'
    )
  #Landuse
    plotViolinBox(temp %>% filter(feature == 'Landuse')) +
      facet_wrap(~Foodgroup, scale = 'free') +
      labs(
        color = 'Country',
        x = 'Country',
        y = 'm2 per year'
      )

  # Health scores
    #numerical
    plots$violinbox$healthNum <- plotViolinBox(health_indicators %>%
                    filter(feature %in% various$health) %>%
                    filter(feature != 'keyhole_certified') %>%
                    mutate(feature = feature %>%
                           str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
                           str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
                           str_replace('who_score', 'World Health Organization\nRecommendations') %>%
                           str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model'))) + facet_wrap(~feature, scales = 'free') +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Score'
    ) +
    #Add lines and p-value significance
    stat_pvalue_manual(stats$dunn_test %>% 
                        filter(feature %in% various$health) %>%
                        filter(feature != 'keyhole_certified') %>%
                        mutate(feature = feature %>%
                                  str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
                                  str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
                                  str_replace('who_score', 'World Health Organization\nRecommendations') %>%
                                  str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model')
                                ) %>%
                         #Make some adjustments to pvalue position
                         mutate(y.position = case_when(
                           feature == 'Nordic Nutritional\nRecommendations' & group1 == 'UK' ~ y.position + 0.8,
                           feature == 'Nordic Nutritional\nRecommendations' & group1 == 'Norway' & group2 == 'US' ~ y.position + 0.4,
                           
                           TRUE ~ y.position
                         )),
                       label = "p.adj.signif", tip.length = 0.01, hide.ns = TRUE, bracket.nudge.y = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      theme(legend.position = "bottom")
    
    #Save
    save_plot('./thesis/images/violinbox_health_numerical.png', plots$violinbox$healthNum,
              ncol = 1.7, nrow = 2.4)

  # categorical, first format data
  temp <- health_indicators %>%
    filter(feature == 'keyhole_certified') %>%
    mutate(value = case_when(
      value == 0 ~ 'No keyhole',
      TRUE ~ 'Keyhole'
    )) %>%
    rename(Keyhole = value,
           Nutriscore = nutriscore_letter) %>%
    select(sample_id, Nutriscore, Keyhole) %>%
    pivot_longer(.,
                 cols = -sample_id,
                 names_to = 'feature',
                 values_to = 'value') %>% unique() %>% inner_join(., metadata) %>%
    group_by(group, feature, value) %>%
    #pct of recipe with each score
    summarise(n = n()) %>%
    mutate(pct = n / sum(n)*100) %>% ungroup() %>% select(-n) %>%
    #Add that Norway and UK have 0 recipes with nutriscore E for plot
    add_row(group = 'Norway', feature = 'Nutriscore', value = 'E', pct = 0) %>%
    add_row(group = 'UK', feature = 'Nutriscore', value = 'E', pct = 0)
  #plot
  plots$violinbox$healthCat <- ggplot(temp, aes(x = value, y = pct, fill = group)) +
    geom_bar(stat="identity", position = 'dodge') +
    scale_fill_manual(values = various$country_colors$sample_group) +
    facet_wrap(~feature, scales = 'free_x') +
  
    labs(
      y = '% of recipes',
      x = 'Score',
      fill = 'Country'
    )

    #Legend, if only plotting keyhole value, not nutriscore
    plot_legends$healthCat <- get_legend(plots$violinbox$healthCat, position = 'right')
    #Temp plot positioning plot and legend if only plotting keyhole 
    temp_plot <- plot_grid(plots$violinbox$healthCat + theme(legend.position = 'none'), plot_legends$healthCat, NULL,
                           nrow = 1,
                           rel_widths = c(0.44, 0.1, 0.46))

  #Plot both together
  plots$final$health_indicators <- plot_grid(plots$violinbox$healthNum,
                                             #plots$violinbox$healthCat, #Remove # to include in final plot
                                             temp_plot,
                                             ncol = 1,
                                             rel_heights = c(2:1), #Must be tweaked if healthcat is included
                                             labels = "AUTO")

  plots$final$health_indicators

    #Save
    save_plot('./thesis/images/violinbox_health_indicators.png', plots$final$health_indicators,
              ncol = 1.7, nrow = 3)

  # Minerals
  plotViolinBox(run_stats %>%
                  filter(feature %in% various$minerals)) + facet_wrap(~feature, scales = 'free') +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Percentage of RDI'
    ) + theme(legend.position="bottom") +
    #Add lines and p-value significance
    stat_pvalue_manual(stats$dunn_test %>% filter(feature %in% various$minerals),
                       label = "p.adj.signif", tip.length = 0.01, hide.ns = TRUE, bracket.nudge.y = 0.5) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))

  # Vitamins
  plotViolinBox(run_stats %>%
                filter(feature %in% various$vitamins)) +
    facet_wrap(~feature, scales = 'free') +
    labs(
      color = 'Country',
      x = 'Country',
      y = 'Percentage of RDI'
    ) + theme(legend.position="bottom")
  
  # Energy
  plotViolinBox(run_stats %>%
                filter(feature %in% various$energy_contributing) %>%
                mutate(feature = feature %>%
                         str_replace('Carbo', 'Carbohydrates E%') %>%
                         str_replace('Sugar', 'Sugar E%') %>%
                         str_replace('Fat', 'Fat E%') %>%
                         str_replace('SatFa', 'Saturated Fat E%') %>%
                         str_replace('Protein', 'Protein E%') %>%
                         str_replace('Dietary fibre', 'Dietary fibre g/MJ') %>%
                         str_replace('Kilocalories', 'Kilocalories/100g')
                   )) +
      facet_wrap(~factor(feature, levels = c('Kilocalories/100g', 'Dietary fibre g/MJ', 'Protein E%',
                                         'Carbohydrates E%', 'Sugar E%', 'Fat E%', 'Saturated Fat E%')) ,scale = 'free') +
      labs(
        color = 'Country',
        x = 'Country',
        y = 'Value'
      )

# All countries, all nutrients, healthiness indicators and environmental impact scores----
plots$all_values <- list()

  #minerals
  plots$all_values$minerals <- plotViolinBox2(run_stats %>% filter(feature %in% various$minerals) %>%
                                                #Change the order of some minerals so the names are readable in the plot
                                                mutate(feature = factor(feature, levels = c('Calcium', 'Copper', 'Iodine', 'Iron', 'Magnesium', 'Zinc', 'Potassium', 'Selenium', 'Sodium', 'Phosphorus')))
                                                , x = feature, color = FALSE) +
    coord_cartesian(ylim = c(0,100)) + labs(x = '', y = '') + theme(axis.title.x = element_blank()) +
    #Set y axis scale to percent
    scale_y_continuous(labels = function(x) paste0(x, "%"))

  #Vitamins
  plots$all_values$vitamins <- plotViolinBox2(run_stats %>% filter(feature %in% various$vitamins) %>%
                                                #Make long names go over two rows
                                                mutate(feature = feature %>%
                                                         str_replace('Vitamin ', 'Vitamin\n') %>%
                                                         str_replace('Beta-carotene', 'Beta\ncarotene')) %>%
                                                #Set order of vitamins to fat-soluble then water soluble
                                                mutate(feature = factor(feature, levels = c('Vitamin\nA', 'Beta\ncarotene', 'Retinol', 'Vitamin\nD', 'Vitamin\nE', 'Vitamin\nC', 'Thiamin', 'Riboflavin', 'Niacin', 'Vitamin\nB6', 'Folate', 'Vitamin\nB12')))
                                                , x = feature, color = FALSE) +
    coord_cartesian(ylim = c(0,100)) + labs(x = '', y = '') + theme(axis.title.x = element_blank()) +
    #Set y axis scale to percent
    scale_y_continuous(labels = function(x) paste0(x, "%"))

    #Title for plot_grid
    plots$titles$mineral_vitamin <- ggdraw() + 
      draw_label(
        "Micronutrients in % of recommended daily intake",
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
  
    #Legend
    plot_legends$all_values <- get_legend(plots$all_values$vitamins, position = 'bottom')

    #Both together
    plots$final$mineral_vitamin <- plot_grid(#plots$titles$mineral_vitamin,
                                             plots$all_values$minerals + theme(legend.position="none"),
                                             plots$all_values$vitamins+ theme(legend.position="none"),
                                             plot_legends$all_values,
                                             ncol = 1,
                                             rel_heights = c(#0.1,
                                                             0.45, 0.45, 0.1)) +
      #Shared y label
      draw_label("Percentage of recommended daily intake", x =  0, y =0.5, vjust= 1.5, angle=90)
  
    #Save
    save_plot('./thesis/images/all_minerals_vitamins.png', plots$final$mineral_vitamin,
              nrow = 1.5, ncol = 1.5)
    
    plots$final$mineral_vitamin

  #Energy providing nutrients
    #Kilocalories
    plots$all_values$energy_kilocalories <- plotViolinBox2(run_stats %>% filter(feature == 'Kilocalories'),
                                                           x = feature, color = FALSE) + labs(x = '', y = 'Kilocalories/100g') +
      theme(axis.title.x = element_blank())
  
    #Fibre
    plots$all_values$energy_fibre <- plotViolinBox2(run_stats %>% filter(feature == 'Dietary fibre'),
                                                    x = feature, color = FALSE) + labs(y = 'g/mJ', x = '') +
      theme(axis.title.x = element_blank())
  
    #Macros
    plots$all_values$energy_macros <- plotViolinBox2(run_stats %>% filter(feature %in% various$energy_contributing) %>%
                                                     #Only macros
                                                     filter(!feature %in% c('Dietary fibre', 'Kilocalories')) %>%
                                                     #Change order and names
                                                     mutate(feature = feature %>%
                                                              str_replace('Carbo', 'Carbohydrates') %>%
                                                              str_replace('SatFa', 'Saturated Fat')) %>%
                                                     mutate(feature = factor(feature, level = c('Carbohydrates', 'Sugar', 'Fat', 'Saturated Fat', 'Protein'))),
                                                     x = feature, color = FALSE) +
      labs(y = 'Percentage of energy', x = '') +
      theme(axis.title.x = element_blank()) +
      #Set y axis scale to percent
      scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%"))
  
    #Title
    plots$titles$energy <- ggdraw() + 
      draw_label(
        "Energy and macronutrient content",
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
  
    #All together, first without title or legend
    plots$final$energy1 <- plot_grid(plots$all_values$energy_kilocalories + theme(legend.position = "none"),
                                     plots$all_values$energy_macros + theme(legend.position = "none"),
                                     plots$all_values$energy_fibre + theme(legend.position = "none"),
                                     nrow = 1,
                                     rel_widths = c(0.2, 0.6, 0.2))
    
    #With title and/or legend
    plots$final$energy <- plot_grid(#plots$titles$energy, #Remove # to include title
                                    plots$final$energy1,
                                    plot_legends$all_values,
                                    ncol = 1,
                                    rel_heights = c(#0.1, #Remove # to include title
                                                    0.9,
                                                    0.1))
    #Save
    save_plot('./thesis/images/all_macros_cal.png', plots$final$energy,
              ncol = 1.5)
  

  #Health indicators
  plots$all_values$health <- plotViolinBox2(run_stats %>% filter(feature %in% various$health) %>%
                                              #Clean up names
                                              mutate(feature = feature %>%
                                                       str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
                                                       str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model') %>%
                                                       str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
                                                       str_replace('who_score', 'World Health Organization\nRecommendations')),
                                                x = feature, color = FALSE) + facet_wrap(~feature, scales = 'free') + labs(x = '', y = '') +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank())

  #Environmental impact
  plots$all_values$env <- plotViolinBox2(run_stats %>% filter(feature %in% various$sustainability) %>%
                                           #Clean up names
                                           mutate(feature = feature %>%
                                                    str_replace('CO2', 'Kilo CO<sub>2</sub> equivalents\nper 100g') %>%
                                                    str_replace('Landuse', 'm<sup>2</sup> per year\nper 100g')),
                                         x = feature, color = FALSE) + facet_wrap(~feature, scales = 'free', ncol = 1) + labs(x = '', y = '') +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank())

    #Title
    plots$titles$health_env <- ggdraw() + 
      draw_label(
        "Healthiness indicators and environmental impact",
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
  
    #Both together, first without title and legend
    plots$final$health_env1 <- plot_grid(plots$all_values$health + theme(legend.position = "none"),
                                         plots$all_values$env + theme(legend.position = "none"),
                                         rel_widths = c(2/3, 1/3),
                                         labels = "AUTO")
    #With legend and title
    plots$final$health_env <- plot_grid(#plots$titles$health_env, #Remove # to include title
                                        plots$final$health_env1,
                                        plot_legends$all_values,
                                        ncol = 1,
                                        rel_heights = c(
                                          #0.1, #remove # to include title
                                          0.9,0.1))
  
      #Save
      save_plot('./thesis/images/all_health_env.png', plots$final$health_env,
                ncol = 1.5,
                nrow = 1.5)
  
  
  #All together
  plots$final$all_values <- plot_grid(plots$final$energy,
                                      plots$final$health_env,
                                      plots$final$mineral_vitamin,
                                      ncol = 1,
                                      rel_heights = c(1/5, 2/5, 2/5))

  plots$final$all_values
  
# How the different countries scored on the healthiness indicators----
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
           group = factor(group, levels = c('Norway', 'UK', 'US'))) %>%
    arrange(group)
   
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
    scale_color_manual(values = various$country_colors$sample_group, breaks = c('Norway', 'UK', 'US'), labels = c('Norway', 'UK', 'US')) +
    
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
  
  plots$final$raw_nutriscore <- plot_grid(plots$raw_scores$nutriscore_disq + theme(legend.position = 'none'),
            temp,
            nrow = 1,
            rel_widths = c(1.66,1))
  
    #Save
    save_plot('./thesis/images/raw_nutriscores.png', plots$final$raw_nutriscore,
              ncol = 1.7)
    
  #Multiple traffic light
  temp <- data_recipes %>% select(-group) %>% calculateNutritionScore_trafficlights(., raw_scores = TRUE)
  temp <- temp$raw_scores %>% inner_join(., metadata) %>% #Add metadata
    #Rename and fix order of features
    mutate(feature = str_replace(feature, 'SatFa', 'Saturated\nFat')) %>%
    mutate(feature = factor(feature, levels = c('Fat', 'Saturated\nFat', 'Sugar', 'Salt')))
    
  plots$raw_scores$mtl <- ggplot(temp, aes(x = feature, y = inverted_traffic_light_rating, color = group)) +
    geom_boxplot(position = position_dodge(1)) +
    #geom_half_boxplot(side = 'r') + geom_half_violin() +
    scale_color_manual(values = various$country_colors$sample_group) +
    scale_fill_manual(values = various$country_colors$sample_group) +
    
    #Fix labs and axis
    labs(
      y = 'Points',
      color = 'Country'
    ) +
    theme(axis.title.x = element_blank(),
          legend.position = 'bottom') +
    scale_y_continuous(breaks = seq(0, 3, by = 1))
  
  save_plot('./thesis/images/raw_scores_mtl.png', plots$raw_scores$mtl, ncol = 1.7)
  
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
    mutate(feature = factor(feature, levels = c('Carbohydrates', 'Sugar', 'Dietary\nFibre', 'Fat', 'Saturated\nFat', 'Protein')))
  
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
    mutate(feature = factor(feature, levels = c('Carbohydrates', 'Sugar', 'Dietary\nFibre', 'Fat', 'Saturated\nFat', 'Protein')))
  
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
                                          plots$raw_scores$nnr + theme(legend.position = 'bottom') + labs(y=' ', title = 'Nordic Nutritional Recommendations dietary guidelines'),
                                          ncol = 1,
                                          rel_heights = c(1,1.2)) +
   #Shared label
    draw_label("Percentage of recipes that fulfill criteria", x =  0, y =0.5, vjust= 1.5, angle=90)

  save_plot('./thesis/images/raw_scores_guidelines.png', plots$final$raw_guidelines, nrow = 1.7, ncol = 1.7)
  
# How the different foodgroups contribute to recipe weight----
plots$violinbox$foodgroups <- list()

  #Animal sourced
  temp <- tidy_ingredients %>%
    filter(Foodgroup %in% c('Meat and\nmeat products', 'Dairy', 'Eggs', 'Seafood')) %>%
    #pivot wider to get Amounts column
    pivot_wider(names_from = feature,
                values_from = value) %>%
    group_by(group, sample_id, Foodgroup) %>%
    summarise(value = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
                #Turn into grams, not fraction per 100 g
                mutate(value = value*100) %>%
    #Set factor level for plot
    mutate(Foodgroup = factor(Foodgroup, levels = c('Meat and\nmeat products', 'Seafood', 'Dairy', 'Eggs')))
  
  #Plot
  plots$violinbox$foodgroups$animal_sourced <- ggplot(temp, aes(x = Foodgroup, y = value, color = group)) +
      geom_half_violin() + 
      geom_half_boxplot(side = 'r') +
      scale_color_manual(values = various$country_colors$sample_group) +
      labs(y = ' ') +
      theme(axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          legend.position = "none") +
    #Set y axis scale to percent
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  #Plant based
  temp <- tidy_ingredients %>%
    filter(Foodgroup %in% c('Vegetables and\nvegetable products', 'Roots and tubers', 'Fruit and\nfruit products', 'Fruit/vegetable juice\n and nectar', 'Grains and grain\nbased products', 'Legumes, nuts, seeds')) %>%
    #pivot wider to get Amounts
    pivot_wider(names_from = feature,
                values_from = value) %>%
    group_by(group, sample_id, Foodgroup) %>%
    summarise(value = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
    #Turn into grams, not fraction per 100 g
    mutate(value = value*100) %>%
    #Set factor level for plot
    mutate(Foodgroup = factor(Foodgroup, levels = c('Vegetables and\nvegetable products', 'Roots and tubers', 'Fruit and\nfruit products', 'Fruit/vegetable juice\n and nectar', 'Grains and grain\nbased products', 'Legumes, nuts, seeds')))
  
    #Plot
    plots$violinbox$foodgroups$plant_based <- ggplot(temp, aes(x = Foodgroup, y = value, color = group)) +
      geom_half_violin() + 
      geom_half_boxplot(side = 'r') +
      scale_color_manual(values = various$country_colors$sample_group) +
      labs(y = ' ') +
      theme(axis.title.x = element_blank(),
            #axis.title.y = element_blank(),
            legend.position = "none")+
      #Set y axis scale to percent
      scale_y_continuous(labels = function(x) paste0(x, "%"))
    
    #Other
    temp <- tidy_ingredients %>%
      filter(!Foodgroup %in% c('Meat and\nmeat products', 'Dairy', 'Eggs', 'Seafood', 'Vegetables and\nvegetable products', 'Roots and tubers', 'Fruit and\nfruit products', 'Fruit/vegetable juice\n and nectar', 'Grains and grain\nbased products', 'Legumes, nuts, seeds')) %>%
      filter(Foodgroup != 'Food imitates') %>% #Only one
      #pivot wider to get Amounts column
      pivot_wider(names_from = feature,
                  values_from = value) %>%
      group_by(group, sample_id, Foodgroup) %>%
      summarise(value = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
      #Turn into grams, not fraction per 100 g
      mutate(value = value*100) %>%
      #Set factor level for plot
      mutate(Foodgroup = factor(Foodgroup, levels = c('Water-based\nbeverages', 'Fats and oils', 'Seasoning, sauces\nand conditments', 'Sugar and\n confectionary', 'Alcoholic beverages', 'Coffee, cocoa, tea\nand infusions', 'Unknown')))
    
    #Plot
    plots$violinbox$foodgroups$others <- ggplot(temp, aes(x = Foodgroup, y = value, color = group)) +
      geom_half_violin() + 
      geom_half_boxplot(side = 'r',position = position_dodge2(preserve = "single")) +
      scale_color_manual(values = various$country_colors$sample_group) +
      labs(y = ' ',
           color = 'Country') +
      theme(axis.title.x = element_blank(),
            #axis.title.y = element_blank(),
            legend.position = "bottom")+
      #Set y axis scale to percent
      scale_y_continuous(limits = c(0,100), labels = function(x) paste0(x, "%"))
    
    #Legend
    temp <- get_legend(plots$violinbox$foodgroups$others)
    
    #All together, change y axis to %
    plots$final$foodgroups <- plot_grid(plots$violinbox$foodgroups$animal_sourced %>% changeGGplotTxtSize(txt_size = 8),
                                        plots$violinbox$foodgroups$plant_based %>% changeGGplotTxtSize(txt_size = 8),
                                        plots$violinbox$foodgroups$others + theme(legend.position = "none") %>% changeGGplotTxtSize(txt_size = 8),
                                        temp,
                                        rel_heights = c(3,3,3,1),
                                        ncol = 1
    ) + 
    #Add shared y label
    draw_label("Percentage of recipe in weight", x =  0, y =0.5, vjust= 1.5, angle=90)
    plots$final$foodgroups
    
    save_plot('./thesis/images/foodgroups.png', plots$final$foodgroups,
              nrow = 1.5, ncol = 1.3)
    
    #Just meat and meat products for article----
    temp <- tidy_ingredients %>%
      filter(Foodgroup == 'Meat and\nmeat products') %>%
      #pivot wider to get Amounts column
      pivot_wider(names_from = feature,
                  values_from = value) %>%
      group_by(group, sample_id, Foodgroup) %>%
      summarise(value = sum(Amounts, na.rm = TRUE)) %>% ungroup() %>%
      #Turn into grams, not fraction per 100 g
      mutate(value = value*100)
    
    #Plot
    plots$violinbox$foodgroups$meat_products <- ggplot(temp, aes(x = Foodgroup, y = value, color = group)) +
      geom_half_violin() + 
      geom_half_boxplot(side = 'r', outlier.shape = NA) +
      geom_quasirandom(alpha = 0.4, dodge.width = 0.75) +
      scale_color_manual(values = various$country_colors$sample_group) +
      labs(y = 'Percentage of recipe weight') +
      theme(axis.title.x = element_blank(),
            legend.position = "none") +
      #Set y axis scale to percent
      scale_y_continuous(labels = function(x) paste0(x, "%"))
    
    plots$violinbox$foodgroups$meat_products
  
# Number of recipes from each country with various protein sources----
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
    
    
  #Calculate percentage of recipes from each country
  temp <- various$recipe_protein_source %>%
    group_by(group, type) %>%
    summarise(n = n()) %>% ungroup() %>% drop_na(type) %>%
  
  #Calculate % of recipes
  #Add number of recipes from each country
  inner_join(., metadata %>% group_by(group) %>% summarise(number_of_recipes = n()) %>% ungroup) %>%
  mutate(pct = n/number_of_recipes*100) %>%
  #Add empty 'game' row for US to use in ggplot
  add_row(group = 'US', type = 'Game', pct = 0) %>%
      #Set order for plot
      mutate(type = factor(type, levels = c('Beef', 'Lamb', 'Game', 'Pork', 'Poultry', 'Lean fish', 'Oily fish', 'Shellfish', 'Vegan', 'Vegetarian')))

  #Plot
    plots$barplots <- list()
    
plots$barplots$protein_source <- ggplot(temp, aes(x = type, fill = group, y = pct)) +
    geom_bar(stat = 'Identity', position = position_dodge2(width = 0.9, preserve = "single")) +
    geom_text(aes(label = paste0(round(pct, 0), '%')), position = position_dodge2(width = 0.9, preserve = "single"), vjust = -0.2, size = 3) +
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

  #Violin/boxplot of the environmental impact of the recipes based on protein source
  temp <- various$recipe_protein_source %>%
    inner_join(tidy_recipes %>% filter(feature %in% c('CO2', 'Landuse'))) %>%
    #Add a fake US recipe with game to get the right width of the quasirandom points
    add_row(group = 'US', sample_id = 'fake', type = 'Game', Source = 'fake', feature = 'CO2', value = 3) %>%
    add_row(group = 'US', sample_id = 'fake', type = 'Game', Source = 'fake', feature = 'Landuse', value = 5) %>%
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
  
  plots$violinbox$recipe_protein_sources <- ggplot() +
    #Boxplot
    geom_boxplot(data = temp %>% filter(!sample_id == 'fake'), aes(x = type, y = value, color = group),
                 position = position_dodge(preserve = "single"), outlier.shape = NA) +
    scale_color_manual(values = various$country_colors$sample_group) +
    #Points
    geom_quasirandom(data = temp, aes(x = type, y = value, color = group, alpha = alpha_level),
                     dodge.width = 0.75, size = 1, varwidth = FALSE) +
    scale_alpha_discrete(range = c(0, 0.5)) + #Set fake data "invisible" to have zero alpha
    facet_wrap(~feature, scales = 'free', nrow = 2) +
    labs(y = ' ',
         x = 'Protein source',
         color = 'Country') +
    theme(legend.position = 'bottom') +
    #Remove alpha legend
    guides(alpha = "none")
  
  save_plot('./thesis/images/protein_source_violinbox.png', plots$violinbox$recipe_protein_sources, nrow = 2.2, ncol = 1.7)

# Article plot----
  plots$article <- list()
  
  #Add label to embedded plot with amount of meat per 100 g
  temp <- plot_grid(plots$violinbox$foodgroups$meat_products + labs(y = 'Recipe weight') + scale_x_discrete(position = "top"),
                    labels = 'B')
  plots$article$protein_sources <- plot_grid(plots$barplots$protein_source + theme(legend.position = c(0.075, 0.8)) +
                                               annotation_custom(ggplotGrob(temp),
                                                                 xmin = 7, xmax = 10, ymin = 22, ymax = 50),
                                             plots$violinbox$recipe_protein_sources + theme(legend.position = 'none'),
                                             nrow = 2,
                                             labels = c('A', 'C'),
                                             rel_heights = c(1.5,2)
  )
  
  plots$article$protein_sources
  
  save_plot('./thesis/images/article_proteinsources.png', plots$article$protein_sources, ncol = 2, nrow = 3)
  
## Adjancency matrix nutrient sources----
  
  #Some dataframes that are needed
  various$adjacency <- list()
  
  #Number of recipes that are a sources of a nutrient depending on type of recipe
  various$adjacency$n_source_of_protein_and_nutrients <- various$with_RDI %>%
    #Add protein
    bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
    #fill missing country and source information
    group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
    #Find sources and good sources
    mutate(nutrient_source = case_when(
      #Protein
      feature == 'Protein' & value > 12 ~ 'source',
      #Fibre
      feature == 'Dietary fibre' & value > 3 ~ 'source',
      #Micronutrients
      feature != 'Protein' & value >= 15 ~ 'source',
      
      TRUE ~ 'not_source'
    ))
  
  #Number of recipes that are good sources of a nutrient depending on type of recipe
  various$adjacency$n_good_source_of_protein_and_nutrients <- various$with_RDI %>%
    #Add protein
    bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
    #fill missing country and source information
    group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
    #Find sources and good sources
    mutate(nutrient_source = case_when(
      #Protein
      feature == 'Protein' & value > 20 ~ 'good_source',
      #Fibre
      feature == 'Dietary fibre' & value > 6 ~ 'good_source',
      #Micronutrients
      feature != 'Protein' & value >= 30 ~ 'good_source',

      TRUE ~ 'not_source'
    ))
  
  #Apply the same formatting to both
  various$adjacency <- lapply(various$adjacency, function(x) {
    
    x %>% #Rename features to better fit with the plots to create
      mutate(feature = feature %>%
               str_replace_all('Vitamin', 'Vit.') %>%
               str_replace_all('Calcium', 'Ca') %>%
               str_replace_all('Copper', 'Cu') %>%
               str_replace_all('Folate', 'Vit. B9') %>%
               str_replace_all('Iodine', 'I') %>%
               str_replace_all('Iron', 'Fe') %>%
               str_replace_all('Magnesium', 'Mg') %>%
               str_replace_all('Niacin', 'Vit. B3') %>%
               str_replace_all('Phosphorus', 'P') %>%
               str_replace_all('Potassium', 'K') %>%
               str_replace_all('Riboflavin', 'Vit. B2') %>%
               str_replace_all('Selenium', 'Se') %>%
               str_replace_all('Sodium', 'Na') %>%
               str_replace_all('Thiamin', 'Vit. B1') %>%
               str_replace_all('Zinc', 'Zn') %>%
               str_replace_all('Dietary fibre', 'Fibre')
      ) %>%
      #Add protein source
      inner_join(., various$recipe_protein_source) %>%
      #Count number of recipes that are (good) sources
      group_by(feature, type, nutrient_source) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(nutrient_source != 'not_source') %>% #Only sources are used in plot
      #Scale by the number of recipes in the category
      #Add total number of recipes
      inner_join(., various$recipe_protein_source %>%
                   group_by(type) %>%
                   summarise(total_n_recipes = n()) %>%
                   ungroup()) %>%
      #Scale
      mutate(scaled = round(n/total_n_recipes*100, 0)) %>%
      #rename feature to fit with graph object
      rename(name = feature) %>%
      #Split by protein source
      split(., .$type) %>%
      #Change 'type' into the categories "red meat", "poultry" etc
      lapply(., function(x) {x %>%
          #Save old typing
          mutate(org_type = type) %>%
          #change
          mutate(type = type %>%
                   str_replace('Beef|Lamb|Game|Pork', 'Red meat') %>%
                   str_replace('Lean fish|Oily fish|Shellfish', 'Seafood') %>%
                   str_replace('Vegetarian|Vegan', 'Vegetarian/Vegan')
          )})
    
  })
  
  #Function to turn df into adjacency list
  #df a wide dataframe with the recipes in rows and the nutrients in columns, 1 for each nutrient the recipe is a source of, 0 if it is not a source
  makeAdjacencyMatrix <- function(df){
    
    temp <- df %>% select(-type)
    
    #Create a matrix
    mat <- as.matrix(temp[-c(1,2)])
    
    #Adjancencymatrix
    adjacency <- t(mat) %*% mat
    
    adjacency
    
  }
  
  #Turn an adjacencymatrix into an adjacency list with no duplicates
  makeAdjacencyList <- function(adjacency_matrix){
    
    #List
    list <- reshape::melt(adjacency_matrix) %>%
      mutate(delete = case_when(
        X1 == X2 ~ 'delete',
        TRUE ~ 'keep'
      )) %>%
      filter(delete == 'keep') %>%
      mutate(X1 = as.character(X1),
             X2 = as.character(X2)) %>%
      #Remove small connections
      mutate(value = case_when(
        value <= 1 ~ 0,
        TRUE ~ value
      )) %>% select(-delete) %>%
      #Remove duplicate connections
      group_by(X1, X2) %>%
      mutate(edge_id = paste(sort(unique(c(X1,X2))), collapse="_")) %>%
      ungroup() %>%
      select(edge_id, value) %>% unique() %>%
      separate(., col = edge_id, into = c('X1', 'X2'), sep = '_') %>%
      select(X1, X2, value) %>%
      filter(value != 0)
    
    list
    
  }
  
  #Create an adjacency matrix of nutrients recipes are sources of
  various$adjacency$matrix$source <- various$with_RDI %>%
    #Add protein
    bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
    #fill missing country and source information
    group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
    #Find recipes that are sources of the nutrients
    mutate(nutrient_source = case_when(
      #Protein
      feature == 'Protein' & value > 12 ~ 1,
      #Fibre
      feature == 'Dietary fibre' & value > 3 ~ 1,
      #Micronutrients
      feature != 'Protein' & value >= 15 ~ 1
    )) 
  
  #Create an adjacency matrix of nutrients recipes are good sources of
  various$adjacency$matrix$good_source <- various$with_RDI %>%
    #Add protein
    bind_rows(., various$with_energy_pct_densityMJ %>% filter(feature %in% c('Protein', 'Dietary fibre'))) %>%
    #fill missing country and source information
    group_by(sample_id) %>% fill(., c(group, Source)) %>% ungroup() %>%
    #Find recipes that are sources of the nutrients
    mutate(nutrient_source = case_when(
      #Protein
      feature == 'Protein' & value > 20 ~ 1,
      #Fibre
      feature == 'Dietary fibre' & value > 6 ~ 1,
      #Micronutrients
      feature != 'Protein' & value >= 30 ~ 1
    )) 
  
  #Do same formatting to both
  various$adjacency$matrix <- lapply(various$adjacency$matrix, function(x) {
    
    x %>%
      #Rename features to better fit in these plots
      mutate(feature = feature %>%
               str_replace_all('Vitamin', 'Vit.') %>%
               str_replace_all('Calcium', 'Ca') %>%
               str_replace_all('Copper', 'Cu') %>%
               str_replace_all('Folate', 'Vit. B9') %>%
               str_replace_all('Iodine', 'I') %>%
               str_replace_all('Iron', 'Fe') %>%
               str_replace_all('Magnesium', 'Mg') %>%
               str_replace_all('Niacin', 'Vit. B3') %>%
               str_replace_all('Phosphorus', 'P') %>%
               str_replace_all('Potassium', 'K') %>%
               str_replace_all('Riboflavin', 'Vit. B2') %>%
               str_replace_all('Selenium', 'Se') %>%
               str_replace_all('Sodium', 'Na') %>%
               str_replace_all('Thiamin', 'Vit. B1') %>%
               str_replace_all('Zinc', 'Zn') %>%
               str_replace_all('Dietary fibre', 'Fibre')
      ) %>%
      #Filter non-source nutrients
      filter(!is.na(nutrient_source)) %>%
      #Remove columns and turn wider
      select(-c(Source, value)) %>%
      pivot_wider(
        names_from = feature,
        values_from = nutrient_source,
        values_fill = 0
      ) %>%
      #Remove beta-carotene and retinol
      select(-c(`Beta-carotene`, Retinol)) %>%
      #Add protein source and split into multiple dataframes based on protein source
      inner_join(., various$recipe_protein_source) %>%
      split(., .$type) %>%
      #Turn into adjacency matrix
      lapply(., makeAdjacencyMatrix)
    
  })
  
  
  # Chord diagrams----
  #Only use good sources as plot gets too full if not
  #Calculate "big gap" for each matrix, to have number of recipes scaled to the Oily Fish dataset for chord diagram
  gaps <- lapply(various$adjacency$matrix$good_source[c(1:4, 6:10)], calc_gap, x1 = various$adjacency$matrix$good_source$`Oily fish`)
  
  #Turn adjancency matrix into adjacency list
  temp <- lapply(various$adjacency$matrix$good_source, makeAdjacencyList)
    
  #Colorbrewer set3 for coloring the nutrients
  grid_colors <- c(Protein = '#8dd3c7', Fibre = '#ffffb3', P = '#bebada', Na = '#ffed6f', Cu = '#80b1d3',
                `Vit. A` = '#ffffff', `Vit. B6` = '#b3de69', `Vit. B12` = '#fccde5', Zn = '#d9d9d9',
                `Vit. C` ='#bc80bd', Se = '#1FDEA0', `Vit. E` = '#ccebc5', Fe = '#ffed6f', `Vit. B2` = '#d8fb98',
                I = '#fdb462', `Vit. B1` = '#3ddd0c', `Vit. B9` = '#cfa649', `Vit. B3` = '#fb8072')
  
  #Plot titles
  titles <- various$recipe_protein_source %>%
    group_by(type) %>%
    summarise(n = n()) %>% ungroup() %>%
    mutate(title = paste0(type, ' (n = ', n, ')')) %>% select(title)
  
  #Save plots as png
  for(i in 1:length(temp)){
    
    #Set colors
    #nutrients <- as_vector(c(temp[[1]][1], temp[[1]][2])) %>% unique()
    
    pdf(paste0('./thesis/images/chordDiagram_', names(temp[i]), '.pdf'))
    
    circos.clear()
    chordDiagram(temp[i],
               #Use black point lines to separate chords from each other
               link.lwd = 1, link.lty = 1, link.border = "black",
               #Set color of chords to gray
               col = "gray",
               grid.col = grid_colors,
               #Set annotations at 90 degree angle
               annotationTrack = c("grid"),
               preAllocateTracks = list(track.height = max(strwidth(temp$Beef$X1)))
               )
  
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), ylim[1], si, sector.index = si, track.index = 1,
                facing = "clockwise", niceFacing = TRUE, adj = c(-0.2, 0.5), cex = 0.8)
    circos.axis(h = 0,
                #major.at = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5) ,
                labels.cex = 0.6,labels.facing = "inside", 
                sector.index = si, track.index = 2)
  }
  
  #Create title
  title(titles[[i, 1]])
  
  dev.off()
    
  }
  
  
  # Network plot----
  library(tidygraph)
  library(ggraph)
  
  #Create a tidygraph object with type of protein information and how many recipes are sources
  #Source
  various$adjacency$tidygraph$source <- lapply(various$adjacency$matrix$source,
                                               #First make adjacency list
                                               makeAdjacencyList) %>% 
    #Scale edges by number of recipes in that category and turn into tidygraph object
    mapply(function(x, y) {x %>%
        mutate(total_n_recipes = unique(y$total_n_recipes)) %>%
        mutate(scaled = round(value/total_n_recipes*100, 0))}, x = ., y = various$adjacency$n_source_of_protein_and_nutrients,
      USE.NAMES = TRUE, SIMPLIFY = FALSE) %>%
    lapply(., as_tbl_graph, directed = FALSE) %>%
    #Also scale nodes by total number of recipes
    mapply(left_join, x = ., y = various$adjacency$n_source_of_protein_and_nutrients, USE.NAMES = TRUE, SIMPLIFY = FALSE)
  #Good source
  various$adjacency$tidygraph$good_source <- lapply(various$adjacency$matrix$good_source,
                                                    #First make adjacency list
                                                    makeAdjacencyList) %>% 
    #Scale edges by number of recipes in that category and turn into tidygraph object
    mapply(function(x, y) {x %>%
        mutate(total_n_recipes = unique(y$total_n_recipes)) %>%
        mutate(scaled = round(value/total_n_recipes*100, 0))}, x = ., y = various$adjacency$n_good_source_of_protein_and_nutrients,
        USE.NAMES = TRUE, SIMPLIFY = FALSE) %>%
    lapply(., as_tbl_graph, directed = FALSE) %>%
    #Also scale nodes by total number of recipes
    mapply(left_join, x = ., y = various$adjacency$n_good_source_of_protein_and_nutrients, USE.NAMES = TRUE, SIMPLIFY = FALSE)
  
  
  various$protein_source_colors <- setNames(c('#DF4B67',
                                            '#EAE51A',
                                            '#365D8D',
                                            '#7CD250'),
                                            c('Red meat',
                                              'Poultry',
                                              'Seafood',
                                              'Vegetarian/Vegan')
                                            )
  
  #Graph = tidygraph object
  #Source = good_source, or just source, if 'good_source' some graphs will be made into trees as they have few nodes and connection
  #label_size is the text size of the labels
  plotNetwork <- function(graph, source = NULL, label_size = 3.58) {
    
    #Vegan must have "tree" mode as there are so few recipes
    if(source == 'good_source' & data.frame(graph)$org_type %in% c('Vegan', 'Game')) {
      
      temp <- ggraph(graph, 'tree')
      
    } else {
      
      temp <- ggraph(graph)
      
    }
    
    temp +
      #Draw edges and nodes
      geom_edge_link(aes(alpha = scaled)) + #Alpha based on the number of connections
      geom_node_point(aes(size = scaled, fill = type), shape = 21, color = 'black') +  #Size based on the number of recipes that are a source
      geom_node_label(aes(label = name), size = label_size, repel = TRUE, show.legend = FALSE) + #Add labels for the nutrients
      theme(legend.position = "none") +
      
      #Scale alpha and size between plots
      scale_size_area(limit = c(0,100), breaks = c(5, 25, 50, 75, 100), labels = c('5 %', '25 %', '50 %', '75 %', '100 %')) +
      scale_edge_alpha(range = c(0.01, 1), guide = 'none') +
      #Add colors and change the size of the legend item for fill/color
      scale_fill_manual(values = various$protein_source_colors) +
      #Add title
      ggtitle(label = data.frame(graph)$org_type)
    
  } 
  
  #Plot
  plots$network$source <- sapply(various$adjacency$tidygraph$source, plotNetwork, source = 'source', USE.NAMES = TRUE, simplify = FALSE, label_size = 3)
  plots$network$source$Vegetarian
  plots$network$good_source <- sapply(various$adjacency$tidygraph$good_source, plotNetwork, source = 'good_source', USE.NAMES = TRUE, simplify = FALSE)
  plots$network$good_source$Vegan
    
  #Create a legend for the plots
  plot_legends$network <- plot_grid(get_legend(plotNetwork(various$adjacency$tidygraph$source$Shellfish, source = 'source') +
    theme(legend.position = 'right') +
    labs(fill = 'Protein source') +
    guides(fill = guide_legend(override.aes = list(size = 5)),
           alpha = 'none',
           size = 'none')),
    get_legend(plotNetwork(various$adjacency$tidygraph$source$Shellfish, source = 'source') +
                 theme(legend.position = 'right') +
                 labs(size = 'Percentage of recipes') +
                 guides(fill = 'none',
                        alpha = 'none')),
    ncol = 2, align = 'hv')
  
  #Build a large plot
  plots$final$network$sources <- plot_grid(
    plots$network$source$Beef, plots$network$source$Lamb, plots$network$source$Game, plots$network$source$Pork,
    plots$network$source$Poultry, plots$network$source$`Lean fish`, plots$network$source$`Oily fish`, plots$network$source$Shellfish,
    plots$network$source$Vegetarian, plots$network$source$Vegan, plot_legends$network  + theme(plot.margin = margin(0, 0, 0, 25)), #Add margin between last plot and legend
    ncol = 4)
  
  plots$final$network$good_sources <- plot_grid(
    plots$network$good_source$Beef, plots$network$good_source$Lamb, plots$network$good_source$Game, plots$network$good_source$Pork,
    plots$network$good_source$Poultry, plots$network$good_source$`Lean fish`, plots$network$good_source$`Oily fish`, plots$network$good_source$Shellfish,
    plots$network$good_source$Vegetarian, plots$network$good_source$Vegan, plot_legends$network + theme(plot.margin = margin(0, 0, 0, 25)),
    ncol = 4)
  
  #Save
  save_plot('./thesis/images/network_source.png', plots$final$network$sources, ncol = 2, nrow = 2)
  save_plot('./thesis/images/network_good_source.png', plots$final$network$good_sources, ncol = 2, nrow = 2)
  

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
  
  #Get the p value and effect size for each feature
  'kruskal_wallis' = stats$kruskal_wallis %>% filter(feature %in% temp$feature) %>%
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
  'dunn' = stats$dunn_test %>% filter(p.adj <0.05) %>%
    mutate(Pairwise = paste0(group1, ' - ', group2)) %>%
    select(feature, Pairwise, p.adj) %>%
    #Use <0.05, <0.01 and <0.001 from p value
    mutate(p.adj = case_when(
      p.adj < 0.001 ~ '<0.001',
      p.adj < 0.01 ~ '<0.01',
      p.adj < 0.05 ~ '<0.05'
    )) %>%
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

## Other data ----
#Guidelines, nutriscore and trafficlights
#Guidelines and traffic lights
guidelines_trafficlights <- read_csv2('./Data/health_indicators/guidelines_trafficlights.csv') %>%
  rename(Feature = X1) %>%
  replace(is.na(.), ' ')

#Nutriscore
nutriscore_points <- read_csv2('./Data/health_indicators/nutriscore_points.csv') %>%
  select(-Points_1) %>%
  replace(is.na(.), ' ')


## Save objects to be used in RMarkdown----
save(stat_table, tidy_ingredients, guidelines_trafficlights, source_of_nutrients_table, pct_recipes_healthiness_scores,
     nutriscore_points, descriptive_stats_total, file = './Data/results_allrecipes.RData')
