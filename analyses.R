devtools::load_all(path = '.')


## Setup ----
#Empty list to fill with plots and plot legends
plots <- list()
plot_legends <- list()
plots$final <- list() #For plots that needs extra tweaking   

#GGplot theme
theme_set(theme_bw())

#Read data
data_recipes <- readRDS('./Data/output/recipes_for_analysis.Rds') %>% #Nutrient content/sustainability indicators pr 100g recipe
  #Remove non-dinner recipes
  filter(!sample_id %in% c('Baklava (Turkey)', 'Sausage Lunch', 'Half-fermented trout', 'Straight trout'))

data_ingredients <- readRDS('./Data/output/ingredients_for_analysis.Rds') %>% #Nutrient content/sustainability indicators for every ingredient pr 100g recipe
  rename(Foodgroup = L1) %>%
  #Remove non-dinner recipes
  filter(!sample_id %in% c('Baklava (Turkey)', 'Sausage Lunch', 'Half-fermented trout', 'Straight trout'))

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
           str_replace('Water and water-based beverages', 'Water-based\nbeverages')
  ) %>% replace_na(list(Foodgroup = 'Unknown'))

#Recipe metadata
metadata <- readRDS('./Data/output/recipe_metadata.Rds')
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
      ) + ylim(0,100),
    'environment' = plotViolinBox(data_completeness$no_env_info) +
      scale_color_manual(values = various$country_colors$sample_group) +
      labs(
        color = 'Country',
        x = 'Country',
        y = '% of recipe weight with env. impact indicators'
      ) + ylim(0,100)
    
  )
  #Capture legens
  plot_legends$data_completeness <- get_legend(
    plots$data_completeness$nutrients +  
      guides(color = guide_legend(nrow = 3)) +
      theme(legend.position = "right")
  )
  #Final plot
  plots$final$data_completeness <- plot_grid(plots$data_completeness$nutrients + theme(legend.position  = 'none'),
                                             plots$data_completeness$environment + theme(legend.position = 'none'),
                                             plot_legends$data_completeness,
                                             
                                             #Labels, number of rows and relative width of plots
                                             labels = c('A', 'B', ''),
                                             nrow = 1,
                                             rel_widths = c(1,1,0.3))
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
  
  'Keyhole' = tidy_ingredients %>% calculateNutritionScore_keyhole() %>%
    inner_join(metadata %>% select(sample_id, group)) %>%
    #Turn keyhole certified to 1 for certified and 0 for not certified
    mutate(keyhole_certified = case_when(
      keyhole_certified == 'No Keyhole' ~ 0,
      keyhole_certified == 'Keyhole' ~ 1)),
  
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
  inner_join(., metadata)

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
descriptive_stats <- run_stats %>%
  group_by(group, feature) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            q3 = quantile(value, 0.75),
            max = max(value))

#Descriptive stats of missing data
temp <- data_completeness$no_nutrient_info %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

temp <- data_completeness$no_sustainability_indicators %>%
  group_by(group) %>%
  summarise(min = min(value),
            q1 = quantile(value, 0.25),
            median = median(value),
            mean = mean(value),
            q3 = quantile(value, 0.75),
            max = max(value)) %>% ungroup()

# Kruskal Wallis and dunn----
stats <- list(
  
  'kruskal_wallis' = run_stats %>%
    group_by(feature) %>%
    kruskal_test(., value ~ group) %>%
    adjust_pvalue(., method = 'BH'),
  
  'kruskal_wallis_effectsize' = run_stats %>%
    group_by(feature) %>%
    kruskal_effsize(value ~ group),
  
  'dunn_test' = run_stats %>%
    group_by(feature) %>%
    dunn_test(value ~ group, detailed = TRUE) %>%
    adjust_pvalue(., method = 'BH')
  
)

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
           str_replace('keyhole_certified', 'Keyhole')
  ) %>%
  inner_join(., metadata) %>%
  pivot_wider(., names_from = 'feature', values_from = 'value')

#Health indicators with sustainability indicators
plots$correlations$healthVSsustainability <- ggpairs(temp %>% select(-sample_id),
        mapping = ggplot2::aes(color=group, alpha = 0.6),
        columns = 32:38,
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
        columns = c(25:31, 37,38),
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
        columns = c(4,5, 7:9, 37,38),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

plots$correlations$mineralsVSsustainability2 <- ggpairs(temp %>% select(-sample_id),
                                                        mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                        columns = c(11, 12, 15, 16, 24, 37,38),
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
        columns = c(18,3,13,22,23, 21, 37,38),
        upper = list(continuous = myCorrelations_textsize),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)

plots$correlations$vitaminsVSsustainability2 <- ggpairs(temp %>% select(-sample_id),
                                                        mapping = ggplot2::aes(color=group, alpha = 0.6),
                                                        columns = c(17,14,10,20,6,19, 37,38),
                                                        upper = list(continuous = myCorrelations_textsize),
                                                        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1, se = FALSE))) +
  scale_color_manual(values = various$country_colors$sample_group) +
  scale_fill_manual(values = various$country_colors$sample_group)


  #Save
  save_plot('./thesis/images/vitaminsVSsustainability1.png', plots$correlations$vitaminsVSsustainability1,
            ncol = 2.2, nrow = 2.2)
  save_plot('./thesis/images/vitaminsVSsustainability2.png', plots$correlations$vitaminsVSsustainability2,
            ncol = 2.2, nrow = 2.2)

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
  theme(legend.position = 'bottom') +
  #Fix lab
  labs(color = 'Country')

#Loadings plot
plots$pca$loadings <- createPCA(temp, plots = 'loadings', cutoff = 0.043) +
  theme(legend.position = 'bottom')

plots$final$pca <- plot_grid(plots$pca$scores,
            plots$pca$loadings,
            #Labels, relative width/height of plots and number of grid columns
            labels = "AUTO",
            ncol = 1,
            rel_heights = c(1,1.1))

  #Save
  save_plot('./thesis/images/pca_plots.png', plots$final$pca,
            nrow = 2)

## Violin boxplots----
#Sustainability
plotViolinBox(run_stats %>%
                filter(feature %in% various$sustainability) %>%
                mutate(feature = feature %>%
                         str_replace('CO2', 'Kilo CO2 equivalents\nper 100g') %>%
                         str_replace('Landuse', 'm2 per year\nper 100g'))) +
  facet_wrap(~feature, scale = 'free', nrow = 2) +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Value'
  )

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

#Health scores
plotViolinBox(health_indicators %>%
                filter(feature %in% various$health) %>%
                mutate(feature = feature %>%
                         str_replace('inverted_nutriscore', 'Inverted Nutriscore') %>%
                         str_replace('nnr_score', 'Nordic Nutritional\nRecommendations') %>%
                         str_replace('who_score', 'World Health Organization\nRecommendations') %>%
                         str_replace('inverted_traffic_score', 'Inverted Multiple\nTraffic Light Model'))) + facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country'
  )
#Minerals
plotViolinBox(run_stats %>%
                filter(feature %in% various$minerals)) + facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Percentage of RDI'
  ) + theme(legend.position="bottom")
#Vitamins
plotViolinBox(run_stats %>%
                filter(feature %in% various$vitamins)) +
  facet_wrap(~feature, scales = 'free') +
  labs(
    color = 'Country',
    x = 'Country',
    y = 'Percentage of RDI'
  ) + theme(legend.position="bottom")
#Energy
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

#Percentage of different foodgroups in the different countries recipes
temp <- tidy_ingredients %>% group_by(group, sample_id, Foodgroup) %>%
  summarise(value = sum(Amounts, na.rm = TRUE))

plotViolinBox(temp) + facet_wrap(~Foodgroup) +
  labs(
    y = 'Percentage of recipe weight',
    x = 'Country'
  ) + scale_color_manual(values = various$country_colors$sample_group)


#Tables----
#Stats----
#Significantly differences from dunn test
temp <- stats$dunn_test %>% filter(p.adj <0.05)

#Format the relevant data
stat_table <- list(
  
  'descriptive_stats' = descriptive_stats %>% filter(feature %in% temp$feature) %>%
    #Create one row for each feature, with the median + interquartile range for each country
    mutate(median_iqr = paste0(round(median, 1), ' (', round(q1, 1), ', ', round(q3, 1), ')')) %>%
    select(feature, group, median_iqr) %>%
    pivot_wider(.,
                names_from = group,
                values_from = median_iqr),
  
  #Get the chi square statistic and p value for each feature
  'kruskal_wallis' = stats$kruskal_wallis %>% filter(feature %in% temp$feature) %>%
    select(feature, statistic, p) %>%
    #Turn p value to scientific notation in text and round chisquare stat
    mutate(p = formatC(p, format = "e", digits = 2),
           statistic = round(statistic, 1)) %>%
    rename(`Chi square` = statistic,
           `p-value` = p),
  
  #Get the pairwise comparison and adjusted p value for each feature
  'dunn' = stats$dunn_test %>% filter(p.adj <0.05) %>%
    mutate(Pairwise = paste0(group1, ' - ', group2)) %>%
    select(feature, Pairwise, p.adj) %>%
    #Turn p value to scientific notation in text
    mutate(p.adj = formatC(p.adj, format = "e", digits = 2)) %>%
    rename(`Adj. p-value` = p.adj) %>%
    #One row for each feature
    group_by(feature) %>%
    summarise(Pairwise = paste0(Pairwise, collapse = '<br><br>'),
              `Adj. p-value` = paste0(`Adj. p-value`, collapse = '<br><br>')) %>% ungroup()
  
) %>%
  
  #Finished table
  reduce(full_join, by = 'feature') %>%
  #Clean up some names
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
           str_replace('Kilocalories', 'Kilocalories/100g'))

#KableExtra formatting
formatKableStat <- function(df, features = NULL, caption = NULL){
  
  #Filter out features of interest or show all?
  if(!is.null(features)){
    dt <- df %>% filter(Feature %in% features)
  } else {
    dt <- df
  }
  
  #Add a caption or not
  if(!is.null(caption)){
    kbl(dt, 'html', escape = FALSE) %>%
      kable_classic_2() %>%
      kable_styling(full_width = FALSE) %>%
      add_header_above(c(" " = 1, "Median (IQR)" = 3, "Kruskal-Wallis test" = 2, "Dunn test, BH corrected" = 2))
  } else {
    kbl(dt, 'html', caption = caption, escape = FALSE) %>%
      kable_classic_2() %>%
      kable_styling(full_width = FALSE) %>%
      add_header_above(c(" " = 1, "Median (IQR)" = 3, "Kruskal-Wallis test" = 2, "Dunn test, BH corrected" = 2))
  }
  
}

#All
formatKableStat(stat_table)
#Sustainability indicators
formatKableStat(stat_table, c('CO2', 'Landuse'))
#Health indicators
formatKableStat(stat_table, c('Inv. Nutriscore', 'Inv. Traffic Light', 'WHO Score', 'NNR Score'))
#Energy
formatKableStat(stat_table, c('Carbohydrates E%', 'Sugar E%', 'Fat E%', 'Saturated Fat E%', 'Dietary fibre g/MJ', 'Protein E%'))
#Minerals
formatKableStat(stat_table, various$minerals)
#Vitamins
formatKableStat(stat_table, various$vitamins)

#Others----
standardKbl <- function(df, caption = NULL){
  #df = dataframe to turn into kableExtra table, caption = caption for the table
  
  if(is.null(caption)){
    
    table <- df %>%
      kbl(escape = FALSE) %>%
      kable_classic_2(full_width = FALSE)
    
  } else {
    table <- df %>%
      kbl(caption = caption, escape = FALSE) %>%
      kable_classic_2(full_width = FALSE)
  }
  
  table 
  
}

#Foodgroups in SHARP
temp <- tidy_ingredients %>%
  select(Foodgroup) %>% unique() %>% arrange(., Foodgroup)

standardKbl(temp)

#Guidelines, nutriscore and trafficlights
guidelines_trafficlights <- read_csv2('./Data/health_indicators/guidelines_trafficlights.csv') %>%
  rename(Feature = X1) %>%
  replace(is.na(.), ' ')

#Guidelines
kbl(guidelines_trafficlights %>% select(Feature, `World Health Organization`, `Nordic Nutritional Recommendation`), escape = FALSE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Dietary guidelines' = 2))
#Multiple traffic lights
kbl(guidelines_trafficlights %>%
      select(Feature, `Green/low`, `Amber/medium`, `Red/high`) %>%
      filter(!Feature %in% c('Carbohydrate', 'Dietary fibre', 'Protein')), escape = FALSE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Multiple traffic light model' = 3))
#Nutriscore
nutriscore_points <- read_csv2('./Data/health_indicators/nutriscore_points.csv') %>%
  select(-Points_1) %>%
  replace(is.na(.), ' ')

kbl(nutriscore_points, escape = TRUE) %>%
  kable_classic_2(full_width = FALSE) %>%
  add_header_above(c(' ' = 1, 'Disqualifying' = 4, 'Qualifying' = 3))

#RDIs
standardKbl(various$RDI %>%
              #Rename columns
              rename(Feature = feature,
                     RDI = rdi) %>%
              #Add units
              mutate(Unit = case_when(
                Feature %in% c('Calcium', 'Iron', 'Zinc', 'Magnesium',
                               'Phosphorus', 'Copper', 'Potassium', 'Thiamin',
                               'Riboflavin', 'Vitamin B6', 'Vitamin C', 'Sodium') ~ 'mg',
                Feature %in% c('Iodine', 'Selenium', 'Vitamin D', 'Folate',
                               'Vitamin B12') ~ 'mcg',
                Feature == 'Vitamin A' ~ 'RAE',
                Feature == 'Vitamin E' ~ 'mcg TE',
                Feature == 'Niacin' ~ 'NE'
              )))

group_colors <- various$country_colors$sample_group
RDI <- various$RDI
minerals <- various$minerals
energy_contributing <- various$energy_contributing
vitamins <- various$vitamins
sustainability <- various$sustainability
health <- various$health

#Save objects to be used in RMarkdown
save(data_ingredients, data_ingredients, tidy_recipes,
     tidy_ingredients, run_stats, stat_table, health_indicators,
     RDI, group_colors, minerals, vitamins, sustainability,
     health, energy_contributing, guidelines_trafficlights,
     nutriscore_points, data_completeness, file = './Data/results_allrecipes.RData')



