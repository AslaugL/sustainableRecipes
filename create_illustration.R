library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)

#GGplot theme
theme_set(theme_classic())

## Inport raw data----
raw <- list(
  
  GHGE = read_xlsx('./Data/for_figures/1-s2.0-S0959652616303584-mmc1.xlsx'),
  water_footprint_crops = read_xlsx('./Data/for_figures/Report47-Appendix-II.xlsx',
                                    sheet = 2),
  water_footprint_animals = read_xlsx('./Data/for_figures/Report48-Appendix-V.xlsx',
                                      sheet = 2) %>%
    #Remove duplicate columns
    select(-c(...814, ...815, ...816, ...817)),
  land_use = read_xls('./Data/for_figures/aaq0216_datas2.xls')
  
)

## GHGE----
GHGE <- list(
  'animals' = raw$GHGE %>%
  filter(`Food type` %in% c('Beef', 'Lamb', 'Pork', 'Chicken')) %>%
  select(`Food type`, `kg CO2-eq/kg produce, BFM or L after conversion`) %>%
  rename(Food = `Food type`,
         value = `kg CO2-eq/kg produce, BFM or L after conversion`),

'seafood' = raw$GHGE %>%
  filter(`Sub-category` %in% c('Fish Counter', 'Shelfish')) %>%
  mutate(Food = case_when(
    `Sub-category` == 'Fish Counter' ~ 'Fish',
    TRUE ~ 'Shellfish'
  )) %>%
  rename(value = `kg CO2-eq/kg produce, BFM or L after conversion`) %>%
  select(Food, value),

'plants' = raw$GHGE %>%
  filter(`Food counter` %in% c('Fruit and Vegetable Counter', 'Flours, Grains, Pulses  and Nuts')) %>%
  mutate(Food = case_when(
    `Sub-category` == 'Cereal' ~ 'Cereals',
    `Sub-category` == 'Legume' ~ 'Legumes',
    `Sub-category` == 'Tree Nuts' ~ 'Nuts',
    `Food counter` == 'Fruit and Vegetable Counter' ~ 'Fruit and\nvegetables'
  )) %>%
  rename(value = `kg CO2-eq/kg produce, BFM or L after conversion`) %>%
  select(Food, value)
) %>%
  
  #All in one df
  bind_rows(.)  %>%
  
  #Drop NA values in CO2 (excluded studies),
  #In Food column NA values are food items not relevant
  drop_na('value') %>%
  drop_na('Food') %>%
  
  #Calculate mean, median, 5th and 95th percentile
  group_by(Food) %>%
  summarise(mean = mean(value),
            median = median(value),
            `5th pctl` = quantile(value, 0.05),
            `95th pctl` = quantile(value, 0.95),
            max = max(value),
            min = min(value)) %>% ungroup() %>%
  
  #Set order of the Food column
  arrange(match(Food, c('Beef', 'Lamb', 'Pork', 'Chicken', 'Fish', 'Shellfish', 'Legumes', 'Nuts', 'Cereals', 'Fruit\nvegetables'))) %>%
  
  #Turn long for ggplot
  pivot_longer(.,
               cols = -Food,
               names_to = 'statistic',
               values_to = 'value') 


## Water footprint----
#Column names crops
temp <- c('Product code (FAOSTAT)', 'Product code (HS)', 'Product code      (SITC)',
          'Product description (HS)', 'Product description (FAOSTAT)', 'Root product (HS)',
          'Product fraction (pf)', 'Value fraction (vf)', 'WF_type', as.character(raw$water_footprint_crops[2, 10:3263]))
names(raw$water_footprint_crops) <- temp

#Clean up dataframe
water_footprint <- list(
  'crops' = raw$water_footprint_crops %>%
    #Remove unneccessary columns
    select(-c(`Product code (FAOSTAT)`, `Product code (HS)`,
              `Product code      (SITC)`, `Product description (HS)`,
              `Root product (HS)`, `Product fraction (pf)`,
              `Value fraction (vf)`, ends_with('999'))) %>%
    #Remove rows without food information
    slice(6:n()) %>%
    #Rename column
    rename(Product = `Product description (FAOSTAT)`))
  
#Only use foods with FAOSTAT identifier
#Find the row indices for FAOSTAT identifiers, add 1 and 2 to get indices for blue and gray water rows as well
temp <- as.tibble(which(!is.na(water_footprint$crops))) %>%
  rename(Green = value) %>%
  mutate(Blue = Green + 1,
         Gray = Green + 2) %>%
  #Turn into one column
  pivot_longer(
    cols = everything(),
    names_to = 'WF_type',
    values_to = 'Indices'
  )

#Filter out the FAOSTAT identifier rows
water_footprint$crops <- water_footprint$crops %>%
  slice(., temp$Indices) %>%
  #Fill in the FAOSTAT identifier for the blue and gray water
  fill(`Product`) %>%
  
  #Turn into long format
  pivot_longer(.,
               cols = -c(Product, WF_type),
               names_to = 'Country/Region',
               values_to = 'value') %>%
  
  #Select columns
  select(`Country/Region`, Product, WF_type, value) %>% unique()

#Column names animals
temp <- raw$water_footprint_animals %>%
  slice(2:3) %>% #Rows with the names
  pivot_longer(cols = everything()) %>% #Pivot longer to combine the rows together
  mutate(value = case_when(
    name %in% c('...11', '...12', '...13') & is.na(value) ~ 'World Average',
    name %in% c('...823', '...824', '...825') & is.na(value) ~ 'Zimbabwe',
    TRUE ~ value
  )) %>%
  group_by(name) %>% #Combine the rows belonging together
  summarise_all(paste0, collapse = '-') %>% ungroup() %>%
  mutate(name = name %>% #Fix names so they can be arranged in the correct order
           str_replace('Appendix V. Water footprint of  animal products \\(m3/ton\\). Period 1996-2005', '1') %>%
           str_replace('...', ''),
         value = value %>%
           str_replace(., '-NA', '') %>%
           str_replace('Country-Production system >>', 'WF_type')
           ) %>%
  mutate(name = as.numeric(name)) %>%
  arrange(name)
names(raw$water_footprint_animals) <- temp$value

#Clean up dataframe
water_footprint$animals <- raw$water_footprint_animals %>%
  select(`Product discription (HS)`, WF_type, ends_with('Weighted average')) %>% #Choose the weighted averages
  select(-starts_with('World Average')) %>%
  slice(4:n()) %>% #Remove unnecessary rows
  fill(`Product discription (HS)`) %>% #Fill in missing identifiers
  filter(str_detect(`Product discription (HS)`, '\\blive\\b')) %>% #Filter out live animals
  rename(Product = `Product discription (HS)`) %>%
  
  #Pivot longer
  pivot_longer(.,
               cols = -c(Product, WF_type),
               names_to = 'Country/Region',
               values_to = 'value')


#Both together
water_footprint <- bind_rows(water_footprint) %>%
  
  #Calculate total water footprint
  mutate(value = as.numeric(value)) %>%
  pivot_wider(.,
              names_from = WF_type,
              values_from = value) %>%
  #Calculate a total column 
  mutate(Total = Green + Blue + Grey) %>%
  
  #Turn long again
  pivot_longer(.,
               cols = -c(Product, `Country/Region`),
               names_to = 'WF_type',
               values_to = 'value') %>%
  
  #Set food group names
  mutate(Food = case_when(
    
    #Cereals
    Product %in% c('Wheat', 'Rice, paddy', 'Barley',
                   'Maize', 'Rye', 'Oats', 'Millet',
                   'Sorghum', 'Buckwheat', 'Quinoa',
                   'Fonio', 'Triticale', 'Canary seed',
                   'Mixed grain', 'Cereals, nes') ~ 'Cereals',
    
    #Fruit and vegetables
    Product %in% c('Potatoes', 'Sweet potatoes', 'Cassava',
                   'Yautia (cocoyam)', 'Taro (coco yam)',
                   'Yams', 'Roots and tubers nes', 'Cabbages and other brassicas',
                   'Artichokes', 'Asparagus', 'Lettuce and chicory',
                   'Spinach', 'Tomatoes', 'Cauliflowers and broccoli',
                   'Pumpkins, squash and gourds', 'Cucumbers and gherkins',
                   'Eggplants (aubergines)', 'Chillies and peppers, green',
                   'Onions (inc. shallots), green', 'Garlic',
                   'Carrots and turnips', 'Okra', 'Vegetables, fresh nes',
                   'Lemons and limes', 'Grapefruit (inc. pomelos)',
                   'Citrus fruit, nes', 'Apples', 'Pears', 'Apricots',
                   'Sour cherries', 'Cherries', 'Peaches and nectarines',
                   'Plums and sloes', 'Stone fruits, nes', 'Strawberries',
                   'Raspberries', 'Gooseberries', 'Currants', 'Grapes',
                   'Blueberries', 'Cranberries', 'Berries, nes', 'Watermelons',
                   'Other melons (inc.cantaloupes)', 'Figs', 'Mangoes, mangosteens, guavas',
                   'Avocados', 'Pineapples', 'Dates', 'Cashewapple', 'Kiwi fruit',
                   'Papayas', 'Fruit, tropical fresh nes', 'Fruit Fresh Nes') ~ 'Fruit and\nvegetables',
    
    #Legumes
    Product %in% c('Beans, dry', 'Chick peas', 'Cow peas, dry',
                   'Pigeon peas', 'Lentils', 'Bambara beans',
                   'Vetches', 'Lupins', 'Pulses, nes', 'Soybeans',
                   'Groundnuts in shell', 'Beans, green',
                   'Peas, green', 'String beans') ~ 'Legumes',
    
    #Nuts
    Product %in% c('Brazil nuts, with shell', 'Cashew nuts',
                   'Chestnuts', 'Almonds, with shell',
                   'Walnuts, with shell', 'Pistachios', 'Kolanuts',
                   'Hazelnuts, with shell', 'Arecanuts', 'Nuts, nes') ~ 'Nuts',
    
    
    #Animals
    str_detect(Product, 'Bovine') ~ 'Beef',
    str_detect(Product, 'Sheep') ~ 'Sheep',
    str_detect(Product, 'Swine') ~ 'Pork',
    str_detect(Product, 'Poultry') ~ 'Poultry'
    
  )) %>%
  #Remove products not needed
  drop_na(Food) %>%
  
  #Calculate mean, median, 5th and 95th percentile
  group_by(Food, WF_type) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              `5th pctl` = quantile(value, 0.05, na.rm = TRUE),
              `95th pctl` = quantile(value, 0.95, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              min = min(value, na.rm = TRUE)) %>% ungroup() %>%
  
  #Set order of the Food and WF_type columns
  group_by(Food) %>%
  arrange(match(WF_type, c('Total', 'Green', 'Blue', 'Grey'))) %>% ungroup() %>%
  arrange(match(Food, c('Beef', 'Sheep', 'Pork', 'Poultry', 'Legumes', 'Nuts', 'Cereals', 'Fruit and\nvegetables'))) %>%
  
  #Turn long for ggplot
  pivot_longer(.,
               cols = -c(Food, WF_type),
               names_to = 'statistic',
               values_to = 'value')


## Land use----
land_use <- raw$land_use %>%
  
  #Select relevant columns
  select(...1, `Resampled, Randomized Data`, ...4, ...5, ...7) %>%
  
  #Rename columns
  rename(Product = ...1,
         `5th pctl` = `Resampled, Randomized Data`,
         mean = ...4,
         median = ...5,
         `95th pctl` = ...7) %>%
  #Remove unnecessary rows
  slice(3:45) %>%
  
  #Set food group names
  mutate(Food = case_when(
    
    #Cereals
    Product %in% c('Wheat & Rye (Bread)', 'Maize (Meal)',
                   'Barley (Beer)', 'Oatmeal', 'Rice') ~ 'Cereals',
    
    #Fruit and vegetables
    Product %in% c('Potatoes', 'Cassava', 'Tomatoes', 'Onions & Leeks',
                   'Root Vegetables', 'Brassicas', 'Other Vegetables',
                   'Citrus Fruit', 'Bananas', 'Apples', 'Berries & Grapes',
                   'Other Fruit') ~ 'Fruit and\nvegetables',
    
    #Legumes
    Product %in% c('Other pulses', 'Peas', 'Groundnuts') ~ 'Legumes',
    
    #Nuts
    Product %in% c('Nuts') ~ 'Nuts',
    
    #Animals
    Product == 'Bovine Meat (beef herd)' ~ 'Beef',
    Product == 'Lamb & Mutton' ~ 'Lamb and\nMutton',
    Product == 'Pig Meat' ~ 'Pork',
    Product == 'Poultry Meat' ~ 'Poultry',
    Product == 'Fish (farmed)' ~ 'Fish\nfarmed',
    Product == 'Crustaceans (farmed)' ~ 'Shellfish\nfarmed'
    
  )) %>%
  
  drop_na(Food) %>%
  select(-Product) %>%
  
  #Use mean of the various statistics
  pivot_longer(.,
               cols = -Food,
               names_to = 'statistic',
               values_to = 'temp') %>%
  mutate(temp = as.numeric(temp)) %>%
  group_by(Food, statistic) %>%
  summarise(value = mean(temp)) %>% ungroup() %>%
  
  #Set order of the Food column
  arrange(match(Food, c('Beef', 'Lamb and\nMutton', 'Pork', 'Poultry', 'Fish\nfarmed', 'Shellfish\nfarmed', 'Legumes', 'Nuts', 'Cereals')))

rm(raw)
rm(temp)

## Plots----

#GHGE
GHGE_plot <- ggplot() +
  
  #Add the 5th-95th percentile line
  geom_line(data = GHGE %>%
              filter(statistic %in% c('5th pctl', '95th pctl')),
            aes(group = Food, x = Food, y = value), size =1, color = '#000000') +
  
  #Add points for the mean and median
  geom_point(data = GHGE %>%
               filter(statistic %in% c('mean', 'median')),
             aes(x = Food, y = value, shape = statistic, fill = statistic), size = 2, show.legend = FALSE) +
  
  #Set the shapes, oolors and discrete x axis
  scale_shape_manual(values = c('mean' = 21, 'median' = 23)) +
  scale_fill_manual(values = c('mean' = '#000000', 'median' = '#FFFFFF')) +
  scale_x_discrete(limits = unique(GHGE$Food)) +
  
  #Fix labels and add source
  labs(
    title = 'Greenhouse gas emissions',
    y = expression(kg~CO[2]-eq/kg~produce),
    x = 'Food group'#,
   # caption = 'Source: Clune, S., Crossin, E. & Verghese, K. Systematic review of greenhouse gas emissions for different fresh food categories.\nJ. Clean. Prod. 140, 766–783 (2017).'
  ) +
  #Place caption to the left
  theme(plot.caption.position = 'plot',
        plot.caption = element_text(hjust = 0, size = 8))

GHGE_plot

#Water footprint
WF_plot <- ggplot() +
  
  #Add the 5th-95th percentile line
  geom_line(data = water_footprint %>%
              filter(statistic %in% c('5th pctl', '95th pctl')) %>%
              mutate(WF_type = factor(WF_type, levels = c('Total', 'Green', 'Blue', 'Grey'))),
            aes(group = interaction(Food, WF_type), x = Food, y = value, color = WF_type), size = 1, position = position_dodge(width = 0.5)) +
  
  #Add points for the mean and median
  geom_point(data = water_footprint %>%
               filter(statistic %in% c('mean', 'median')) %>%
               mutate(WF_type = factor(WF_type, levels = c('Total', 'Green', 'Blue', 'Grey'))),
             aes(group = interaction(Food, WF_type), x = Food, y = value,
                 shape = statistic, fill = statistic), size = 2, position = position_dodge(width = 0.5),
             show.legend = FALSE) +
  
  #Set the shapes, oolors and discrete x axis
  scale_shape_manual(values = c('mean' = 21, 'median' = 23)) +
  scale_color_manual(values = c('Total' = 'Black', 'Green' = '#4AC16C', 'Blue' = '#375A8C', 'Gray' = 'Gray')) +
  scale_fill_manual(values = c('mean' = '#000000', 'median' = '#FFFFFF')) +
  scale_x_discrete(limits = unique(water_footprint$Food)) +
  
  #Fix labels and add source
  labs(
    title = 'Water footprint ',
    y = 'L/kg',
    x = 'Food group',
    color = ''#,
   # caption = 'Source: Gerbens-Leenes, P., Mekonnen, M. & Hoekstra, A. The water footprint of poultry, pork and beef: A comparative study in different countries and production systems.\nWater Resour. Ind. 1–2, 25–36 (2013).\nMekonnen, M. & Hoekstra, A. The green, blue and grey water footprint of crops and derived crop products. Hydrol. Earth Syst. Sci. 15, 1577–1600 (2011).'
  ) +
  
  #Place legend and caption
  guides(color = guide_legend(nrow = 1)) + #One row
  theme(legend.position = c(0.26,1), #Legend under title
        plot.caption.position = 'plot',
        plot.caption = element_text(hjust = 0, size = 8)) #Caption to the left

WF_plot

#Landuse
landuse_plot <- ggplot() +
  
  #Add the 5th-95th percentile line
  geom_line(data = land_use %>%
              filter(statistic %in% c('5th pctl', '95th pctl')),
            aes(group = Food, x = Food, y = value), size = 1, color = '#000000') +
  
  #Add points for the mean and median
  geom_point(data = land_use %>%
               filter(statistic %in% c('mean', 'median')),
             aes(x = Food, y = value, shape = statistic, fill = statistic), size = 2, show.legend = FALSE) +
  
  #Set the shapes, oolors and discrete x axis
  scale_shape_manual(values = c('mean' = 21, 'median' = 23)) +
  scale_fill_manual(values = c('mean' = '#000000', 'median' = '#FFFFFF')) +
  scale_x_discrete(limits = unique(land_use$Food))  +
  
  #Fix labs
  labs(
    title = 'Land use',
    y = expression(m^2/kg),
    x = 'Food group'#,
    #caption = 'Source: Poore, J. & Nemecek, T. Reducing food’s environmental impacts through producers and consumers. Science (80-. ). 360, 987–992 (2018).'
  ) +
  #Place caption to the left
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.caption.position = 'plot')

landuse_plot

#Explanation plot
#Some sample data
temp <- tibble(
  'x' = c('test', 'test', 'test','test'),
  'statistic' = c('mean', 'median', '5th pctl', '95th pctl'),
  'value' = c(45, 55, 5, 95)
)

explanation_plot <- ggplot() +
  
  #Add line
  geom_line(data = temp %>% filter(statistic %in% c('5th pctl', '95th pctl')),
            aes(x = x, y = value), size = 1, color = '#000000') +
  #Add median and mean
  geom_point(data = temp %>% filter(statistic %in% c('mean', 'median')),
             aes(x = x, y = value, shape = statistic, fill = statistic), size = 2) +
  
  #Add explanatory text
  geom_text(aes(x = 1, y = 95), label = '95th pctl', hjust = -0.1) +
  geom_text(aes(x = 1, y = 5), label = '5th pctl', hjust = -0.1) +
  geom_text(aes(x = 1, y = 45), label = 'Mean', hjust = -0.3) +
  geom_text(aes(x = 1, y = 55), label = 'Median', hjust = -0.2) +
  
  #Set shape and colors
  scale_shape_manual(values = c('mean' = 21, 'median' = 23)) +
  scale_fill_manual(values = c('mean' = '#000000', 'median' = '#FFFFFF')) +
  
  #Turn blank except for figure
  labs(
    x = '',
    y = ''
  ) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        aspect.ratio = 5/1) +
  coord_cartesian(xlim = c(1.4,5),
                  ylim = c(0, 100))

explanation_plot

#All plots together
#First the three environmental impact plots
temp <- plot_grid(GHGE_plot + theme(axis.title.x = element_blank(),
                                    plot.margin = margin(0.1,0,0,0, 'cm')),
                  landuse_plot + theme(axis.title.x = element_blank(),
                                       plot.margin = margin(0.1,0,0,0, 'cm')),
                  WF_plot + theme(axis.title.x = element_blank(),
                                  plot.margin = margin(0.1,0,0,0, 'cm')),
                  nrow = 3)

temp
#The explanation plot
temp2 <- plot_grid(NULL, explanation_plot, NULL, nrow = 3,
                   rel_heights = c(1,10,1))
temp2

#Add the explanation plot
plots <- plot_grid(temp + theme(plot.margin = margin(0,0,0,0, 'cm')),
                   temp2 + theme(plot.margin = margin(0,0,0,0, 'cm')),
                   rel_widths = c(7, 1.5),
                   rel_heights = c(1, 0.1),
                   ncol = 2)

plots

save_plot('./thesis/images/environmental_impacts.png', plots,
          ncol = 1.5, nrow = 2.)

