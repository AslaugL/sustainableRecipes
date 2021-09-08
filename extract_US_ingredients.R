#Cleanup
library(tidyverse)
library(readxl)

#Get the recipes used by Andira----
#US large database
temp <- read_delim('./Data/recipes/ingredients_allrecipes_nomissing.csv', delim = ';')

#Clean up the recipe names
raw <- temp %>%
  mutate(recipe_id = str_replace_all(recipe_id, 'http://allrecipes.com/recipe/|/detail.aspx', ''),
         recipe_id = str_replace_all(recipe_id, '-', ' '))
#Used by Andira, according to extra file sent by Alain, chinese pepper steak is chinese pepper steak 2 in allrecipes
recipe_names <- read_xlsx('./Data/Oppskrifter/Data_US_100.xlsx') %>%
  select(`Selected Meals`) %>%
  rename(recipe_id = `Selected Meals`) %>% mutate(recipe_id = recipe_id %>%
                                                    tolower() %>%

                                                    #Remove characters not found in urls
                                                    str_replace_all('-', ' ') %>%
                                                    str_replace_all("'|\\(|\\)", "") %>%
                                                    #change chinese pepper steak
                                                    str_replace('chinese pepper steak', 'chinese pepper steak 2')
                                                  )

#Get only the recipes needed
raw_recipes <- raw %>%
  filter(recipe_id %in% recipe_names$recipe_id)

#Clean up dataframe----
clean_US <- raw_recipes %>%

  #Change names to the other dataframes and turn grams to kilos
  rename(`Selected Meals` = recipe_id,
         Ingredients = ingredient_name,
         Amounts_kg = ingredient_grams) %>%
  mutate(Amounts_kg = Amounts_kg/1000)


#No use to add some bicarbonate of soda, baking powder and salt to the recipe with self-raising flour, these ingredients are not in SHARP and the amounts are negliable
saveRDS(clean_US, './recipes/US_clean.Rds')

