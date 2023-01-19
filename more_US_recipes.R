library(tidyverse)
library(stringi)

#Read raw data
raw <- read_delim('./Data/recipes/ingredients_allrecipes_nomissing.csv', delim = ';')

#Set seed and randomize recipe id's
set.seed(42)

ids <- as.tibble(unique(raw$recipe_id)) %>%
  slice_sample(., n = 300)

#Extract the randomized recipes
recipes <- raw %>% filter(recipe_id %in% ids$value)

rm(raw)
rm(ids)


#Standardise ingredients
temp <- recipes %>%
  rename(sample_id = recipe_id) %>%
  mutate(Ingredients = paste0(ingredient_grams, ' g ', ingredient_name),
         sample_id = sample_id %>%
           str_replace_all('http://allrecipes.com/recipe/|/detail.aspx', '') %>%
           str_replace_all('-', ' ')) %>%
  select(-c(ingredient_id, ingredient_grams, ingredient_amount, ingredient_name))

test <- temp %>%
  mutate(Source = '') %>% standardiseRecipes()

t <- test %>%
  select(org_ingredients, Ingredients) %>% unique()
