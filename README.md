# sustainableRecipes
This is all the code used in my master thesis in clinical nutrition, "Exploring the sustainability of dinner recipes" at the University of Bergen. The code is able to recognize recipe ingredients, recalculate volume units into weight and calculate a recipe's nutrient content and environmental impact by using the three databases "Weights, measures and portion sizes for foods" from Norway, the Norwegian food composition datatable 2020 edition, and the SHARP Indicator database. 

Nutrient content can be compared to the macronutrient criteria from the World Health Organization and the Nordic Nutrition Recommendations, and the front-of-pack labels the UK multiple traffic light and French Nutriscore. 

Some volume/weight and nutrients were found from other sources than those listed which can be seen in the clean_database.R script. 

# Files and folders
- **Data** contains the databases used, the nutrient criteria for the two dietary guidelines and the front-of-pack labels and the recipes.
- **R** Contains functions used. standardiseIngredients and checkRef are used to standardise recipe ingredient names and map them to the databases. Choices to change one ingredient for a similar one are found in these scripts.
- **man** contains the help-manual for each of the functions written. 
- **thesis** contains the individual tex and rmarkdown chapter files in addition to the figures for the thesis.

# How to run
## Clean databases and add composite ingredients to the databases
Open clean_databases.R and block out the code for adding composite ingredients to the nutrient and environmental impact databases. 

Run the code which will creae two output files for each of the three databases used in the thesis. One output file is a shortened version of the database while the other is a set of words used to map recipe ingredients to the database.

Then run composite_ingredients.R and go back to clean_databases.R and include the code to add composite ingredients. This step must be done twice as some composite ingredients depend on other composite ingredients.

## Calculate nutrient content and environmental impact of the recipes
After preparing the databases, run all_recipes.R to map the recipe ingredients to the databases and calculate the nutrient content and environmental impact per 100 grams of each recipe.

## Calculate healthiness, run statistical tests, build plots and tables
Run analysis.R script.
