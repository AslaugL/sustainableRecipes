# sustainableRecipes
This is the code used for the analyses done in the article "Healthiness and environmental impact of dinner recipes vary widely across developed countries", based on the work done in my master thesis  in clinical nutrition, "Exploring the sustainability of dinner recipes" at the University of Bergen (the files for the master thesis can be found in the master_thesis branch of this repository). 

The code is able to recognize recipe ingredients, recalculate volume units into weight and calculate a recipe's nutrient content and environmental impact by using the three databases "Weights, measures and portion sizes for foods" from Norway, the Norwegian food composition datatable 2020 edition, and the SHARP Indicator database. 

Nutrient content can be compared to the macronutrient criteria from the World Health Organization and the Nordic Nutrition Recommendations, and the front-of-pack labels the UK multiple traffic light and French Nutriscore. 

Some volume/weight and nutrients were found from other sources than those listed which can be seen in the clean_database.R script. 

# Files and folders
- **Data** contains the databases used, the nutrient criteria for the two dietary guidelines and the front-of-pack labels and the recipes.
- **R** Contains functions used. standardiseIngredients and checkRef are used to standardise recipe ingredient names and map them to the databases. Choices to change one ingredient for a similar one are found in these scripts.
- **man** contains the help-manual for each of the functions written. 

# How to run
## Analyses
The analysis.R script contains the analyses done in the study to calculate healthiness, run statistical tests, build plots and tables. All files needed for redoing the analyses are present in the repository except the UK recipes that came from recipe books. The scripts are able to run without these recipes. In analysis.R the nutrient content and environmental impact of each individual ingredient and per 100g of each recipe has already been found. To redo these calculations see below.

## Make the databases and calculate nutrient content and environmental impact of individual recipes
Before analysis, the individual ingredients in the recipes were mapped to the nutrient and environmental impact databases. First the database ingredient names must be formatted similar to the ingredient names in the recipes to be used as query words, then the recipe ingredients are mapped to the databases and nutrient content and environmental impact per 100g is calculated.

### Clean databases and add composite ingredients to the databases
Open clean_databases.R and run.

The code will create two output files for each of the three databases used in the thesis. One output file is a shortened version of the database while the other is a set of words used to map recipe ingredients to the database.

Then run composite_ingredients.R and go back to clean_databases.R and unblock the code "Add composite ingredients" for the nutrient and sustainability database. This step must be done twice as some composite ingredients depend on other composite ingredients.

### Calculate nutrient content and environmental impact of the recipes
After preparing the databases, run all_recipes.R to map the recipe ingredients to the databases and calculate the nutrient content and environmental impact per 100 grams of each recipe.
