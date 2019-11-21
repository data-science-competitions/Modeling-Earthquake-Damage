# http://factominer.free.fr/index.html
# https://github.com/kassambara/factoextra
library(FactoMineR) # Perform Multivariate Exploratory Data Analysis
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses

# Multiple Factor Analysis (MFA) ------------------------------------------
fs <- FeatureStore$new()
data <- fs$tidy_data %>% dplyr::select(dplyr::starts_with("has_"))
