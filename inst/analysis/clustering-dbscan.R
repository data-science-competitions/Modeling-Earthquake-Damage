# Density Based Clustering of Applications with Noise (DBSCAN)
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html

# Setup ------------------------------------------------------------------------
library("dbscan")
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger")[3]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
historical_data <-
    ds$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    as.data.frame(stringsAsFactors = FALSE)

data <-
    historical_data %>%
    dplyr::select(dplyr::starts_with("geo_")) %>%
    purrr::map_dfc(function(x) x %>% as.character() %>% as.numeric()) %>%
    dplyr::filter(geo_level_1_id == 6) %>%
    dplyr::select(dplyr::matches("2|3"))

# Hierarchical DBSCAN -----------------------------------------------------
cl <- dbscan::hdbscan(data, minPts = 5)

# Cluster Summary Report --------------------------------------------------
print(cl)
print(cl$hc)
print(cl$cluster_scores) # Cluster Stability Scores (the higher the better)

# Visualisation -----------------------------------------------------------
par(pty = "m")
plot(cl, show_flat = TRUE) #, main = "HDBSCAN Simplified Tree")

par(pty = "m")
plot(cl$hc, main = "HDBSCAN Hierarchy")

par(pty = "s")
plot(data, col = cl$cluster + 1, pch = 20)
