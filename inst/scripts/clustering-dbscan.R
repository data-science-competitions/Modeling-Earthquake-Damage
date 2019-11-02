# Density Based Clustering of Applications with Noise (DBSCAN)
# https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html

# Setup ------------------------------------------------------------------------
library("dbscan")
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger")[3]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
set.seed(1738)
historical_data <-
    ds$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::sample_n(1000)

# Sample the Data ---------------------------------------------------------
rset_obj <- sample_the_data(historical_data)
role_pk <- "building_id"
role_none <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("geo_"))
role_input <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("has_"))
role_target <- "damage_grade"

data <-
    historical_data %>%
    dplyr::select(dplyr::starts_with("geo_")) %>%
    dplyr::select(dplyr::matches("1|2"))

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


