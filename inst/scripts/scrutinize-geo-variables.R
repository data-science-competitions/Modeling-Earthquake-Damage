# Setup ------------------------------------------------------------------------
ds <- DataStore$new()
model_name <- c("arithmetic-mean", "rpart", "ranger")[3]
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
historical_data <-
    ds$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    dplyr::select(dplyr::starts_with("geo_")) %>%
    purrr::map_dfc(function(x) x %>% as.character() %>% as.numeric()) %>%
    dplyr::filter(geo_level_1_id == 6) %>%
    dplyr::select(dplyr::matches("2|3"))

new_data <-
    ds$data_model %>%
    dm::cdm_get_tables() %>%
    .$new_data %>%
    dplyr::select(dplyr::starts_with("geo_")) %>%
    purrr::map_dfc(function(x) x %>% as.character() %>% as.numeric()) %>%
    dplyr::filter(geo_level_1_id == 6) %>%
    dplyr::select(dplyr::matches("2|3"))

# Compare Geo Codes -------------------------------------------------------
geo_codes <-
    dplyr::bind_rows(historical_data = historical_data, new_data = new_data, .id = "set") %>%
    tidyr::unite("code", dplyr::starts_with("geo_"), sep = "-", remove = FALSE) %>%
    dplyr::distinct(set, code, .keep_all = TRUE)

intersect_codes <-
    dplyr::intersect(
        geo_codes %>% dplyr::filter(set %in% "historical_data") %>% dplyr::select("code"),
        geo_codes %>% dplyr::filter(set %in% "new_data") %>% dplyr::select("code")
    )

diff_codes <-
    dplyr::setdiff(
        geo_codes %>% dplyr::filter(set %in% "historical_data") %>% dplyr::select("code"),
        geo_codes %>% dplyr::filter(set %in% "new_data") %>% dplyr::select("code")
    )

message("Out of ", nrow(geo_codes), " distinct geo-codes:",
        "\n", 2*nrow(intersect_codes), " codes exist in both data sets; and",
        "\n", nrow(diff_codes), " codes exist in only one data set.")
