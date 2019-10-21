# Helper Functions -------------------------------------------------------------
sample_the_data <- function(.data){
    .data %>%
        dplyr::group_by(damage_grade) %>%
        dplyr::sample_frac(size = 0.1) %>%
        rsample::initial_split(prop = 0.7, strata = "damage_grade")
}

# Get the Data -----------------------------------------------------------------
historical_data <-
    DataStore$new()$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    as.data.frame(stringsAsFactors = FALSE)

# Sample the Data --------------------------------------------------------------
set.seed(1936)
rset_obj <- sample_the_data(historical_data)
role_pk <- "building_id"
role_none <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("geo_"))
role_input <- tidyselect::vars_select(names(historical_data), dplyr::starts_with("has_"))
role_target <- "damage_grade"

train_set <-
    get_rsample_training_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    get_rsample_test_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

# Run model ---------------------------------------------------------------
pm <- PentaModel$new(path = file.path(.Options$path_models, "rpart-tree"))
pm$set_historical_data(train_set)
pm$set_new_data(test_set)
pm$set_role_pk(role_pk)
pm$set_role_input(role_input)
pm$set_role_target(role_target)

pm$model_init()
pm$model_fit()
pm$model_predict()

# Evaluate Model ----------------------------------------------------------
library(yardstick)
evaluate_model <- function(data, truth, estimate){
    stopifnot(all(c(truth, estimate) %in% colnames(data)))

    # Regression Metrics
    data[, truth] <- data[, truth] %>% as.character() %>% as.numeric()
    regression_metrics <- tibble::tibble()
    criteria <- c("rmse", "mae", "iic")
    for(criterion in criteria){
        command <- paste0("yardstick::", criterion, "(data, !!truth, !!estimate)")
        regression_metric <- try(eval(expr = parse(text = command)))
        if("try-error" %in% class(regression_metric)) next()
        regression_metrics <- dplyr::bind_rows(regression_metrics, regression_metric)
    }

    # Classification Metrics
    data[, truth] <- data[, truth] %>% as.factor()
    classification_metrics <- tibble::tibble()

    # Return
    metrics <- dplyr::bind_rows(regression_metrics, classification_metrics)
    return(metrics)
}

metrics <- evaluate_model(
    data = dplyr::right_join(test_set, pm$response, by = role_pk),
    truth = role_target,
    estimate = colnames(pm$response)[2]
)

print(metrics)

# Cleanup -----------------------------------------------------------------
ls(pm, all.names = TRUE)
