# Setup -------------------------------------------------------------------
library(GA)
fs <- FeatureStore$new()
model_name <- "xgboost"
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <- fs$tidy_data %>% dplyr::left_join(by = "building_id", fs$geo_features)
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data")
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data")

# Sample the Data ---------------------------------------------------------
role_pk <- "building_id"
role_none <- NULL
role_input_1 <- match_columns(historical_data, "^geo_level_")
role_input_2 <- match_columns(historical_data, "^age")
role_input_3 <- match_columns(historical_data, "^has_superstructure_mud_mortar_stone$|_type$")
role_input <- c(role_input_1, role_input_2, role_input_3)
role_target <- "damage_grade"

train_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "train") %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "test") %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

# Formulate a Fitness Function --------------------------------------------
eval_function <- function(params_values){ #browser()
    stopifnot(exists("train_set"), exists("test_set"))

    pm <- PentaModel$new(path = file.path(.Options$path_models, model_name))
    pm$set_historical_data(train_set)
    pm$set_new_data(test_set)
    pm$set_role_pk(role_pk)
    pm$set_role_input(role_input)
    pm$set_role_target(role_target)

    pm$model_init()

    params <- list(params_values)[[1]]
    names(params) <- get("names", envir = parent.frame())
    print(round(params, 2))
    current_params <- pm$model_environment$params
    updated_params <- utils::modifyList(current_params, as.list(params))
    for(element in names(current_params)){
        if(!is.numeric(current_params[[element]])) next
        if(current_params[[element]] %% 1 != 0) next
        updated_params[[element]] <- updated_params[[element]] %>% round()
    }
    assign("params", updated_params, envir = pm$model_environment)

    pm$model_fit()
    pm$model_predict()

    data <-
        test_set %>%
        dplyr::left_join(pm$response, by = role_pk) %>%
        dplyr::rename("truth.numeric" = !!role_target, "estimate.numeric" = "fit") %>%
        dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric)) %>%
        dplyr::group_by(geo_level_1_id)

    Yardstick$
        new(data %>% dplyr::ungroup(), truth = "truth.class", estimate = "estimate.class")$
        set_estimator("micro")$
        insert_label(".model", pm$model_name)$
        f_meas %>% dplyr::slice(1) %>% .$.estimate
}

# Run Genetic Algorithm ---------------------------------------------------
ga_obj <- GA::ga(
    type = "real-valued",
    fitness = eval_function,
    lower = c(0.001, 0.01, 01, 00, 0.11, 0.11, 0.01, 0.01), # minimum values
    upper = c(0.999, 20.0, 15, 50, 0.99, 0.99, 9.99, 9.99), # maximum values
    names = c("eta", "gamma", "max_depth", "min_child_weight", "subsample", "colsample_bytree", "lambda", "alpha"),
    popSize = 2^4,    # population size
    maxiter = 2^3,    # number of iterations
    pcrossover = 0.8, # probability of crossover between pairs of chromosomes
    pmutation = 0.5,  # probability of mutation
    elitism = 0.25,   # percentage of elitism (fraction of best current solutions used on next round)
    suggestions = c(0.13, 1.09, 11, 27, 0.70, 0.70, 1.98, 1.64),
    parallel = FALSE,
    optim = FALSE,
    keepBest = TRUE,
    monitor = plot,
    seed = 1919
)
summary(ga_obj)
