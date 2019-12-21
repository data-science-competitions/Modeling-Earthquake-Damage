# Configuration -----------------------------------------------------------
options(verbose = FALSE)
enable_parallelism()

# Setup -------------------------------------------------------------------
library(GA)
fs <- FeatureStore$new()
model_name <- "xgboost-classification"
output_dir <- file.path(getOption("path_archive"), model_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the Data ------------------------------------------------------------
tidy_data <-
    fs$tidy_data %>%
    dplyr::left_join(by = "building_id", fs$geo_features) %>%
    dplyr::left_join(by = "building_id", fs$mfa_features) %>%
    dplyr::left_join(by = "building_id", fs$age_features)

# Get Variables Names -----------------------------------------------------
yaml_path <- file.path(getOption("path_models"), "model-variables.yml")
yaml_list <- yaml::read_yaml(yaml_path, eval.expr = TRUE)
list2env(yaml_list$default %>% lapply(merge_elements), envir = globalenv())

# Sample the Data ---------------------------------------------------------
historical_data <- tidy_data %>% dplyr::filter(.set_source %in% "historical_data")
new_data <- tidy_data %>% dplyr::filter(.set_source %in% "new_data")

# Split the Data ----------------------------------------------------------
train_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "train") %>%
    dplyr::select(-dplyr::starts_with(".")) %>%
    dplyr::select(role_pk, role_input, role_target, role_none)

test_set <-
    historical_data %>%
    dplyr::filter(.set_role %in% "validation") %>%
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
        dplyr::mutate(truth.class = as_earthquake_damage(truth.numeric), estimate.class = as_earthquake_damage(estimate.numeric))

    f1_micro <- yardstick::f_meas(data, truth = "truth.class", estimate = "estimate.class", estimator = "micro")
    return(f1_micro[[".estimate"]])
}

# Run Genetic Algorithm ---------------------------------------------------
ga_obj <- GA::ga(
    type = "real-valued",
    fitness = eval_function,
    lower = c(0.01, 01, 01, 0.01, 0.01), # minimum values
    upper = c(0.10, 20, 50, 0.99, 0.99), # maximum values
    names = c("eta", "max_depth", "min_child_weight", "subsample", "colsample_bytree"),
    popSize = 2^4,    # population size
    maxiter = 2^3,    # number of iterations
    pcrossover = 0.8, # probability of crossover between pairs of chromosomes
    pmutation = 0.5,  # probability of mutation
    elitism = 0.25,   # percentage of elitism (fraction of best current solutions used on next round)
    suggestions = c(0.02, 06, 15, 0.95, 0.80),
    parallel = FALSE,
    optim = FALSE,
    keepBest = TRUE,
    monitor = plot,
    seed = 1919
)
summary(ga_obj)
