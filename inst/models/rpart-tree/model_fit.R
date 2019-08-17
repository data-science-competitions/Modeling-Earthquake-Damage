#' @title Fit One or More Models to a Training Set
#' @param training_set (`data.frame`) A table where samples are in rows and features are in columns.
model_fit <- function(training_set)
{
    ###########################
    ## Defensive Programming ##
    ###########################
    ## Do not edit this part by hand
    assertive::assert_has_rows(training_set)
    assertive::assert_all_are_existing(c("slug_model_fit", "split_num", "dataset_key_column", "model_archive"))
    ## Here you may add your assertions


    ###########
    ## Setup ##
    ###########
    # Remove the unique key column from the training set (to avoid overfitting)
    training_set <- training_set %>% select(-dataset_key_column)
    # Compose the stage tag slug
    current_stage_tags <- compose_tags(slug_model_fit, split = split_num)


    ################
    ## Fit Models ##
    ################
    ## Define formulas
    formulas <- c("MPG ~ .", "MPG ~ HP*GEAR + I(CYL^2)")
    ## Fit models
    foreach(m = seq_along(formulas)) %dopar% {
        ## Extract formula argument names
        formula <- as.formula(formulas[m])
        independent_vars <- all.vars(formula)[-1]
        dependent_var <- all.vars(formula)[+1]
        ## Compose model name
        model_tags <- compose_tags(current_stage_tags, formula = formula, dependent_var = dependent_var)
        ## Fit model to the data
        model_object <- base_learner_fit(data = training_set, formula = formula)
        ## Retain model object
        replace_artifact(model_object, model_tags, model_archive)
    }


    ############
    ## Return ##
    ############
    ## Do not edit this part by hand
    return(invisible())
}
