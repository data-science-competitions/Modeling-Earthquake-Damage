#' @title Prepare everything the prediction model needs
#' @param model_name The model name (must be identical to the model folder name).
model_init <- function(model_name){
    ###########################
    ## Defensive Programming ##
    ###########################
    ## Do not edit this part by hand
    assertive::assert_is_a_non_empty_string(model_name)
    ## Here you may add your assertions. Useful expressions include:
    ## * base::missing() - test whether a value was specified as a function arg
    ## * base::stopifnot() - for conditional statements to ensure code integrity
    ## * if(conditional statements) stop(<failure reason>)
    ## * {assertive} - informative check functions to ensure code integrity


    ###########
    ## Setup ##
    ###########
    ## Load model helper functions
    rmonic::load_model_helper_functions(model_name)
    ## Initialize the caching engine
    model_archive <<- file.path(k_path_cache, model_name)
    unlink(model_archive, recursive = TRUE, force = TRUE)
    rmonic::create_archive(path = model_archive)
    ## Get model's metadata from yaml file and make it available globally
    ## Note: model's metadata may contain parameters to pass into the model
    model_config <- rmonic::load_model_config(model_name, k_path_models)


    ######################
    ## Global Variables ##
    ######################
    ## Add model metadata parameters to the global environment
    rmonic::list_to_env(model_config[["model_metadata"]])
    ## Add model parameters to the global environment
    rmonic::list_to_env(model_config[["model_parameters"]])
    ## Add input metadata parameters to the global environment
    rmonic::list_to_env(model_config[["data_source"]])
    ## Other important variables
    ### Generate slug tags from the model metadata
    slug_model <<- compose_tags(model_uid = model_config[["model_metadata"]]$model_uid)
    slug_model_fit <<- compose_tags(slug_model, source = "model_fit")
    slug_model_predict <<- compose_tags(slug_model, source = "model_predict")
    ### The sampled data split number. split_num = 0, may signal that the model
    ### was not executed under a data partition schema, such as K-fold CV or
    ### bootstrap.
    #split_num <<- 0


    ###################
    ## Data Pipeline ##
    ###################
    ## Here you may add your code


    ##################
    ## Calculations ##
    ##################
    ## Here you may add your code


    ############
    ## Return ##
    ############
    ## Do not edit this part by hand
    return(invisible())
}
