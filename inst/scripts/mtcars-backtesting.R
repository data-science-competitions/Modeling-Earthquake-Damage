################################################################################
##                             Model Backtesting                              ##
################################################################################
#' WARNINGR:
#' Researching and backtesting is like drinking and driving.
#' Do not research under the influence of a backtest.
rmonic::setup()
rmonic::list_to_env(yaml::read_yaml(file.path(k_path_scripts, "backtesting.yml")))


#######################
## Configure Backend ##
#######################
if(allow_parallel) rmonic::parallelisation_on() else rmonic::parallelisation_off()


################
## Load Model ##
################
## Load model_init, model_fit, model_store, model_end located in model folder
rmonic::load_model_components(model_name, k_path_models)


#############################
## Load Data for Modelling ##
#############################
## Extract the data source details into global environment
model_config <- rmonic::load_model_config(model_name, k_path_models)
rmonic::list_to_env(model_config[["data_source"]], smart_parsing = TRUE)
## Get the data
rset_obj <- load_data_for_modelling()


##########################
## Initialise the Model ##
##########################
## Prepare everything our model needs
model_init(model_name)


###############
## Run Model ##
###############
message("\033[42m\033[46m", "\nModelling ", model_name, "\033[42m\033[49m")


## Find out how many splits the rsample object contains
K <- rmonic::get_rsample_num_of_splits(rset_obj)
## Loop over the dataset splits
start <- Sys.time()
foreach(k = seq_len(K), .errorhandling = "stop") %dopar% {
    message("\n", "\033[42m", "Running fold ", k,  " out of ", K,"\033[49m")

    ## Update global variables
    do.call(function(k) split_num <<- k, list(k = k), envir = .GlobalEnv)

    ## Extract the current training set and test set from the rsample objects
    training_set <- rmonic::get_rsample_training_set(rset_obj, k)
    test_set <- rmonic::get_rsample_test_set(rset_obj, k)

    ## Fit model(s) to the training set
    model_fit(training_set)

    ## Predict the test set
    model_predict(test_set)

    ## Store the results for further analysis
    model_store()

    ## Return NULL
    return(invisible())
}# for-loop

end <- Sys.time()
## Post-modelling operations
model_end()


print(round(end - start, 1))
