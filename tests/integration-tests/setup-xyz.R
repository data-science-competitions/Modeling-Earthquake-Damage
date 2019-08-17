.create_temp_folder()

.create_valid_mock_pentamodel <- function(path){
    writeLines("model_init <- function() NULL", file.path(path, "model_init.R"))
    writeLines("model_fit <- function() NULL", file.path(path, "model_fit.R"))
    writeLines("model_predict <- function() NULL", file.path(path, "model_predict.R"))
    writeLines("model_store <- function() NULL", file.path(path, "model_store.R"))
    writeLines("model_end <- function() NULL", file.path(path, "model_end.R"))
}
