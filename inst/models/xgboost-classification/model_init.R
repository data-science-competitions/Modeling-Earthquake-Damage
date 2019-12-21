#' @title Prepare everything the prediction model needs
model_init <- function(){
    install_non_installed_package <- function(pkg) if(is_package_not_installed(pkg)) install_package(pkg)
    is_package_not_installed <- function(pkg) !pkg %in% rownames(installed.packages())
    install_package <- function(pkg)  utils::install.packages(pkg, repos = getOption("repos", "https://cloud.r-project.org"), dependencies = TRUE)
    for(pkg in c("xgboost")) install_non_installed_package(pkg)

    dynamic_preprocessing_function <- function(){
        stopifnot(exists("role_input"), exists("role_target"))
        `%+%` <- function(a,b) paste0(a,b)

        matrix_formula_sting <- paste("~", paste(role_input, collapse = " + "))
        command <-
            "function(data, weight = rep(1,nrow(data))){" %+%
            "labels = data[[\"" %+% role_target %+% "\"]] - 1;" %+%
            "labels = if(is.null(labels)) numeric(nrow(data)) else labels;" %+%
            "xgboost::xgb.DMatrix(" %+%
            "data = Matrix::sparse.model.matrix(formula(" %+% matrix_formula_sting %+% "), data = data)," %+%
            "weight = weight, label = labels" %+%
            ")}"
        eval(parse(text = command))
    }
    preprocessing_function <- get("dynamic_preprocessing_function")()

    dynamic_predict_function <- function(){
        `%+%` <- function(a,b) paste0(a,b)
        preprocessing_function <- get("dynamic_preprocessing_function")()

        command <-
            c("function(model_object, new_data){",
            "preprocessing_function <- " %+% capture.output(preprocessing_function)[1],
            "new_data <- preprocessing_function(new_data)",
            "predict(object = model_object, newdata = new_data) %>%",
            "as.data.frame(stringsAsFactors = FALSE) %>%",
            "dplyr::rename('fit' = '.') %>%",
            "purrr::map_df(link_function)",
            "}")
        eval(parse(text = command))
    }
    predict_function <- get("dynamic_predict_function")()

    link_function <- function(x) x + 1

    model_config <- config::get(file = file.path(model_path, "model_config.yml"), use_parent = FALSE)

    list2env(model_config, envir = parent.frame())
    assign("install_non_installed_package", install_non_installed_package, envir = parent.frame())
    assign("is_package_not_installed", is_package_not_installed, envir = parent.frame())
    assign("install_package", install_package, envir = parent.frame())
    assign("preprocessing_function", preprocessing_function, envir = parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())
    assign("link_function", link_function, envir = parent.frame())
    return(invisible())
}
