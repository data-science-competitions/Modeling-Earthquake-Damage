#' @title Prepare everything the prediction model needs
model_init <- function(){
    install_non_installed_package <- function(pkg) if(is_package_not_installed(pkg)) install_package(pkg)
    is_package_not_installed <- function(pkg) !pkg %in% rownames(installed.packages())
    install_package <- function(pkg)  utils::install.packages(pkg, repos = getOption("repos", "https://cloud.r-project.org"), dependencies = TRUE)
    for(pkg in c("rpart", "rpart.plot")) install_non_installed_package(pkg)

    predict_function <- function(model_object, new_data){
        predict(object = model_object, newdata = new_data) %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            dplyr::rename("fit" = ".") %>%
            purrr::map_df(link_function)
    }

    link_function <- function(x){ # 1 <= x <= 3
        minmax <- function(x, a, b) pmin(pmax(x, a), b)
        normalize <- function(x) if(max(x) == min(x)) x else (x - min(x)) / (max(x) - min(x))
        scale <- function(x) if(isTRUE(x %>% sd() > 0)) base::scale(x) else base::scale(x, TRUE, FALSE)

        y <- x %>% minmax(1, 3) %>% scale() %>% normalize()
        y <- y * 2 + 1

        as.vector(y)
    }

    model_config <- config::get(file = file.path(model_path, "model_config.yml"), use_parent = FALSE)

    list2env(model_config, envir = parent.frame())
    assign("predict_function", predict_function, envir = parent.frame())
    assign("link_function", link_function, envir = parent.frame())
    return(invisible())
}
