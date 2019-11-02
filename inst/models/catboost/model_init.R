#' @title Prepare everything the prediction model needs
model_init <- function(){
    is_package_not_installed <- function(pkg) !pkg %in% rownames(installed.packages())

    if(is_package_not_installed("catboost")){
        message("Installing catboost; this may take a few minutes")
        os <- Sys.info()[['sysname']]
        ver <- "0.18"
        github_slug <- 'https://github.com/catboost/catboost/releases/download/'
        url <- paste0(github_slug, "v", ver, "/catboost-R-", os, "-", ver, ".tgz")
        remotes::install_url(url, args = c("--no-multiarch", "--no-test-load"))
        # remotes::install_github('catboost/catboost', subdir = 'catboost/R-package')
    }

    predict_function <- function(model_object, new_data){
        predict(object = model_object, data = new_data, type = "response") %>%
            ranger::predictions() %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            dplyr::rename("fit" = ".")
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
