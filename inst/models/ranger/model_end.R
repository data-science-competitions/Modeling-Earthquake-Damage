#' @title Post Modelling Operations
#' @description Execute code after completion of backtesting
model_end <- function(){
    ## Explore, Explain, and Debug model
    target <- file.path(getOption("path_archive", default = tempdir()), model_name, "explanations")
    dir.create(target, showWarnings = FALSE, recursive = TRUE)
    explainer <- DALEX::explain(
        model = model_object,
        data = historical_data[, role_input],
        y = historical_data[[role_target]],
        predict_function = function(model, data) predict_function(model, data)[["fit"]],
        link = link_function,
        label = model_name,
        verbose = getOption("verbose")
    )

    ## Visualisation: Agnostic Variable Importance
    figure_path <- file.path(target, "(variable-importance)(agnostic).jpg")
    fi <- ingredients::feature_importance(explainer, type = "difference")
    ggplot2::ggsave(figure_path, fi %>% plot(), "jpeg", width = 297, height = 210, units = "mm")

    ## Visualisation: Partial Dependence Plots
    var_names <-
        fi %>%
        dplyr::arrange(dplyr::desc(dropout_loss)) %>%
        dplyr::filter(!variable %in% c("_baseline_", "_full_model_")) %>%
        top_n(10) %>% .$variable %>% as.character()
    for(var_name in var_names){
        n_values <- dplyr::n_distinct(explainer$data[, var_name])
        if(n_values <= 2 | n_values > 32) next
        variable_type <- dplyr::if_else("factor" %in% class(explainer$data[, var_name]), "categorical", "numerical")
        figure_path <- file.path(target, paste0("(pdp)(", var_name, ").jpg"))
        pdp <- ingredients::partial_dependency(explainer, var_name, variable_type = variable_type)
        ggplot2::ggsave(figure_path, pdp %>% plot(), "jpeg", width = 297, height = 210, units = "mm")
    }

    return(invisible())
}
