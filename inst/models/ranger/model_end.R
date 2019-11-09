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
        predict_function = predict_function,
        link = link_function,
        label = model_name,
        verbose = FALSE
    )

    ## Visualisation: Variable Importance
    figure_path <- file.path(target, "variable-importance.jpg")
    explanation <- ingredients::feature_importance(explainer, type = "difference") %>% plot()
    ggplot2::ggsave(figure_path, explanation, "jpeg", width = 297, height = 210, units = "mm")

    return(invisible())
}
