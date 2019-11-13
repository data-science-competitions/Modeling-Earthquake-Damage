#' @title Post Modelling Operations
#' @description Execute code after completion of backtesting
model_end <- function(){
    ## Explore, Explain, and Debug model
    target <- file.path(getOption("path_archive", default = tempdir()), model_name, "explanations")
    dir.create(target, showWarnings = FALSE, recursive = TRUE)

    ## Visualisation: Variable Importance
    figure_path <- file.path(target, "variable-importance.jpg")
    explanation <- xgboost::xgb.importance(model = model_object) %>% xgboost::xgb.ggplot.importance(top_n = 20)
    ggplot2::ggsave(figure_path, explanation, "jpeg", width = 297, height = 210, units = "mm")

    return(invisible())
}
