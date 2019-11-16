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
        verbose = getOption("Verbose")
    )

    ## Variable Importance
    figure_path <- file.path(target, "variable-importance.jpg")
    explanation <- ingredients::feature_importance(explainer, type = "difference") %>% plot()
    ggplot2::ggsave(figure_path, explanation, "jpeg", width = 297, height = 210, units = "mm")


    ## Plot an rpart model
    figure_path <- file.path(target, "single-tree.jpg")
    jpeg(figure_path, width = 800, height = 600)
    rpart.plot::rpart.plot(model_object, type = 2, branch.type = 5, extra = 101, yesno = FALSE)
    dev.off()
    return(invisible())
}
