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
    print(ExplainerYardstick(explainer)$delete_label(".estimator")$insert_label(".dataset", "historical_data")$all_numeric_metrics)

    ## Visualisation: Agnostic Variable Importance
    figure_path <- file.path(target, "variable-importance-agnostic.jpg")
    explanation <- ingredients::feature_importance(explainer, type = "difference") %>% plot()
    ggplot2::ggsave(figure_path, explanation, "jpeg", width = 297, height = 210, units = "mm")

    ## Visualisation: XGBoost Variable Importance
    figure_path <- file.path(target, "variable-importance-xgboost.jpg")
    explanation <- xgboost::xgb.importance(model = model_object) %>% xgboost::xgb.ggplot.importance(top_n = 20)
    ggplot2::ggsave(figure_path, explanation, "jpeg", width = 297, height = 210, units = "mm")

    # Visualisation: A Singe Tree ---------------------------------------------
    # for(pkg in c("DiagrammeR", "DiagrammeRsvg", "rsvg")) install_non_installed_package(pkg)
    # figure_path <- file.path(target, "single-tree.pdf")
    # dgr_graph <- xgboost::xgb.plot.tree(model = pm$model_object, trees = 0, render = FALSE)
    # DiagrammeR::export_graph(dgr_graph, figure_path, width = 2970, height = 2100)

    return(invisible())
}
