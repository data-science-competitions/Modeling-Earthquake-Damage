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

    ## Report: Model Performance
    print(ExplainerYardstick(explainer)$delete_label(".estimator")$insert_label(".dataset", "historical_data")$all_numeric_metrics)

    ## Visualisation: Agnostic Variable Importance
    figure_path <- file.path(target, "(variable-importance)(agnostic).jpg")
    fi <- ingredients::feature_importance(explainer, type = "difference")
    ggplot2::ggsave(figure_path, fi %>% plot(), "jpeg", width = 297, height = 210, units = "mm")

    ## Visualisation: Partial Dependence Plots
    var_names <-
        fi %>%
        dplyr::arrange(dplyr::desc(dropout_loss)) %>%
        dplyr::filter(!variable %in% c("_baseline_", "_full_model_")) %>%
        slice(1:5) %>% .$variable %>% as.character()
    for(var_name in var_names){
        var_class <- class(explainer$data[, var_name])
        var_levels <- dplyr::n_distinct(explainer$data[, var_name])
        variable_type <- dplyr::if_else("factor" %in% var_class, "categorical", "numerical")
        if(var_levels <= 2 | ("factor" %in% var_class & var_levels > 32)) next
        figure_path <- file.path(target, paste0("(pdp)(", which(var_names %in% var_name),")(",var_name, ").jpg"))
        pdp <- ingredients::partial_dependency(explainer, var_name, variable_type = variable_type)
        ggplot2::ggsave(figure_path, pdp %>% plot(), "jpeg", width = 297, height = 210, units = "mm")
    }

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
