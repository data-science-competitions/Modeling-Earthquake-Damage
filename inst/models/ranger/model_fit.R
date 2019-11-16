#' @title Fit One or More Models to a Training Set
#' @param historical_data (`data.frame`) A table where samples are in rows and features are in columns.
#' @param model_formula (`formula`) A symbolic description of the model to be fitted.
#' @note \code{model_fit} has access to variables which were assigned to \code{model_environment}.
#' @return A model object
model_fit <- function(historical_data, model_formula)
{
    set.seed(1451)

    mdl_obj <- ranger::ranger(
        model_formula,
        data = historical_data,
        verbose = getOption("Verbose"),
        num.trees = params$num.trees,
        mtry = eval(parse(text = params$mtry)),
        max.depth = params$max.depth,
        sample.fraction = params$sample.fraction,
        splitrule = params$splitrule,
        num.random.splits = params$num.random.splits,
        respect.unordered.factors = params$respect.unordered.factors
    )

    return(mdl_obj)
}
