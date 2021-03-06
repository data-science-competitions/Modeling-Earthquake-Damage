# Helper-Functions -------------------------------------------------------------
calc_confusion_matrix <- function(actual, predicted){
    suppressWarnings(
        caret::confusionMatrix(
            data = predicted %>% as.factor(),
            reference = actual %>% as.factor()
        )
    )
}

sample_the_data <- function(.data){
    .data %>%
        dplyr::group_by(damage_grade) %>%
        dplyr::sample_frac(size = 0.1) %>%
        rsample::initial_split(prop = 0.7, strata = "damage_grade")
}

compose_formula <- function(role_pk = NULL, role_none = NULL, role_input, role_target){
    X <- role_input %>% setdiff(role_none) %>% setdiff(role_pk) %>% setdiff(role_target)
    y <- role_target

    formula(paste(y, "~", paste(X, collapse = " + ")))
}

minmax <- function(x, lb, ub) {
    stopifnot(lb < ub)
    .minmax <- function(x, lb, ub) min(ub, max(x, lb))
    sapply(x, .minmax, lb = lb, ub = ub)
}

# Get the Data -----------------------------------------------------------------
historical_data <-
    DataStore$new()$data_model %>%
    dm::cdm_get_tables() %>%
    .$historical_data %>%
    as.data.frame(stringsAsFactors = FALSE)

# Sample the Data --------------------------------------------------------------
#' <https://www.ibm.com/support/knowledgecenter/en/SSLVMB_23.0.0/spss/base/dataedit_roles.html>
#' role_target ~ role_input - role_none | role_pk
set.seed(1936)
rset_obj <- sample_the_data(historical_data)
role_pk <- "building_id" # private key
role_none <- c(
    tidyselect::vars_select(names(historical_data), dplyr::starts_with("geo_")),
    tidyselect::vars_select(names(historical_data), dplyr::starts_with("has_"))
)
role_input <- names(historical_data)
role_target <- "damage_grade"

train_set <-
    get_rsample_training_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, -role_none)

test_set <-
    get_rsample_test_set(rset_obj, split = 1) %>%
    dplyr::select(role_pk, role_input, role_target, -role_none)

# Fit Model --------------------------------------------------------------------
set.seed(1915)
mdl_formula <- compose_formula(role_pk, role_none, role_input, role_target)
mdl_obj <-
    randomForest::randomForest(
        mdl_formula, data = train_set,
        ntree = 1 , mtry = 3,
        importance = TRUE
    )

# Predict Test Set -------------------------------------------------------------
predict_function <- function(X, m) predict(m, X)
link_function <- function(x) x %>% minmax(lb = 1, ub = 3) %>% round()

response <- predict_function(X = test_set, m = mdl_obj) %>% link_function()
names(response) <- test_set[[role_pk]]
response <- tibble::enframe(response, name = role_pk, value = role_target)

# Model Evaluation -------------------------------------------------------------
median_value <- train_set[[role_target]] %>% median() %>% rep(nrow(test_set))

cm_baseline <- calc_confusion_matrix(actual = test_set[[role_target]], predicted = median_value)
cm_mdl <- calc_confusion_matrix(actual = test_set[[role_target]], predicted = response[[role_target]])

print(cm_baseline)
print(cm_mdl)

cat("Percentage Change from base-model to model-under-test")
M1 <- cm_baseline$overall
M2 <- cm_mdl$overall
round(100 * (M2 - M1) / abs(M1), 1)

# Visualisation ----------------------------------------------------------------
dplyr::bind_cols(actual = test_set[[role_target]], predicted = response[[role_target]]) %>%
    ggplot(aes(x = actual, y = predicted)) +
    ggtitle("Predicted vs Actual Values") +
    geom_point() +
    geom_count() +
    # scale_x_continuous(lim=c(0,50), breaks = (0:100) * 10, expand = c(0,0)) +
    # scale_y_continuous(lim=c(0,50), breaks = (0:100) * 10, expand = c(0,0)) +
    coord_equal(ratio = 1) +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw()

plot(mdl_obj, main = "Error as a Function of Number of Trees in the Forest", xaxt="n")
axis(side=1, at=(0:100)*25, labels = TRUE)

randomForest::varImpPlot(mdl_obj, n.var = nrow(mdl_obj$importance), scale = TRUE, main = "Feature Importance")

# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
library(help = randomForestExplainer)
library(randomForestExplainer)

url <- randomForestExplainer::explain_forest(mdl_obj, interactions = F, data = train_set)
browseURL(url)
# unlink(url)

# randomForestExplainer::plot_importance_rankings(mdl_obj)
# randomForestExplainer::plot_min_depth_distribution(mdl_obj)

# options(scipen=999)
randomForestExplainer::measure_importance(mdl_obj) %>% print(digits = 3)
