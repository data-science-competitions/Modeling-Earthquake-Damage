# Setup -------------------------------------------------------------------
# http://factominer.free.fr/index.html
# https://github.com/kassambara/factoextra
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
library(FactoMineR) # Perform Multivariate Exploratory Data Analysis
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses

# Get the Data ------------------------------------------------------------
fs <- FeatureStore$new()
role_pk <- "building_id"
set.seed(0856)
data <- fs$tidy_data %>%
    dplyr::select(building_id, dplyr::starts_with("has_")) %>%
    purrr::modify_if(is.logical, factor, levels = c("FALSE", "TRUE")) %>%
    dplyr::sample_n(1e4) %>%
    column_to_rownames(role_pk)

# Multiple Factor Analysis (MFA) ------------------------------------------
data_mfa <-
    data %>%
    FactoMineR::MFA(
        group = c(11, 11), # 11 has_superstructure vars and 11 has_secondary_use vars
        type = c("n", "n"), # both groups are categorical variables
        name.group = c("superstructure", "secondary_use"),
        graph = FALSE
    )
# Extract the results for individuals.
MFA_scores <-
    tibble::as_tibble(data_mfa$ind$coord, rownames = role_pk) %>%
    dplyr::rename_at(
        dplyr::vars(dplyr::starts_with("Dim.")),
        function(x) paste0("dim_", stringr::str_remove(x, "Dim."))
    )

# Eigenvalues / Variances -------------------------------------------------
## Statistics
eig_val <- factoextra::get_eigenvalue(data_mfa)
head(eig_val)
## Scree Plot
factoextra::fviz_screeplot(data_mfa, ncp = Inf)

# Graph of Variables ------------------------------------------------------
choice <- c("quanti.var", "group", "quali.var")[3]
## Contribution Scatter Plot
factoextra::fviz_mfa_var(data_mfa, choice)
## Contribution Bar Graph
### Contribution to the first dimension
factoextra::fviz_contrib(data_mfa, choice, axes = 1, top = 11, palette = "jco")
### Contribution to the second dimension
factoextra::fviz_contrib(data_mfa, choice, axes = 2, top = 11)

# Graph of Individuals ----------------------------------------------------
factoextra::fviz_mfa_ind(
    data_mfa, addEllipses = FALSE, repel = FALSE,
    col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


