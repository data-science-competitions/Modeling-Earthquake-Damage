# <https://github.com/mlflow/mlflow/tree/master/mlflow/R/mlflow>
# Installation -----------------------------------------------------------------
mlflow_version <- "1.2.0"
envname <- paste0("r-mlflow-", mlflow_version)
# devtools::install_version(
#     "mlflow", version = mlflow_version, repos = "http://cran.us.r-project.org",
#     dependencies = TRUE, upgrade = "never"
# )
reticulate::conda_create(envname)
reticulate::use_condaenv(envname, required = TRUE)
# reticulate::conda_install("r-mlflow", paste0("mlflow==", mlflow_version), pip = TRUE)
# system(paste0("pip install --user ", "mlflow==", mlflow_version), intern = TRUE)
mlflow::install_mlflow(python_version = "3.7")


# Tracking ---------------------------------------------------------------------
path_mlflow <- getOption("path_mlflow")

mlflow::mlflow_ui()

# system("mlflow server --host 0.0.0.0")
# mlflow::mlflow_set_tracking_uri(uri = "http://tracking-server:5000")
mlflow::mlflow_set_tracking_uri(uri = "http://localhost:5000")
# mlflow::mlflow_set_tracking_uri(uri = "127.0.0.1:5000")

client <- mlflow::mlflow_client(tracking_uri)
experiment_name <- "Test"
artifact_location <- path_mlflow
# experiment_obj <- mlflow::mlflow_get_experiment(client = client, name = experiment_name)
# experiment_id <- mlflow::mlflow_id(experiment_obj)

mlflow::mlflow_create_experiment(
    client = client,
    name = experiment_name,
    artifact_location = artifact_location
)


# mlflow::mlflow_set_experiment(experiment_name = "Test")
# mlflow::mlflow_ui()







