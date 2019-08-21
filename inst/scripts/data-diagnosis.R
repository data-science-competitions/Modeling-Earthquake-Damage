## Setup
# install.packages("dlookr")
output_format <- c("pdf", "html")[2]
path_project <- tryCatch(rprojroot::find_rstudio_root_file(), error=function(e) getwd())
path_reports <- file.path(path_project, "reports")
dir.create(path_reports, showWarnings = FALSE, recursive = TRUE)

## Get datasets
dataset_list <- DataStore$new()$data_model %>% dm::cdm_get_tables()

## Create reports
for(k in seq_along(dataset_list)){
    dataset_name <- names(dataset_list)[k]
    dataset_data <- dataset_list[[k]]
    output_dir <- file.path(path_reports, dataset_name)
    message("Processing ", dataset_name)

    unlink(output_dir, force = TRUE, recursive = TRUE)
    dir.create(output_dir, showWarnings = FALSE)
    dlookr::diagnose_report(dataset_data,
                    output_format = output_format,
                    output_file = NULL,
                    output_dir)
}
