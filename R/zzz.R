# .onAttach --------------------------------------------------------------------
.onAttach <- function(...) {#nocov start
    ######################
    ## Helper Functions ##
    ######################
    load_config <- function(value = NULL, config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                            file = Sys.getenv("R_CONFIG_FILE", "config.yml"), use_parent = TRUE){

        remove_special_apostrophe_from_yaml <- function(yaml_content){
            stopifnot(is.list(yaml_content))
            target_path <- tempfile()
            yaml::write_yaml(yaml_content, target_path)
            config <- readLines(target_path)
            config <- gsub("'!expr", "!expr", config)
            config <- gsub("('$)", "", config)
            return(config)
        }

        read_yaml_without_expression_evaluation <- function(file){
            yaml::read_yaml(
                file,
                eval.expr = FALSE,
                handlers = list(expr = function(x) {paste("!expr", x)})
            )
        }

        add_default_to_config <- function(config) unique(c("default", config))

        config <- add_default_to_config(config)
        yaml_content <- read_yaml_without_expression_evaluation(file)
        yaml_content <- yaml_content[config]
        yaml_content <- remove_special_apostrophe_from_yaml(yaml_content)

        yaml_path <- tempfile()
        base::get("writeLines")(text = yaml_content, con = yaml_path)

        suppressWarnings(
            config::get(value = value, config = config, file = yaml_path, use_parent = use_parent)
        )
    }

    find_CONFIGURATION_path <- function(){
        try_build_dir <- function(){
            list.files(path = ".",
                       pattern = "CONFIGURATION",
                       all.files = TRUE,
                       full.names = TRUE,
                       recursive = TRUE)[1]
        }

        try_package_dir <- function(){
            system.file("CONFIGURATION", package = utils::packageName())[1]
        }

        if(!is.na(try_build_dir())) return(try_build_dir())
        if(!is.na(try_package_dir())) return(try_package_dir())
        stop("Couldn't find CONFIGURATION")
    }

    #######################
    ## Programming Logic ##
    #######################
    source_path <- find_CONFIGURATION_path()
    invisible(load_config(file = source_path, config = c("default", "package")))
}#nocov end
