#' @title Instantiate the Prepare Interface
#'
#' @description Data preprocessing is a data mining technique that involves
#'   transforming raw data into an understandable format
#'
#' @return (`Prepare`) An interface that defines an abstract type that contains
#'   no data but defines behaviours as method signatures.
#'
#' @export
#'
#' @family Data Pipeline
#'
#' @docType class
#' @format \code{\link[R6]{R6Class}} object.
#' @keywords data
PrepareData <- R6::R6Class(
    classname = "PrepareData",
    inherit = Prepare,
    cloneable = FALSE,
    lock_objects = FALSE,
    private = list(
        # Private Variables ----------------------------------------------------
        .ingest = getOption("dataflows.ingest.concrete", "IngestData"),

        # Private Methods ------------------------------------------------------
        import_data.frames_from_Ingest = function() .import_data.frames_from_Ingest(private),
        cast_data = function() .cast_data(private)
    ),

    active = list(
        historical_data = function() private$.historical_data,
        new_data = function() private$.new_data,
        submission_format = function() private$.submission_format
    )
)#end PrepareData

# Private Methods: High-level Functions ----------------------------------------
.cast_data <- function(private){
    starts_with_has <- function(.data) .data %>% colnames() %>% stringr::str_detect("^has_") %>% which()

    table <- dplyr::bind_rows(train = private$.historical_data, test = private$.new_data, .id = "Source")

    table <-
        table %>%
        dplyr::mutate(
            building_id = as.character(building_id),
            geo_level_1_id = as.factor(geo_level_1_id),
            geo_level_2_id = as.factor(geo_level_2_id),
            geo_level_3_id = as.factor(geo_level_3_id),
            count_floors_pre_eq = as.integer(count_floors_pre_eq),
            age = as.integer(age),
            area_percentage = as.integer(area_percentage),
            height_percentage = as.integer(height_percentage),
            land_surface_condition = as.factor(land_surface_condition),
            foundation_type = as.factor(foundation_type),
            roof_type = as.factor(roof_type),
            ground_floor_type = as.factor(ground_floor_type),
            other_floor_type = as.factor(other_floor_type),
            position = as.factor(position),
            plan_configuration = as.factor(plan_configuration),
            legal_ownership_status = as.factor(legal_ownership_status),
            count_families = as.integer(count_families)
        ) %>%
        dplyr::mutate_at(starts_with_has(table), as.logical) %>%
        as.data.frame()

    private$.historical_data <- table %>% dplyr::filter(Source == "train") %>% dplyr::select(-Source)
    private$.new_data <- table %>% dplyr::filter(Source == "test") %>% dplyr::select(-Source)

    invisible(private)
}
