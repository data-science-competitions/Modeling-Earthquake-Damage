#' @title Add Row Names from a Data Frame
#'
#' @description This is the inverse function of
#'   \link[tibble]{rownames_to_column}.
#'
#' @inheritParams tibble::column_to_rownames
#'
#' @details Given a data frame \code{.data} and variable name \code{var}, the
#'   function:
#' 1. Names the rows of \code{.data} using \code{var}; and
#' 2. Removed the column \code{var}.
#'
#' @family tibble functions
#' @export
#'
#' @seealso \link[tibble]{rownames_to_column}
#'
#' @examples
#' \dontrun{
#'  mtcars_without_rownames <- mtcars %>% tibble::rownames_to_column("id")
#'  mtcars_with_rownames <- column_to_rownames(mtcars_without_rownames, "id")
#' }
column_to_rownames <- function(.data, var){
    suppressWarnings(rownames(.data) <- .data[[var]])
    .data %>% dplyr::select(-!!var)
}

#' @title Treat Non-Finite Values
#'
#' @description Treatment of NA, NaN, -Inf and +Inf through safe replacement
#'   plus indicator column.
#'
#' @details
#' For every column in \code{data}:
#' * If the column has finite values, the column is left unchanges.
#' * Else:
#'   * \link{treat_na} adds an indicator colunms such that:
#'     * New column name is as the original + "_NA" suffix; and
#'     * Non-finite values are flagged with TRUE, finite values are flagged with
#'     FALSE.
#'   * \link[tidyr]{replace_na} replaces missing values in the original column
#'   using the given values in \code{replace}.
#'
#' @section Treatment of Non-Finite Values:
#' 1. \link[dplyr]{na_if}: replaces non-finite values with NA;
#' 2. \link{flag_na}: adds an NA indicator column; and
#' 3. \link[tidyr]{replace_na}: replaces missing values.
#'
#' @inheritParams tidyr::replace_na
#'
#' @inherit tidyr::replace_na return
#'
#' @family tidyr functions
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble(x = c(1, 2, Inf), y = c("a", NA, "b"))
#' tidy_df <- df %>% dplyr::mutate(x = dplyr::na_if(x, Inf)) %>% flag_na() %>% replace_na(list(x = 0, y = "unknown"))
#' identical(treat_non_finite(df), tidy_df)
#'  }
#'
treat_non_finite <- function(data, replace = list(), ...){
    stopifnot("list" %in% class(replace))
    is_not_finite <- function(x) identical(isFALSE(is.numeric(x)) | isTRUE(is.finite(x)), FALSE)
    if(length(replace) > 0 & any(sapply(replace, is_not_finite))) stop("replace contains invalid values")

    suppressWarnings(
        data %>%
            purrr::modify_if(is.numeric, dplyr::na_if, y = -Inf) %>%
            purrr::modify_if(is.numeric, dplyr::na_if, y = +Inf) %>%
            purrr::modify_if(is.numeric, dplyr::na_if, y = NaN) %>%
            treat_na(replace, ...)
    )
}

#' @title Treat NA Values
#'
#' @description Treatment of missing values through safe replacement plus
#'   indicator column.
#'
#' @details
#' For every column in \code{data}:
#' * If the column has no NA values, the column is left unchanges.
#' * Else:
#'   * \link{treat_na} adds an indicator colunms such that:
#'     * New column name is as the original + "_NA" suffix; and
#'     * NA values are flagged with TRUE, non-NA are flagged with FALSE.
#'   * \link[tidyr]{replace_na} replaces missing values in the original column
#'   using the given values in \code{replace}.
#'
#' @section Treatment of Missing Values:
#' 1. \link{flag_na}: adds an NA indicator column; and
#' 2. \link[tidyr]{replace_na}: replaces missing values.
#'
#' @inheritParams tidyr::replace_na
#'
#' @inherit tidyr::replace_na return
#'
#' @family tidyr functions
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble(x = c(1, 2, 3), y = c("a", NA, "b"))
#' tidy_df <- df %>% flag_na() %>% replace_na(list(x = 0, y = "unknown"))
#' identical(treat_na(df), tidy_df)
#'  }
#'
treat_na <- function(data, replace = list(), ...){
    stopifnot("list" %in% class(replace))
    if(any(is.na(replace))) stop("replace contains invalid values")
    return(data %>% flag_na() %>% tidyr::replace_na(replace, ...))
}

#' @title Flag NA Values
#'
#' @description Flag NA values in a new indicator column.
#'
#' @details
#' For every column in \code{data}:
#' * If the column has no NA values, the column is left unchanges.
#' * Else \code{fill_na} adds an indicator colunms such that:
#'   * New column name is as the original + "_NA" suffix; and
#'   * NA values are flagged with TRUE, non-NA are flagged with FALSE.
#'
#' @inheritSection treat_na Treatment of Missing Values
#'
#' @inheritParams tidyr::replace_na
#'
#' @inherit tidyr::replace_na return
#'
#' @family tidyr functions
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tibble(x = c(1, 2, 3), y = c("a", NA, "b"))
#' df %>% flag_na() %>% replace_na(list(x = 0, y = "unknown"))
#'  }
#'
flag_na <- function(data){
    has_no_NA <- function(x) !any(is.na(x))

    for(col_name in colnames(data)){
        if(has_no_NA(data[[col_name]])) next

        data <-
            data %>%
            dplyr::select(-dplyr::matches(paste0("^",col_name, "_NA","$"))) %>%
            tibble::add_column(!!paste0(col_name, "_NA") := is.na(data[[col_name]]), .after = col_name)
    }

    return(data)
}
