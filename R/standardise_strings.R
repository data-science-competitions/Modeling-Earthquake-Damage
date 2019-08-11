# standardise_strings ----------------------------------------------------------
#
#' @title Convert Any String to Specific Types of Character
#'
#' @description Given \code{allowed_letters} and
#'   \code{allowd_special_character}, when \code{standardise_strings} is
#'   called, all elements in \code{strings} are converted into a representation
#'   of the allowed character types.
#'
#' @details In addition to letters case folding and conversion of special
#'   characters, the function:
#'
#' * Separate camelcase strings into words (except when \code{allowed_letters}
#' is "allcases");
#' * Converts "#", "/" , "-", and "." into spaces; and
#' * Converts spaces into the specified \code{allowd_special_character}.
#' * Recognises formulas, e.g. \code{y ~ x1 + x2}, process the string while
#' keeping the formula special character (\code{~} and \code{+}).
#'
#' @param strings (`character`) Vector of strings to standardise.
#' @param allowed_letters (`character`) What type of letters are allowed? one of
#'   \code{anycase}, \code{lowercase} or \code{uppercase}.
#' @param allowd_special_character (`character`) What type of special
#'   characters are allowed? one of \code{underscores}, \code{hyphens} or
#'   \code{dots}.
#'
#' @section Illegal Values:
#'  * If a string is empty or contains only spaces/special-characters, the
#'  function throws an error.
#'
#' @section Special Cases:
#' * If a string matches time/date format, it will remain untouched.
#' * If a string contains tilde "\code{~}" the function ignores it.
#' * If a string contains plus sigb "\code{+}" the function ignores it.
#'
#' @return (`character`) The standardised version of \code{strings}.
#' @export
#'
#' @examples
#' \dontrun{
#' # WORKS
#' str_before <- "FinancialYear"
#' str_expected <- "FINANCIAL_YEAR"
#' str_after <- standardise_strings(str_before, "uppercase", "underscores")
#' identical(str_expected, str_after)
#'
#' # DOESN'T WORK: Invalid string
#' standardise_strings("_", "uppercase", "underscores")
#' standardise_strings("_", "uppercase", "dots")
#'
#' # Standardise column names
#' colnames(mtcars) <- standardise_strings(colnames(mtcars), "uppercase", "underscores")
#' }
#'
standardise_strings <- function(strings, allowed_letters, allowd_special_character)
{
    .standardise_strings_input_validation(strings, allowed_letters, allowd_special_character)

    allowed_letters <- tolower(allowed_letters)
    allowed_special_character <- tolower(allowd_special_character)
    special_character <- switch(allowed_special_character,
                          "underscores" = "_",
                          "hyphens" = "-",
                          "dots" = ".")

    for(k in seq_along(strings)){
        string <- strings[[k]]
        .assert_is_a_non_missing_nor_empty_string(string)

        string <-
            string %>%
            .split_camelcase() %>%
            .add_special_character(special_character) %>%
            .remove_excessive_special_character(special_character)
        string <- switch(allowed_letters,
                         "uppercase" = toupper(string),
                         "lowercase" = tolower(string),
                         string)

        .assert_is_a_non_missing_nor_empty_string(string)
        strings[[k]] <- string
    }

    return(strings)
}

# .standardise_strings_input_validation ----------------------------------------
.standardise_strings_input_validation <-
    function(strings, allowed_letters, allowd_special_character){
        stopifnot(isFALSE(missing(strings)))
        stopifnot(isFALSE(missing(allowed_letters)))
        stopifnot(isFALSE(missing(allowd_special_character)))
        match.arg(tolower(allowed_letters), c("uppercase", "lowercase", "anycase"))
        match.arg(tolower(allowd_special_character), c("underscores", "hyphens", "dots"))
        return(invisible())
    }

# .split_camelcase -------------------------------------------------------------
.split_camelcase <- function(string) {
    # Seperate camelcase words
    string <- gsub("(?<=[a-z])(?=[A-Z])", " \\1", string, perl = TRUE)
    # Connect cappital letters
    string <- gsub(paste0("(?:(?=\\b(?:\\p{Lu}\\h+){2}\\p{Lu})|",
                          "\\G(?!\\A))\\p{Lu}\\K\\h+(?=\\p{Lu})"),
                   "", string, perl = TRUE)
    # Remove extra spaces
    string <- gsub("^ *|(?<= ) | *$", "", string, perl = TRUE)

    return(string)
}

# .add_special_character -------------------------------------------------------
.add_special_character <- function(string, special_character) {
    `%+%` <- function(a, b) paste0(a, b)

    pattern <- paste(" ", ".", "-", "/", "#", "_", sep = "|\\")

    string %>%
        # Transform columns names separators to under scores
        stringr::str_replace_all(pattern, special_character) %>%
        # Add special_character before and after numbers
        stringr::str_replace_all("(?<=[a-z0-9])(?=[A-Z])|(?<=[a-zA-Z])(?=[0-9])", special_character) %>%
        # Find two or more consecutive underscores and replace them with one underscore
        stringr::str_replace_all("[" %+% special_character %+% "\t]{2,}", special_character)
}

# .remove_excessive_special_character ------------------------------------------
.remove_excessive_special_character <- function(x, special_character) {
    `%+%` <- function(a, b) paste0(a, b)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)

    mysub("[" %+% special_character %+% "\t\r\n]+$",
          mysub("^[" %+% special_character %+% "\t\r\n]+", x))
}
