#' Import and Filter Sentiment Data Frame
#'
#' Reads a sentiment data frame from a `.rds` file, optionally filters it by topics and stakeholders,
#' and encodes sentiment categories numerically. The function excludes entries with undesired sentiment values.
#'
#' @param file_path Character. Path to the `.rds` file containing the sentiment data.
#' @param params Optional list. Contains filtering criteria:
#' \describe{
#'   \item{topics}{A vector or tibble with a column `value` used to filter by topic.}
#'   \item{stakeholders}{A vector or tibble with a column `value` used to filter by stakeholder.}
#' }
#' @param error_values Character vector. Sentiment labels to exclude. Default is \code{c("irrelevant", "null", "Error")}.
#'
#' @return A tibble with filtered sentiment data and an added numeric sentiment column: positive = 1, neutral = 0, negative = -1.
#'
#' @importFrom readr read_rds
#' @importFrom dplyr %>% inner_join filter mutate case_when
#' @importFrom tibble as_tibble
#' @export

import_sentiment_df = function(file_path, params = NULL,
                               error_values = c("irrelevant", "null", "Error")) {


  sentiment_df = read_rds(file_path)

  sentiment_df = sentiment_df %>%
    select(!contains("Unnamed"))


  if("stakeholder" %in% names(params) & "stakeholder" %in% names(sentiment_df)){

    sentiment_df = sentiment_df %>%
      inner_join(params$stakeholder %>% as_tibble(),
                 by = c("stakeholder" = "value"))

  }


  if("topic" %in% names(params) & "topic" %in% names(sentiment_df)){

    sentiment_df = sentiment_df %>%
      inner_join(params$topic %>% as_tibble(),
                 by = c("topic" = "value"))

  }


  numeric_sentiment_df = sentiment_df %>%
    filter(!sentiment %in% error_values) %>%
    mutate(sentiment_num = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "neutral" ~ 0,
      sentiment == "negative" ~ -1
    ))

  return(numeric_sentiment_df)
}


#' Import and Reformat Fusion CSV File
#'
#' Reads a CSV file from the Fusion dataset, extracts the relevant columns, and renames them.
#' The second column is renamed based on the lowercase prefix of the filename.
#'
#' @param file_path Character. The path to the Fusion `.csv` file.
#'
#' @return A tibble with two columns: `date` and a second column named after the file's base name (excluding `.csv`).
#' @importFrom readr read_csv
#' @importFrom stringr str_extract str_remove
#' @importFrom dplyr %>% select
#' @export
#'
#' @examples
#' df = import_fusion_format("data/economy.csv")
import_fusion_format = function(file_path) {
  raw_file = read_csv(file_path, show_col_types = FALSE)

  file_name = str_extract(file_path, "[a-z]+.csv") %>%
    str_remove("\\.csv")

  filtered_file = raw_file %>%
    select(TIME_PERIOD, OBS_VALUE)

  names(filtered_file) = c("date", file_name)

  return(filtered_file)
}


#' Import Economic Indicator Data
#'
#' Loads and processes an economic indicator from a CSV file. Automatically detects the date format
#' and supports reading both regular and Fusion-formatted files.
#'
#' @param indicator_name Character. Name of the indicator (used to construct the filename).
#' @param folder_path Character (optional). Path to the folder containing the CSV files. Defaults to a pre-defined OneDrive path.
#' @param is_fusion_format Logical. If `TRUE`, expects the file to follow the Fusion format.
#'
#' @return A tibble with two columns: `date` (converted to yearmon format) and `benchmark` (numeric indicator values).
#' @importFrom readr read_csv
#' @importFrom stringr str_replace_all
#' @importFrom dplyr %>% select mutate
#' @importFrom zoo as.yearmon
#' @export
#'
#' @examples
#' df = import_economic_indicator("housing index", is_fusion_format = TRUE)
import_economic_indicator = function(indicator_name,
                                     folder_path = NULL,
                                     is_fusion_format = FALSE) {

  if (is.null(folder_path)) {
    folder_path = paste0(Sys.getenv("USERPROFILE"),
                         "\\OneDrive - Bank Of Israel\\current_work",
                         "\\real estate sentiment\\validation",
                         "\\indices\\")
  }

  file_path = paste0(folder_path,
                     str_replace_all(indicator_name, " ", "_"),
                     ".csv")

  if (is_fusion_format) {

    benchmark_df = import_fusion_format(file_path) %>%
      select(date, benchmark = 2)

    } else {

    benchmark_df = read_csv(file_path, show_col_types = FALSE) %>%
      select(date, benchmark = 2)
  }

  benchmark_df =  standardize_date_format(benchmark_df)

  return(benchmark_df)
}


