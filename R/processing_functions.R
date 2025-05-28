#' Standardize Date Format in Data Frame
#'
#' Converts the `date` column of a data frame to `yearmon` format based on
#' common date string formats.   Supports "mm-dd-YYYY" and "YYYY-mm" formats.
#'
#' @param df A data frame or tibble that includes a `date` column as character or factor.
#'
#' @return The input data frame with the `date` column converted to `zoo::yearmon` format.
#' @importFrom dplyr %>% mutate
#' @importFrom zoo as.yearmon
#' @export
#'

standardize_date_format = function(df) {

  if (!is.na(as.yearmon(df[["date"]][1], format = "%m/%d/%Y"))) {

    df = df %>%
      mutate(date = as.yearmon(date, format = "%m/%d/%Y"))

  } else if (!is.na(as.yearmon(df[["date"]][1], format = "%Y-%m"))) {

    df = df %>%
      mutate(date = as.yearmon(date, format = "%Y-%m"))

  }

  return(df)
}


#' Create Comparison Data Frame of Sentiment and Benchmark Indicators
#'
#' Aggregates sentiment data to monthly averages, aligns it with a benchmark series,
#' and optionally applies detrending and smoothing operations.
#'
#' @param sentiment_df A data frame with `date` and `sentiment_num` columns.
#' @param benchmark_df A data frame with `date` and `benchmark` columns.
#' @param detrend Integer. Window size in years for the detrending operation. Default is 2.
#' @param smooth_win Integer. Window size in months for optional smoothing. Default is 1.
#'
#' @return A tibble with `date`, `sentiment_num`, and `benchmark` columns, processed as specified.
#' @importFrom dplyr %>% group_by summarise inner_join mutate across
#' @importFrom zoo as.yearmon
#' @importFrom slider slide_dbl
#' @export
#'
#' @examples
#' comparison_df = make_comparison_df(sentiment_df, benchmark_df, detrend = 1, smooth_win = 3)
make_comparison_df = function(sentiment_df, benchmark_df,
                              detrend = 2, smooth_win = 1) {

  comparison_df = sentiment_df %>%
    group_by(date = as.yearmon(date)) %>%
    summarise(sentiment_num = mean(sentiment_num), .groups = "drop") %>%
    inner_join(benchmark_df, by = "date")

  if (detrend > 0) {
    comparison_df = comparison_df %>%
      mutate(benchmark = benchmark - slide_dbl(benchmark, mean,
                                               .before = detrend * 12))
  }

  if (smooth_win > 0) {
    comparison_df = comparison_df %>%
      mutate(across(c("sentiment_num", "benchmark"), ~slide_dbl(., mean,
                                                                .before = smooth_win)))
  }

  return(comparison_df)
}


#' Construct Validation Data Frame
#'
#' Builds a validation table linking sentiment indicators to benchmark economic indicators,
#' allowing for optional detrending and smoothing. This function processes each pair of indicators
#' and prepares a comparison data frame using `make_comparison_df`.
#'
#' @param detrend_win Integer. Window size in years for detrending. Defaults to 0 (no detrending).
#' @param smooth_win Integer. Window size in months for smoothing. Defaults to 0 (no smoothing).
#' @param validation_table_path Character. Path to the validation CSV table containing columns:
#' `indicator`, `indicator_type`, `stakeholder`, and `topic`.
#' @param benchmark_folder_path Character. Path to the directory containing benchmark indicator CSVs.
#' @param numeric_sentiment_df A data frame containing pre-processed sentiment values
#' with at least the columns `date`, `stakeholder`, `topic`, and `sentiment_num`.
#' @param is_fusion_format Logical. Whether the benchmark files are in Fusion format. Defaults to `FALSE`.
#'
#' @return A tibble including columns for original validation entries, corresponding sentiment and
#' benchmark data frames, and a computed `comparison_df`.
#' @importFrom readr read_csv
#' @importFrom dplyr %>% filter mutate select
#' @importFrom purrr map2
#' @export
construct_validation_df = function(detrend_win = 0, smooth_win = 0,
                                   validation_table_path,
                                   benchmark_folder_path,
                                   numeric_sentiment_df,
                                   is_fusion_format = FALSE) {

  validation_data = read_csv(validation_table_path, show_col_types = FALSE) %>%
    filter(complete.cases(.))

  if ("stakeholder" %in% names(validation_data) & "topic" %in% names(validation_data)){

      validation_data = validation_data %>%
        mutate(sentiment_df = map2(stakeholder, topic,
                                   ~get_sentiment_df(numeric_sentiment_df, .x, .y)))

    } else if ("topic" %in% names(validation_data)){

      validation_data = validation_data %>%
        mutate(sentiment_df = map(topic,
                                   ~get_sentiment_df(numeric_sentiment_df, topic = .x)))

    } else if ("stakeholder" %in% names(validation_data)){

      validation_data = validation_data %>%
        mutate(sentiment_df = map(topic,
                                  ~get_sentiment_df(numeric_sentiment_df, stakeholder = .x)))
  }


  validation_data = validation_data %>%
    mutate(
      benchmark_df = map2(indicator, indicator_type,
                          ~get_benchmark_df(.x, .y, benchmark_folder_path, is_fusion_format)),
      comparison_df = map2(sentiment_df, benchmark_df,
                           ~make_comparison_df(.x, .y, detrend = detrend_win, smooth_win = smooth_win))
    )

  return(validation_data)
}

get_benchmark_df = function(indicator_name, indicator_type, folder_path, is_fusion) {

  benchmark_df = import_economic_indicator(indicator_name,
                            folder_path = folder_path,
                            is_fusion_format = is_fusion)
  return(benchmark_df)

}

get_sentiment_df = function(sentiment_df, stakeholder_val = NULL, topic_val = NULL) {


  filtered_sentiment_df = sentiment_df

  if(!is.null(stakeholder_val)){

    filtered_sentiment_df = filtered_sentiment_df %>%
      filter(stakeholder == stakeholder_val)

  }

  if(!is.null(topic_val)){

    filtered_sentiment_df = filtered_sentiment_df %>%
      filter(topic == topic_val)

  }


  filtered_sentiment_df = filtered_sentiment_df %>%
    select(date, sentiment_num)

  return(filtered_sentiment_df)

}

