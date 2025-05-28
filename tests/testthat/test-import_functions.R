test_that("import_economic_indicator works", {

  indicator_path = paste0(Sys.getenv("USERPROFILE"),
                          "\\OneDrive - Bank Of Israel\\",
                          "current_work\\real estate sentiment\\data",
                          "\\labor_market\\")

  temp_ind = import_economic_indicator(indicator_name = "job_vacancies",
                            folder_path = indicator_path,
                            is_fusion_format = TRUE)

  expect_equal(names(temp_ind), c("date","benchmark"))

})


