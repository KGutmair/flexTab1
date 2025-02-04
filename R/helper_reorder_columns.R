#' This helper function resutructure the column order of the final Table1
#'
#' @param col_names: column names of the table1
#' @param treatment_order: in which order should the treatment variables be ordered?
#' @param group_order: in which order should the group variable within the treatment variable be ordered?
#'
#' @return a character vector with the colnames of the Tab1, in the desired order
#'  @importFrom stringr str_extract str_starts fixed
#'  @importFrom dplyr select
#'  @importFrom magrittr .
#' @noRd
#'
sort_columns <- function(data, treatment_order = NULL, group_order = NULL) {
  col_names <- colnames(data)

  # when there is a group order indicated
  if (!is.null(group_order)) {
    # when there is also a treatment order indicated
    if (!is.null(treatment_order)) {
      # Identify p-value columns and SMD columns
      pvalue_cols <- col_names[grepl("_p-value$", col_names)]
      smd_cols <- col_names[grepl("_SMD$", col_names)]
      main_cols <- col_names[!grepl("_p-value$", col_names) & !grepl("_SMD$", col_names) &
                               !grepl("name", col_names) & !grepl("variable", col_names)]
      print("main_cols")
      print(main_cols)

      # Insert p-values immediately after their corresponding treatment groups
      sorted_cols <- c()
      sorted_cols1 <- c()
      for (treat in treatment_order) {
        # Get all columns for this treatment
        relevant_cols <- main_cols[stringr::str_starts(main_cols, stringr::fixed(paste0(treat, " ")))]
        print("result treatment loop")
        print(treat)
        print(relevant_cols)

        for(grp1 in group_order) {
          relevant_cols1 <- relevant_cols[stringr::str_extract(relevant_cols, "^[^\n]+") == paste(treat, grp1)]
          print("result loop of group")
          print(relevant_cols1)
          sorted_cols1 <- c(sorted_cols1, relevant_cols1)
          print("sorted cols1")
          print(sorted_cols1)
        }

        # If a corresponding p-value exists, insert it after the group columns
        pvalue_col <- pvalue_cols[stringr::str_starts(pvalue_cols, stringr::fixed(paste0(treat, "_p-value")))]
        # If a corresponding SMD-value exists, insert it after the p-value
        smd_col <- smd_cols[stringr::str_starts(smd_cols, stringr::fixed(paste0(treat, "_SMD")))]
        sorted_cols <- c(sorted_cols, sorted_cols1, pvalue_col, smd_col)
        print("sorted cols")
        print(sorted_cols)
      }
      # when there is only a group order indicated
    } else {
      pvalue_cols <- col_names[grepl("_p-value$", col_names)]
      smd_cols <- col_names[grepl("_SMD$", col_names)]
      main_cols <- col_names[!grepl("_p-value$", col_names) & !grepl("_SMD$", col_names)]

      # Insert p-values immediately after their corresponding treatment groups
      sorted_cols <- c()
      for (treat in group_order) {
        # Get all columns for this treatment
        relevant_cols <- main_cols[stringr::str_extract(main_cols, "^[^\n]+") == treat]
        # If a corresponding p-value exists, insert it after the group columns
        pvalue_col <- pvalue_cols[stringr::str_starts(pvalue_cols, stringr::fixed(paste0(treat, "_p-value")))]
        # If a corresponding SMD-value exists, insert it after the p-value
        smd_col <- smd_cols[stringr::str_starts(smd_cols, stringr::fixed(paste0(treat, "_SMD")))]
        sorted_cols <- c(sorted_cols, relevant_cols, pvalue_col, smd_col)
      }
    }
    data <- data %>%
      select(name, variable, sorted_cols)


    # when there is neither a treatment order nor a group order indicated
  } else {
    data <- data %>%
      select(name, variable, sort(setdiff(names(.), c("name", "variable"))))
  }
  return(data)
}
