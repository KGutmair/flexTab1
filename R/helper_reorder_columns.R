#' @description This helper function restructure the column order of the final Table1
#'
#' @param col_names: column names of the table1
#' @param treatment_order: in which order should the treatment variables be ordered?
#' @param group_order: in which order should the group variable within the treatment variable be ordered?
#' @inheritParams Table1_flex
#'
#' @return a character vector with the colnames of the Tab1, in the desired order
#'  @importFrom stringr str_extract str_starts fixed
#' @noRd
#'
#'


sort_columns <- function(tab1,
                         data,
                         treatment_order = NULL,
                         group_order = NULL,
                         group_var = FALSE,
                         treatment_arm = FALSE) {
  col_names <- colnames(tab1)

  # CAVE: I have to change this: I need this always to be run to ensure a stable
  # order even without specifying a group and treatment order. The problem is that
  # when when I have a group category that is alphabeticall after the "p-value"
  # or SMD colum, than in my output table, also p-value will be before the category.
  #--------------------------------------------------
  # 1.  group_variable and treatment_arm present
  #---------------------------------------------------

  # if (!is.null(group_order) & !is.null(treatment_order)) {
  if (!is.logical(group_var) & !is.logical(treatment_arm)) {
    # Identify p-value columns and SMD columns
    pvalue_cols <- col_names[grepl("_p-value$", col_names)]
    smd_cols <- col_names[grepl("_SMD$", col_names)]
    main_cols <- col_names[!grepl("_p-value$", col_names) & !grepl("_SMD$", col_names) &
      !grepl("name", col_names) & !grepl("variable", col_names)]


    # Insert p-values immediately after their corresponding  groups
    sorted_cols <- c()
    #---------------------------------------------------------------------
    # if treatment_order and group_order defined, then use this order, if not
    # use the normal levels of my variables
    #--------------------------------------------------------------------
    if (!is.null(group_order) & !is.null(treatment_order)) {
      treatment_order <- treatment_order
      group_order <- group_order
    } else {
      treatment_order <- sort(unique(data[[treatment_arm]]))
      group_order <- sort(unique(data[[group_var]]))
    }


    for (treat in treatment_order) {
      # Get all columns for this one treatment group
      relevant_cols <- main_cols[stringr::str_starts(main_cols, stringr::fixed(paste0(treat, " ")))]


      sorted_cols1 <- c()
      for (grp1 in group_order) {
        relevant_cols1 <- relevant_cols[stringr::str_extract(relevant_cols, "^[^\n]+") == paste(treat, grp1)]
        sorted_cols1 <- c(sorted_cols1, relevant_cols1)
      }

      # If a corresponding p-value exists, insert it after the group columns
      pvalue_col <- pvalue_cols[stringr::str_starts(pvalue_cols, stringr::fixed(paste0(treat, "_p-value")))]
      # If a corresponding SMD-value exists, insert it after the p-value
      smd_col <- smd_cols[stringr::str_starts(smd_cols, stringr::fixed(paste0(treat, "_SMD")))]
      sorted_cols <- c(sorted_cols, sorted_cols1, pvalue_col, smd_col)
    }
    tab1 <- tab1[, c("name", "variable", sorted_cols)]
    #-----------------------------------------------
    # when there is either group_var or treatment_arm presented
    #-----------------------------------------------

    # } else  if (!is.null(group_order) | !is.null(treatment_order))  {
  } else if (!is.logical(group_var) | !is.logical(treatment_arm)) {
    #---------------------------
    # group order or treatment order given
    #------------------------
    if (!is.null(group_order) | !is.null(treatment_order)) {
      grouping_order <- if (!is.null(group_order)) {
        group_order
      } else {
        treatment_order
      }
      #------------------
      # group order or treatment order is not given
      #-----------------
    } else {
      grouping_order <- if (!is.logical(group_var)) {
        sort(unique(data[[group_var]]))
      } else {
        sort(unique(data[[treatment_arm]]))
      }
    }

    pvalue_cols <- col_names[grepl("p-value$", col_names)]
    smd_cols <- col_names[grepl("SMD", col_names)]
    main_cols <- col_names[!grepl("p-value$", col_names) & !grepl("SMD", col_names)]


    # if either group_var or treatment_var is there, but not both
    sorted_cols <- c()

    for (treat in grouping_order) {
      # Get all columns for this treatment
      relevant_cols <- main_cols[stringr::str_extract(main_cols, "^[^\n]+") == treat]
      # If a corresponding p-value exists, insert it after the group columns
      # If a corresponding SMD-value exists, insert it after the p-value
      sorted_cols <- c(sorted_cols, relevant_cols)
    }

    tab1 <- tab1[, c("name", "variable", sorted_cols, pvalue_cols, smd_cols)]


    #---------------------------------------
    # when there is no stratification group
    #------------------------------------
  } else {
    tab1 <- tab1[, c(
      "name",
      "variable",
      sort(setdiff(names(tab1), c("name", "variable")))
    )]
  }
  return(tab1)
}
