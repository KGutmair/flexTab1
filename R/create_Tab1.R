
#' Generate Table 1 with Customizable Summary Measures
#'
#'@description
#'This function creates a highly flexible Table 1 for descriptive statistics,
#'allowing users to customize output format, choose summary measures, and
#'optionally display missing values, p-values (testing for difference between two groups),
#'and standardized mean differences (SMD). It is designed to adapt to various
#'reporting needs and ensures comprehensive data summaries in a user-friendly format.
#'
#'
#' @param data A `data.frame` containing the data for which the Table 1 should be generated
#' @param variables A character vector specifying the names of the variables for which the Table 1 summary should be created.
#' @param group_var A character string specifying the name of the grouping variable for stratifying the Table 1 output.
#'        If set to `FALSE` (default), the Table 1 will not be stratified by groups.
#' @param treatment_arm A character string specifying the variable of the superior for comparing nested groups within superior groups.
#'        For example, in a dataset with three treatment groups ("treatment": A, B, C) and two subgroups within each
#'        treatment ("sub_group": I, II), set `group_var` to "sub_group" and `treatment_arm` to "treatment".
#'        If p-values or SMD options are selected, the subgroups within the treatment groups will be compared.
#'        Default is `FALSE` if no treatment arms are used.
#'
#' @param new_line logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, an empty row will be added after every variable
#'        in the table, which can improve clarity, especially when returning the Table 1 as a flextable object.
#'
#' @param measures_cat A character vector indicating how categorical variables should be summarized. Options are
#'        `"absolute"`, `"relative"`, or both. Default is `c("absolute", "relative")` for displaying both absolute and
#'        relative frequencies.
#'
#' @param measures_num A character vector defining the summary statistics for numeric variables. Options include `"median"`,
#'        `"min"`, `"max"`, `"quantile1"` (25th percentile), `"quantile3"` (75th percentile), `"mean"`, and `"sd"`
#'        (standard deviation). If the output is a data frame, any combination of measures can be selected. If the output is a
#'        flextable object, the number of measures is limited to three (e.g., mean(sd), median(min-max), or
#'        median(quantile1-quantile3)).
#'
#' @param display_pvalue logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, p-values will be calculated and displayed.
#'
#' @param display_smd logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, standardized mean differences (SMD)
#'        will be calculated and displayed.
#'
#' @param display_missings A logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, the frequency of missing values for
#'        each variable will be shown in the output.
#'
#' @param flextable_output A logical (TRUE/FALSE), default is `TRUE`. If `TRUE`, the Table 1 will be returned as a
#'        flextable object. If `FALSE`, it will be returned as a `data.frame`.
#'
#' @param sort_rows A character vector specifying the order in which the variables should appear in the output table.
#'        The default is NULL, which will sort the variables alphabetically
#'
#'@param add_measure_ident A logical (TRUE/FALSE), default is `TRUE`. If TRUE, the summary measures identifiers, e.g. median (min-max),
#'       mean (sd), n (%) will be added to the Table 1.
#'
#' @return A `data.frame` or `flextable` object, depending on the selected output option.
#'         The returned Table 1 contains the summary measures specified in the input parameters,
#'         including any chosen statistics, p-values, standardized mean differences (SMD), and missing value frequencies.
#' @export
#' @importFrom plyr rbind.fill
#' @importFrom checkmate assert_data_frame assert_vector assert_character assertSubset assertLogical
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
Table1_flex <- function(data,
                        variables,
                        group_var = FALSE,
                        treatment_arm = FALSE,
                        new_line = FALSE,
                        measures_cat = c("absolute", "relative"),
                        measures_num = c("median", "min", "max"),
                        display_pvalue = FALSE,
                        display_smd = FALSE,
                        display_missings = TRUE,
                        flextable_output = TRUE,
                        sort_rows = NULL,
                        add_measure_ident = TRUE) {

  ###############################
  # Testing input parameters
  ###############################

  assert_data_frame(data, min.rows = 1, min.cols = 1)
  assert_vector(variables, any.missing = FALSE, min.len = 1)

  if (any(!variables %in% colnames(data))) {
    stop(paste0(
      "At least any of the variable names ", variables, " cannot be found in the data"
    ))
  }

  assert_vector(group_var, any.missing = FALSE, len = 1)

  if (group_var != FALSE) {
    if (any(!group_var %in% colnames(data))) {
      stop(paste0("Grouping variable ", group_var, " is not in data frame"))
    }
    if (length(unique(data[, group_var])) <= 1) {
      stop("The grouping war has less then 2 categories")
    }
    if (is.numeric(data[, group_var])) {
      stop("The group_var variable must not be numeric")
    }
  }
  if (!is.logical(treatment_arm) & is.logical(group_var)) {
    stop(
      "No grouping variable was provided. Did you confound grouping
                    variable with treatment arm? Refere to the documentation for
                    the difference between treatment_arm and group_var"
    )
  }
  if(!is.null(sort_rows) & all(sort_rows %in% variables) == FALSE) {
    stop("The variable names provided in the `sort_row` argument are not included in the `variables` argument")
  }

  assert_character(
    measures_cat,
    any.missing = FALSE,
    max.len = 2,
    unique = TRUE
  )
  assertSubset(measures_cat, choices = c("absolute", "relative"))
  assert_character(
    measures_num,
    any.missing = FALSE,
    max.len = 3,
    unique = TRUE
  )
  assertSubset(
    measures_num,
    choices = c(
      "mean", "sd", "median", "min", "max",
      "quantile1", "quantile3"
    )
  )
  assertLogical(new_line, any.missing = FALSE, len = 1)
  assertLogical(display_pvalue, any.missing = FALSE, len = 1)
  assertLogical(display_smd, any.missing = FALSE, len = 1)
  assertLogical(display_missings, any.missing = FALSE, len = 1)
  assertLogical(flextable_output, any.missing = FALSE, len = 1)
  assertLogical(add_measure_ident, any.missing = FALSE, len = 1)

  if (is.logical(group_var) &
      (display_pvalue == TRUE | display_smd == TRUE)) {
    warning("Calculation of p-values and SMDs need at least two groups. Only one group provided, setting display_pvalues and display_smd to FALSE")
    display_pvalue <- FALSE
    display_smd <- FALSE
  }

  if (!is.logical(group_var) && (length(unique(data[, group_var])) > 2 &
                                 (display_pvalue == TRUE | display_smd == TRUE))) {
    warning("This is an experimental version. The method for comparing more than two groups is not implemented yet.")
    display_pvalue <- FALSE
    display_smd <- FALSE
  }

  options(dplyr.summarise.inform = FALSE)

  #########################################################
  # Step 1: Check, if and which variables are numeric
  #########################################################
  num_vec <- c()
  cat_vec <- c()
  var_type <- sapply(data[, variables], function(x) all(is.numeric(x)))
  num_vec <- names(var_type[var_type == TRUE])
  cat_vec <- names(var_type[var_type == FALSE])

  ###########################################################
  # Step2: Create Tab 1,
  ###########################################################

  tab_cat1 <- helper_summarize_cat(
    data = data,
    cat_vec = cat_vec,
    new_line = new_line,
    group_var = group_var,
    treatment_arm = treatment_arm,
    measures_cat = measures_cat,
    measure_style = TRUE
  )

  tab_num1 <- helper_summarize_num(
    data = data,
    num_vec = num_vec,
    new_line = new_line,
    group_var = group_var,
    treatment_arm = treatment_arm,
    measures_num = measures_num,
    measure_style = TRUE
  )

  tab1 <- rbind(tab_cat1, tab_num1)


  ###############################################################
  # p-values: this option is only available, if I have a pairwise comparison between groups
  # this means. if chleifen to control: number_arms = 1: no pvalues and SMDs,
  # arms = 2, no problem!, arms >2: case: MULtIPLY, I need an additional function,
  # to tell R, tht I want pairwise comparisons
  #################################################################

  if (display_pvalue & !is.logical(group_var)) {
    pcat <- helper_testing_cat(
      data = data,
      cat_vec = cat_vec,
      group_var = group_var,
      treatment_arm = treatment_arm
    )

    pnum <- helper_testing_num(
      data = data,
      num_vec = num_vec,
      group_var = group_var,
      treatment_arm = treatment_arm
    )

    pval <- rbind(pcat, pnum)
    tab1 <- merge(tab1, pval, by = "name", all.x = TRUE)
  }


  ##################################################################
  # Standardized mean differences
  ##################################################################

  if (display_smd & !is.logical(group_var)) {
    smd_data <- helper_smd (
      data = data,
      variables = variables,
      group_var = group_var,
      treatment_arm = treatment_arm
    )

    smd_data <- smd_data %>%
      rename(name = .data$variable)

    tab1 <- merge(tab1, smd_data, by = "name", all.x = TRUE)
  }

  ##################################################################
  # Missing values
  ###############################################################

  if (display_missings) {
    miss_val <- helper_summarize_missings(
      data = data,
      var_vec = variables,
      measures = measures_cat,
      group_var = group_var,
      treatment_arm = treatment_arm,
      measure_style = TRUE
    )

    tab1 <- rbind.fill(tab1, miss_val)
  }

  #############################################################
  # Step 3: Designing the output table
  ################################################################
    tab1 <- helper_layout(
      tab1 = tab1,
      data = data,
      group_var = group_var,
      new_line = new_line,
      treatment_arm = treatment_arm,
      measures_cat = measures_cat,
      measures_num = measures_num,
      cat_var = cat_vec,
      flextable_output = flextable_output,
      sort_rows = sort_rows,
      add_measure_ident = add_measure_ident)


  tab1
}


