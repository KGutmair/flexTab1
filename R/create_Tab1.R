#' Generate Table 1 with Customizable Summary Measures
#'
#' @description
#' This function creates a highly flexible Table 1 for descriptive statistics.
#' It allows users to customize the output, choose summary measures, display missing values,
#' p-values (testing for difference between two groups),and standardized mean differences (SMD).
#'
#'
#' @param data A `data.frame` containing the data for which the Table 1 should be generated
#' @param variables A character vector specifying  the variable names for which the Table 1 summary should be created.
#' @param group_var A character string specifying the grouping variable for stratification.
#'        If set to `FALSE` (default), the Table 1 is not stratified by groups.
#' @param treatment_arm A character string indicating the main grouping variable. Subgroups defined by group_var are nested within each level of this variable.
#'        For example, in a dataset with three treatment arms ("treatment": A, B, C) and two subgroups within each
#'        treatment category ("sub_group": I, II), set `group_var` to "sub_group" and `treatment_arm` to "treatment".
#'        If p-values or SMD options are selected, the subgroups within the treatment groups are compared.
#'        Default is `FALSE` if no treatment arms are used.
#' @param new_line logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, an empty row is added after every variable
#'        in the table, which can improve clarity, especially when returning the Table 1 as a flextable object.
#' @param measures_cat A character vector indicating how categorical variables should be summarized. Options are
#'        `"absolute"`, `"relative"`, or both. Default is `c("absolute", "relative")` for displaying both absolute and
#'        relative frequencies.
#' @param measures_num A character vector defining the summary statistics for numeric variables. Options include `"median"`,
#'        `"min"`, `"max"`, `"quantile1"` (25th percentile), `"quantile3"` (75th percentile), `"mean"`, and `"sd"`
#'        (standard deviation).  The number of measures is limited to three (e.g., mean(sd), median(min-max), or
#'        median(quantile1-quantile3)).
#' @param display_pvalue logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, p-values are displayed.
#' @param display_smd logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, standardized mean differences (SMD)
#'        is displayed.
#' @param display_missings A logical (TRUE/FALSE), default is `FALSE`. If `TRUE`, the frequency of missing values for
#'        each variable is shown in the output.
#' @param flextable_output A logical (TRUE/FALSE), default is `TRUE`. If `TRUE`, the Table 1 is returned as a
#'        `flextable` object. If `FALSE`, it is returned as a `data.frame`.
#' @param sort_rows A character vector specifying the row order in which the variables should appear in the output table.
#'        The default is NULL, which sorts the variables alphabetically.
#' @param add_measure_ident A logical (TRUE/FALSE), default is `TRUE`. If TRUE, the summary measures identifiers, e.g. median (min-max),
#'       mean (sd), n (%) is added to the Table 1.
#' @param treatment_order character vector containing the categories of the `treatment_arm` parameter in the desired column order in the output table.
#' @param group_order character vector containing the categories of the `group_var` parameter in the desired column order in the output table.
#'
#' @return A `data.frame` or `flextable` object, depending on the selected output option.
#'         The returned Table 1 contains the summary measures specified in the input parameters,
#'         including any chosen statistics, p-values, standardized mean differences (SMD), and missing value frequencies.
#'
#'@details
#'Testing for differences in variables between groups.
#' For numerical variables, a Wilcoxon rank-sum test is used to assess differences between the median of  two groups at a significance level of 5\%.
#' For categorical variables, a Chi-square test is used to assess differences between groups at a significance level of 5\%. If the expected
#' absolute count is equal or below 5, then the Fisher's exact test is used instead.
#' Till now, only group comparisons between two groups is implemented.
#'
#' @export
#' @import checkmate
#' @import magrittr
#' @importFrom rlang .data
#' @references Austin, P. C. (2011). An Introduction to Propensity Score Methods for Reducing the Effects of Confounding in Observational Studies. Multivariate Behavioral Research, 46(3), 399–424. https://doi.org/10.1080/00273171.2011.568786
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   # Load pbc data from the survival package
#'   pbc <- survival::pbc
#'
#'   baseline_var <- c("age", "chol", "sex", "stage", "platelet")
#'
#'   pbc[c("stage", "trt", "edema", "hepato")] <-
#'     lapply(pbc[c("stage", "trt", "edema", "hepato")], as.factor)
#'
#'   pbc <- pbc[!is.na(pbc$trt), ]
#' pbc$trt <- ifelse(pbc$trt == "1",
#' "D-penicillamine",
#' "Placebo")
#'
#' # Table 1 for one group -------------------------------------------
#'
#' tab1_ex <- Table1_flex(
#' data = pbc,
#' variables = baseline_var
#' )
#' flextable::autofit(tab1_ex)
#'
#' # choosing other summary measures
#' Table1_flex(
#' data = pbc,
#' variables = baseline_var,
#' measures_num = c("mean", "sd"),
#' measures_cat = "relative"
#' )
#'
#' # Returning a data.frame for further processing
#' # and other summary measures
#' Table1_flex(
#' data = pbc,
#' variables = baseline_var,
#' measures_num = c("mean", "sd"),
#' measures_cat = c("absolute","relative"),
#' flextable_output = FALSE
#' )
#'
#' # Table 1 for two groups -----------------------------------------
#'
#' tab1_ex <- Table1_flex(
#' data = pbc,
#' variables = baseline_var,
#' group_var = "trt",
#' add_measure_ident = FALSE,
#' new_line = TRUE,
#' sort_rows = c("age", "sex", "stage", "chol", "platelet"),
#' group_order = c("Placebo", "D-penicillamine")
#' )
#'
#' flextable::autofit(tab1_ex)
#'
#' # with additionally p-values and standarized mean differences
#' tab1_ex <- Table1_flex(
#' data = pbc,
#' variables = baseline_var,
#' group_var = "trt",
#' add_measure_ident = FALSE,
#' new_line = TRUE,
#' sort_rows = c("age", "sex", "stage", "chol", "platelet"),
#' group_order = c("Placebo", "D-penicillamine"),
#' display_pvalue = TRUE,
#' display_smd = TRUE
#' )
#'
#' flextable::autofit(tab1_ex)
#'
#'
#' # Table 1 for a nested group structure ----------------------------
#'
#' baseline_var <- c("age", "chol", "platelet", "stage")
#' tab1_ex <- Table1_flex(
#' data = pbc,
#' variables = baseline_var,
#' group_var = "sex",
#' treatment_arm = "trt",
#' add_measure_ident = TRUE,
#' sort_rows = c("age", "stage", "chol", "platelet"),
#' flextable_output = TRUE,
#' display_pvalue = TRUE
#' )
#'
#' flextable::autofit(tab1_ex)
#' }
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
                        add_measure_ident = TRUE,
                        treatment_order = NULL,
                        group_order = NULL) {
  ###############################
  # Testing input parameters
  ###############################

  #------------------------------
  # data: only allow a data.frame
  #------------------------------
  assert_data_frame(data, min.rows = 1, min.cols = 1)

  #-------------------------------
  # variables
  #-------------------------------
  missing_vars <- variables[!variables %in% colnames(data)]
  assert_character(variables, any.missing = FALSE, null.ok = TRUE)

  if (length(missing_vars) > 0) {
    warning(
      paste0(
        "The following variable(s) cannot be found in the data and will be ignored: ",
        paste(missing_vars, collapse = ", ")
      )
    )
    variables <- variables[!variables %in% missing_vars]
  }


  #-----------------------
  # group_var
  #----------------------
  assert(isFALSE(group_var) || (is.character(group_var) && length(group_var) == 1),
    msg = "Input of 'group_var' must be either FALSE or a single character value (column name of data)."
  )

  if (group_var != FALSE) {
    if (any(!group_var %in% colnames(data))) {
      stop(paste0("Grouping variable ", group_var, " is not in data frame"))
    }
    if (length(unique(data[[group_var]])) <= 1) {
      stop("The grouping var has less then 2 categories")
    }
    # alowed data type of group_var
    assert(
      is.logical(data[[group_var]]) ||
        is.factor(data[[group_var]]) ||
        is.character(data[[group_var]]),
      msg = "'group_var' must refer to a logical or factor/character variable in data."
    )
  }


  #---------------------
  # treatment_arm
  #---------------------
  assert(isFALSE(treatment_arm) || (is.character(treatment_arm) && length(treatment_arm) == 1),
    msg = "Input of 'treatment_arm' must be either FALSE or a single character value (column name of data)."
  )

  if (treatment_arm != FALSE) {
    if (any(!treatment_arm %in% colnames(data))) {
      stop(paste0("Treatment_arm variable ", treatment_arm, " is not in data frame"))
    }
    if (length(unique(data[[treatment_arm]])) <= 1) {
      stop("The treatment_variable has less then 2 categories")
    }
    assert(
      is.logical(treatment_arm) ||
        is.factor(data[[treatment_arm]]) ||
        is.character(data[[treatment_arm]]),
      msg = "'treatment_arm'  must refer to a logical or factor/character variable in data."
    )
  }

  #------------------
  # new_line
  #------------------
  # assertLogical(new_line, any.missing = FALSE, len = 1)
  if (!is.logical(new_line) || length(new_line) != 1 || is.na(new_line)) {
    warning("`new_line` is invalid. Setting to FALSE.")
    new_line <- FALSE
  }

  #-----------------
  # measure_cat
  #--------------
  assert_character(
    measures_cat,
    any.missing = FALSE,
    max.len = 2,
    unique = TRUE
  )
  assertSubset(measures_cat, choices = c("absolute", "relative"))


  #----------------
  # measures_num
  #---------------
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

  #----------------------------
  # display_pvalue and SMD
  #----------------------------
  assertLogical(display_pvalue, any.missing = FALSE, len = 1)
  assertLogical(display_smd, any.missing = FALSE, len = 1)

  if ((is.logical(group_var) & is.logical(treatment_arm)) &
    (display_pvalue == TRUE | display_smd == TRUE)) {
    warning("Calculation of p-values and SMDs need at least two groups. Only one group provided, setting display_pvalues and display_smd to FALSE")
    display_pvalue <- FALSE
    display_smd <- FALSE
  }

  if ((!is.logical(group_var) && (length(unique(data[, group_var])) > 2)) |
    (!is.logical(treatment_arm) & is.logical(group_var)) && (length(unique(data[, treatment_arm])) > 2) &
      (display_pvalue == TRUE | display_smd == TRUE)) {
    warning("The method for comparing more than two groups is not implemented yet.")
    display_pvalue <- FALSE
    display_smd <- FALSE
  }


  #---------------
  # display_missings
  #---------------
  # assertLogical(display_missings, any.missing = FALSE, len = 1)
  ok <- checkmate::testLogical(display_missings, any.missing = FALSE, len = 1)

  if (!ok) {
    warning("`display_missings` is invalid. Setting to TRUE.")
    display_missings <- TRUE
  }

  #---------------
  # flextable_output
  #---------------
  # assertLogical(flextable_output, any.missing = FALSE, len = 1)
  ok <- testLogical(flextable_output, any.missing = FALSE, len = 1)


  if (!ok) {
    warning("'flextable_output' is invalid. Setting to TRUE.")
    flextable_output <- TRUE
  }

  #-----------------
  # sort_rows
  #----------------
  # assert(
  #   is.null(sort_rows) ||
  #     (is.character(sort_rows) && !any(is.na(sort_rows))),
  #   msg = "'sort_rows' must be either NULL or a character vector without missing values."
  # )

  if (!is.null(sort_rows) &&
    (!is.character(sort_rows) || anyNA(sort_rows))) {
    warning("'sort_rows' is invalid. Setting to NULL.")
    sort_rows <- NULL
  }

  if (!is.null(sort_rows)) {
    missing_sort_rows <- sort_rows[!sort_rows %in% variables]

    if (length(missing_sort_rows) > 0) {
      warning(
        paste0(
          "'sort_rows' is ignored, because the following variable name(s) provided in the `sort_rows` argument ",
          "are not included in the `variables` argument: ",
          paste(missing_sort_rows, collapse = ", ")
        )
      )
      sort_rows <- NULL
    }
  }


  #--------------
  # add_measure_ident
  #--------------
  ok <- testLogical(add_measure_ident, any.missing = FALSE, len = 1)

  if (!ok) {
    warning("'add_measure_ident' is invalid. Setting to TRUE.")
    add_measure_ident <- TRUE
  }
  #--------------
  #  treatment_order
  #--------------

  # assert(
  #   is.null(treatment_order) ||
  #     (is.character(treatment_order) && !any(is.na(treatment_order))),
  #   msg = "'treatment_order' must be either NULL or a character vector without missing values."
  # )

  if (!is.null(treatment_order)) {
    if (!is.character(treatment_order) || anyNA(treatment_order)) {
      warning("'treatment_order' input is invalid. Setting to NULL.")
      treatment_order <- NULL
    }
  }

  if (!is.null(treatment_order) & is.logical(treatment_arm)) {
    warning("'treatment_order' is specified, but the parameters treatment_arm is empty.
            'treatment_order' is ignored.")
    treatment_order <- NULL
  }

  if (!is.null(treatment_order) & !is.logical(treatment_arm)) {
    missing_treatment <- treatment_order[
      !treatment_order %in% unique(data[[treatment_arm]])
    ]

    if (length(missing_treatment) > 0) {
      warning(
        paste0(
          "'Treatment_order' is ignored, because the following element(s) in `treatment_order` are not part of the ",
          "categories in `treatment_arm`: ",
          paste(missing_treatment, collapse = ", ")
        )
      )
      treatment_order <- NULL
    }
  }


  #--------------
  #  group_order
  #--------------

  # assert(
  #   is.null(group_order) ||
  #     (is.character(group_order) && !any(is.na(group_order))),
  #   msg = "group_order' must be either NULL or a character vector without missing values."
  # )

  if (!is.null(group_order)) {
    if (!is.character(group_order) || anyNA(group_order)) {
      warning("'group_order' is invalid. Setting to NULL.")
      treatment_order <- NULL
    }
  }

  if (!is.null(group_order) & is.logical(group_var)) {
    warning("'group_order' is specified, but the parameters group_var is empty.
            'group_order' is ignored.")
    group_order <- NULL
  }

  if (!is.null(group_order) & !is.logical(group_var)) {
    missing_group <- group_order[
      !group_order %in% unique(data[[group_var]])
    ]

    if (length(missing_group) > 0) {
      warning(
        paste0(
          "'Group order' is being ignored, because the following element(s) in `group_order` are not part of the ",
          "categories in `group_var`: ",
          paste(missing_group, collapse = ", ")
        )
      )
      group_order <- NULL
    }
  }


  if (!is.logical(treatment_arm) & !is.logical(group_var)) {
    if (xor(!is.null(treatment_order), !is.null(group_order))) {
      warning("There is a nested grouping variable (group_var, treatment_arm),
           but ordering is only definied for one of these two grouping variables. 'treatment_order' and 'group_order are ignored'")
      treatment_order <- NULL
      group_order <- NULL
    }
  }


  #--------------------------------------------------------------------------------
  # Creating now the Table1
  #--------------------------------------------------------------------------------

  #------------------------------------------------------
  # Step 1: Check, if and which variables are numeric and which are categorical
  #------------------------------------------------------
  num_vec <- c()
  cat_vec <- c()
  var_type <- sapply(data[, variables, drop = FALSE], function(x) all(is.numeric(x)))
  num_vec <- names(var_type[var_type == TRUE])
  cat_vec <- names(var_type[var_type == FALSE])

  #----------------------------------------------------
  # Step2: Create Tab 1,
  #----------------------------------------------------

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


  #----------------------------------------------------------------
  # p-values:
  #---------------------------------------------------------------

  if (display_pvalue) {
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


  #---------------------------------------------------------------
  # Standardized mean differences
  #---------------------------------------------------------------

  if (display_smd) {
    smd_data <- helper_smd(
      data = data,
      variables = variables,
      group_var = group_var,
      treatment_arm = treatment_arm
    )

    names(smd_data)[names(smd_data) == "variable"] <- "name"

    tab1 <- merge(tab1, smd_data, by = "name", all.x = TRUE)
  }

  #--------------------------------------------------------------
  # Missing values
  #--------------------------------------------------------------

  if (display_missings) {
    miss_val <- helper_summarize_missings(
      data = data,
      var_vec = variables,
      measures = measures_cat,
      group_var = group_var,
      treatment_arm = treatment_arm,
      measure_style = TRUE
    )

   # tab1 <- rbind.fill(tab1, miss_val)
    # rbind and fill with NA
    all_cols <- union(names(tab1), names(miss_val))

    tab1[setdiff(all_cols, names(tab1))] <- NA
    miss_val[setdiff(all_cols, names(miss_val))] <- NA

    tab1 <- rbind(tab1[all_cols], miss_val[all_cols])
  }

  #---------------------------------------------------------------
  # Step 3: Designing the output table
  #---------------------------------------------------------------
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
    add_measure_ident = add_measure_ident,
    treatment_order = treatment_order,
    group_order = group_order
  )


  tab1
}
