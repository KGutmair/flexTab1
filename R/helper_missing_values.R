################################################################
# This function calculates the absolute and relative frequency of
# missing values in multiple variables
################################################################

#' Helper: Calculate Absolute and Relative Frequency of Missing Values
#'
#' @description
#' #'This helper function calculates summary statistics for missing values in variables.
#'One can choose to display only absolute frequencies, relative frequencies, or both.
#'It works with one or more groups and also supports nested group comparisons for
#'more complex analyses, allowing for the comparison of missing values between subgroups
#'within an superior group.
#'
#' @inheritParams Table1_flex
#' @param var_vec A character vector specifying the names of the variables for which the Table 1 summary should be created.
#' @param measures A character vector indicating how categorical variables should be summarized. Options are
#'        `"absolute"`, `"relative"`, or both. Default is `c("absolute", "relative")` for displaying both absolute and
#'        relative frequencies.
#' @param measure_style A logical value (TRUE/FALSE), default is `FALSE`. If `TRUE`, the function `cat_unify_names`
#'        will be called to rename the column to "measures" and combine absolute and relative frequencies into
#'        a single column, provided that more than one summary measure is selected.
#'
#' @return A `data.frame` containing the summary measures (one column for each summary measure)
#'          for missing values specified in the input parameters.
#' @noRd
#' @importFrom dplyr summarize_at select_if mutate relocate select rename group_by slice
#' @importFrom magrittr set_colnames
#' @importFrom tidyr all_of
#'
helper_summarize_missings <- function(data,
                          var_vec,
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE) {
  tab1_list <- list()
  i <- 1

  # Only one group
  if (is.logical(group_var)) {
    # absolute frequencies
    tab1 <- t(data %>%
                summarise_at(var_vec, list(~ sum(is.na(
                  .
                )))) %>%
                select_if(~ !all(is.na(.) | . == 0))) %>%
      data.frame()

    tab1_1 <- tab1 %>%
      set_colnames("n") %>%
      mutate(variable = rownames(tab1))

    # relative frequencies
    tab2 <- t(data %>%
                summarise_at(var_vec, list(~ round((sum(is.na(.)) / length(.) *
                                                      100), 0))) %>%
                select_if(~ !all(is.na(.) | . == 0))) %>%
      data.frame()

    tab2_2 <- tab2 %>%
      set_colnames("relative_freq") %>%
      mutate(variable = rownames(tab2))

    tab3 <- tab1_1
    tab3$measure <- paste0(tab1_1$n, "(", tab2_2$relative_freq, ")")
    tab3 <- tab3[, -1]
    tab4 <-
      Reduce(
        function(x, y) {
          merge(x, y, by = "variable")
        },
        list(tab1_1, tab2_2, tab3)
      )

    # select all the appropriate measure options
    all_measure_options <- c("absolute", "relative")
    names_data <- c("n", "relative_freq")
    keep_vars <- names_data[match(measures, all_measure_options)]

    res_tab <- tab4 %>%
      relocate(variable) %>%
      select(all_of(c("variable", keep_vars)))
    # More than one group
  } else {
    # nested structure: multiple treatment arms and within these treatment arms
    # at least two groups
    if (!is.logical(treatment_arm)) {
      group_var1 <- sym(group_var)
      treatment_arm1 <- sym(treatment_arm)
      data1 <- data %>%
        rename(
          group = !!group_var1,
          treatment_arm = !!treatment_arm1
        ) %>%
        mutate(group = paste(treatment_arm, group, sep = " "))
    } else {
      # only groups, no treatment arms
      group_var1 <- sym(group_var)
      data1 <- data %>%
        rename(group = !!group_var1)
    }

    # absolute frequencies
    tab1 <- t(data1 %>%
                group_by(group) %>%
                summarise_at(var_vec, list(~ sum(is.na(
                  .
                )))) %>%
                select_if(~ !all(is.na(.) | . == 0))) %>%
      data.frame()

    tab1_1 <- tab1 %>%
      set_colnames(paste0("n_", tab1[1, ])) %>%
      slice(-1) %>%
      mutate(variable = rownames(tab1)[-1])

    # relative frequencies
    tab2 <- t(data1 %>%
                group_by(group) %>%
                summarise_at(var_vec, list(~ round((sum(is.na(.)) / length(.) *
                                                      100), 0))) %>%
                select_if(~ !all(is.na(.) | . == 0))) %>%
      data.frame()

    tab2_2 <- tab2 %>%
      set_colnames(paste0("relative_freq_", tab2[1, ])) %>%
      slice(-1) %>%
      mutate(variable = rownames(tab1)[-1])

    tab3 <- tab1_1
    for (i in seq_len(ncol(tab1_1) - 1)) {
      tab3[, i] <- paste0(tab1_1[, i], "(", tab2_2[, i], ")")
    }
    colnames(tab3) <- c(paste0("measure_", tab1[1, ]), "variable")
    tab4 <-
      Reduce(
        function(x, y) {
          merge(x, y, by = "variable")
        },
        list(tab1_1, tab2_2, tab3)
      )

    # select all the appropriate measure options
    all_measure_options <- c("absolute", "relative")
    names_data <- c("n", "relative_freq")
    keep_vars <- names_data[match(measures, all_measure_options)]

    keep_var <- c()
    for (i in seq_along(keep_vars)) {
      keep_var <-
        c(keep_var, paste0(keep_vars[i], "_", unique(data1$group)))
    }

    res_tab <- tab4 %>%
      relocate(variable) %>%
      select(all_of(c("variable", keep_var)))
  }
  res_tab <- res_tab %>%
    rename(name = variable) %>%
    mutate(variable = "missing") %>%
    relocate(variable, .after = name)

  if (measure_style) {
    res_tab <- cat_unify_names(
      data = data,
      group_var = group_var,
      treatment_arm = treatment_arm,
      measures_cat = measures,
      tab_cat_measure = res_tab
    )
  }
  res_tab
}
