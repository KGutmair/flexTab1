########################################################################
# Function 1: absolute and relative frequencies of categorial variables
#######################################################################

#' Helper:Calculate Absolute and Relative Frequencies of Categorical Variables
#'
#'@description
#'This helper function calculates summary statistics for categorical variables.
#'One can choose to display only absolute frequencies, relative frequencies, or both.
#'It works with one or more groups and also supports nested group comparisons for
#'more complex analyses, allowing for the comparison of subgroup statistics within a superior group.
#'
#'
#' @inheritParams Table1_flex
#' @param cat_vec  A character vector specifying the names of the variables for which
#'                the summary measuresshould be created.
#' @param measure_style A logical value (TRUE/FALSE), default is `FALSE`. If `TRUE`, the function `cat_unify_names`
#'        will be called to rename the column to "measures" and combine absolute and relative frequencies into
#'        a single column, provided that more than one summary measure is selected.
#' @return A `data.frame` containing the summary measures (one column for each summary measure)
#'         specified in the input parameters.
#'
#' @importFrom dplyr filter group_by summarize mutate rename relocate select %>% n
#' @importFrom data.table dcast setDT
#' @importFrom tidyr all_of
#' @importFrom rlang sym .data
#'
#'
#' @noRd

# old: tab1_categorical

helper_summarize_cat <- function(data,
                            cat_vec,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            new_line = FALSE,
                            measures_cat = c("absolute", "relative"),
                            measure_style = FALSE) {

  tab1_list <- list()
  i <- 1
  if (is.logical(group_var)) {
    if (length(cat_vec) == 0) {
      all_measure_options <- c("absolute", "relative")
      names_data <- c("n", "relative_freq")
      keep_vars <- names_data[match(measures_cat, all_measure_options)]
      row_names <- c("name", "variable", keep_vars)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      for (param in (cat_vec)) {
        param1 <- sym(param)
        tab1 <- data %>%
          filter(!is.na(!!param1)) %>%
          group_by(!!param1) %>%
          summarise(n = n()) %>%
          mutate(
            relative_freq = round((n / sum(n)) * 100, 0),
            name = param
          )
        colnames(tab1)[1] <- "variable"
        if (new_line) {
          tab1 <- rbind(c(NA, NA, NA, param), tab1)
        }
        tab1_list[[i]] <- tab1
        i <- i + 1
      }
      res_tab <- do.call("rbind", tab1_list)

      # select all the appropriate measure options
      all_measure_options <- c("absolute", "relative")
      names_data <- c("n", "relative_freq")
      keep_vars <-
        names_data[match(measures_cat, all_measure_options)]
      res_tab1 <- res_tab %>%
        relocate(name, .before = variable) %>%
        select(all_of(c("name", "variable", keep_vars)))
    }
  } else {
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
      group_var1 <- sym(group_var)
      data1 <- data %>%
        rename(group = !!group_var1)
    }
    if (length(cat_vec) == 0) {
      all_measure_options <- c("absolute", "relative")
      names_data <- c("n", "relative_freq")
      keep_vars <- names_data[match(measures_cat, all_measure_options)]
      rwo_names1 <- expand.grid(unique(data1$group), keep_vars)
      row_names2 <-
        sprintf("%s_%s", rwo_names1[, 2], rwo_names1[, 1])
      row_names <- c("name", "variable", row_names2)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      tab1_list <- list()
      i <- 1
      for (param in (cat_vec)) {
        param1 <- sym(param)
        tab1 <- data1 %>%
          filter(!is.na(!!param1)) %>%
          group_by(group, !!param1) %>%
          summarise(n = n()) %>%
          group_by(group) %>%
          mutate(
            relative_freq = round((n / sum(n)) * 100, 0),
            name = param
          ) %>%
          data.frame()
        colnames(tab1)[2] <- "variable"
        if (new_line) {
          tab1 <- rbind(c(NA, NA, NA, NA, param), tab1)
        }
        tab1_list[[i]] <-
          as.data.frame(dcast(
            setDT(tab1),
            variable + name ~ group,
            value.var = c("n", "relative_freq")
          ))
        i <- i + 1
      }
      res_tab <- do.call("rbind", tab1_list)
      # select all the appropriate measure options
      all_measure_options <- c("absolute", "relative")
      names_data <- c("n", "relative_freq")
      keep_vars <-
        names_data[match(measures_cat, all_measure_options)]
      keep_var <- c()
      for (i in seq_along(keep_vars)) {
        keep_var <-
          c(keep_var, paste0(keep_vars[i], "_", unique(data1$group)))
      }
      res_tab1 <- res_tab %>%
        relocate(name, .before = variable) %>%
        select(all_of(c("name", "variable", keep_var)))
    }
  }
  if (measure_style) {
    res_tab1 <- cat_unify_names(
      data = data,
      group_var = group_var,
      treatment_arm = treatment_arm,
      measures_cat = measures_cat,
      tab_cat_measure = res_tab1
    )
  }
  res_tab1
}





#########################################################################
# Helper function for the Tab1 functions to unify the names and style of the tab1_categorial
# data.frame in order to merge this with the output of the numeric variables
# (function helper_numeric_summary). This function is called within the helper_categorical_summary function
##########################################################################

#' Helper: Unify Column Names and Style of the tab1_categorical Output
#'
#' @description
#' "This function is used within the `helper_summarize_cat` function.
#' Its main purpose is to standardize column names and combine multiple summary
#' measures into a single column for cleaner output and easier processing in the
#' `Table1_flex` function.
#'
#'
#' inherited from the above function. See docu, how to write inheritances
#' @inheritParams Table1_flex
#' @param tab_cat_measure output data.frame of the function `helper_summarize_cat`
#'
#' @return A `data.frame` containing the summary measures specified in the input parameters.
#'         If more than one summary measure is chosen, the summary measures are merged into
#'         one column
#' @importFrom dplyr mutate select rename mutate_at
#' @importFrom tidyr all_of
#' @importFrom magrittr %>%
#' @importFrom rlang sym :=
#'
#'
#' @noRd

cat_unify_names <- function(data,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            measures_cat,
                            tab_cat_measure) {
  all_measure_options <- c("absolute", "relative")
  names_data <- c("n", "relative_freq")
  keep_vars <- names_data[match(measures_cat, all_measure_options)]

  #1. Only one group
  if (is.logical(group_var)) {
    # 1. One measure
    if (length(measures_cat) == 1) {
      tab_cat_measure <- tab_cat_measure %>%
        rename(measure = !!sym(keep_vars))
    } else {
      # two measures
      tab_cat_measure <- tab_cat_measure %>%
        mutate(measure = paste0(!!sym(keep_vars[1]), " (",!!sym(keep_vars[2]), ")")) %>%
        select(all_of(c("name", "variable", "measure")))
    }
    # 2. more than one group
  } else{
    if (!is.logical(treatment_arm)) {
      group_var1 <- sym(group_var)
      treatment_arm1 <- sym(treatment_arm)

      data <- data %>%
        rename(group = !!group_var1,
               treatment_arm = !!treatment_arm1) %>%
        mutate(group = paste(treatment_arm, .data$group, sep = " "))

    } else {
      group_var1 <- sym(group_var)
      data <- data %>%
        rename(group = !!group_var1)
    }

    # only one measure
    if (length(measures_cat) == 1) {
      names_new <- colnames(tab_cat_measure)
      colnames(tab_cat_measure) <- sub(".*_", "measure_", names_new)
    } else {
      # more than one measure
      var <- paste0("measure_", unique(data$group))
      component1 <- paste0(keep_vars[1], "_", unique(data$group))
      component2 <- paste0(keep_vars[2], "_", unique(data$group))

      for (i in seq_along(var)) {
        tab_cat_measure <- tab_cat_measure %>%
          mutate(!!sym(var[i]) := paste0(!!sym(component1[i]), " (",!!sym(component2[i]), ")"))
      }

      tab_cat_measure <- tab_cat_measure %>%
        select(all_of(c("name", "variable", var)))
    }
  }
  tidy_rows <- function(x) ifelse(x == "NA (NA)", NA, x)
  var <- colnames(tab_cat_measure)
  var <- var[!var %in% c("variable")]
  tab_cat_measure <- tab_cat_measure %>%
    mutate_at(var, tidy_rows)
  tab_cat_measure
}
