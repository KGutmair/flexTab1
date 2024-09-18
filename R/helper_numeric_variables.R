######################################################################
# Function 2: This function calculated the median, median, standard deviation,
#             1. and 3. quantile,  min, max of numeric variables
######################################################################

# input:
#        data: data.frame
#        num_vec: character vector containing the column names for which the respective
#                 measures should be calculated
#        group_var: variable of the groups, if the characteristics of
#                   different groups should be compared, default: FALSE = no grouping variable
#        new line: logical: TRUE/FALSE. should there be an empty line after every variable (default: FALSE = no empty line)
#        measures_num: character variable, which measures should be displayed in the output table:
#                      options: "mean", "sd", "median", "min", "max", "quantile1", "quantile3"
#        measure_style: boolean: TRUE/FALSE, if TRUE: enhancing the output by putting
#                       together all desired measures (up to three) in one column. Usually, one want
#                       to display mean and SD or median and minimum and maximum. This will be summarized
#                       to a column with "mean (SD)" and "median (Minimum - Maximum)"


#' Helper: Calculates Summary Statistics for Multiple Numeric Variables
#'
#'@description
#'"This helper function calculates summary statistics for numerical variables.
#'One can choose to display measures like the median, mean, standard deviation,
#'first and third quartiles, minimum, maximum, or a combination of these.
#'It works with one or more groups and also supports nested group comparisons for
#'more complex analyses, allowing for the comparison of subgroup statistics within a superior group.
#'
#' @inheritParams Table1_flex
#' @param num_vec  A character vector specifying the names of the variables for which
#'                the summary measures should be created.
#' @param measure_style A logical value (TRUE/FALSE), default is `FALSE`. If `TRUE`, the function `cat_unify_names`
#'        will be called to rename the column to "measures" and combine absolute and relative frequencies into
#'        a single column, provided that more than one summary measure is selected.
#'
#' @return A `data.frame` containing the summary measures (one column for each summary measure)
#'         specified in the input parameters.
#' @importFrom dplyr summarize mutate relocate select rename group_by summarise_at %>%
#' @importFrom data.table dcast setDT as.data.table
#' @importFrom tidyr all_of
#' @importFrom rlang sym
#' @importFrom stats sd quantile median
#'
#' @noRd
#'
helper_summarize_num <- function(data,
                         num_vec,
                         group_var = FALSE,
                         treatment_arm = FALSE,
                         new_line = FALSE,
                         measures_num = c("median", "min", "max"),
                         measure_style = FALSE) {


  tab1_list <- list()
  i <- 1

  # no grouping variable
  if (is.logical(group_var)) {
    # if string for numeric variables is empty, return an empty data.frame
    if (length(num_vec) == 0) {
      row_names <- c("name", measures_num, "variable")
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      for (param in num_vec) {
        param1 <- sym(param)

        data1 <- data %>%
          dplyr::summarize(
            mean = round(mean(!!param1, na.rm = TRUE), 2),
            sd = round(sd(!!param1, na.rm = TRUE), 2),
            quantile1 = round(quantile(!!param1, prob = 0.25, na.rm = TRUE), 2),
            median = round(median(!!param1, na.rm = TRUE), 2),
            quantile3 = round(quantile(!!param1, prob = 0.75, na.rm = TRUE), 2),
            min = (round(range(!!param1, na.rm = TRUE), 2))[1],
            max = (round(range(!!param1, na.rm = TRUE), 2))[2]
          ) %>%
          mutate(name = param) %>%
          relocate(name, .before = mean)

        if (new_line) {
          data1 <- rbind(c(param, rep(NA, times = 7)), data1)
        }

        tab1_list[[i]] <- data1
        i <- i + 1
      }
      res_tab <- do.call("rbind", tab1_list)

      res_tab1 <- res_tab %>%
        select(all_of(c("name", measures_num))) %>%
        mutate(variable = NA)
    }
    # more than one arm = grouping variable not empty
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
    if (length(num_vec) == 0) {
      rwo_names1 <- expand.grid(unique(data1$group), measures_num)
      row_names2 <- sprintf("%s_%s", rwo_names1[, 2], rwo_names1[, 1])
      row_names <- c("name", row_names2, "variable")
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      for (param in num_vec) {
        param1 <- sym(param)

        data2 <- data1 %>%
          group_by(group) %>%
          dplyr::summarize(
            mean = round(mean(!!param1, na.rm = TRUE), 2),
            sd = round(sd(!!param1, na.rm = TRUE), 2),
            quantile1 = round(quantile(!!param1, prob = 0.25, na.rm = TRUE), 2),
            median = round(median(!!param1, na.rm = TRUE), 2),
            quantile3 = round(quantile(!!param1, prob = 0.75, na.rm = TRUE), 2),
            min = (round(range(!!param1, na.rm = TRUE), 2))[1],
            max = (round(range(!!param1, na.rm = TRUE), 2))[2]
          ) %>%
          mutate(name = param) %>%
          relocate(name, .before = group) %>%
          data.frame()

        data3 <-
          as.data.frame(dcast(
            as.data.table(data2),
            name ~ group,
            value.var = c(
              "mean",
              "sd",
              "quantile1",
              "median",
              "quantile3",
              "min",
              "max"
            )
          ))

        if (new_line) {
          data3 <- rbind(c(param, rep(NA, times = ncol(data3) - 1)), data3)
        }
        tab1_list[[i]] <- data3
        i <- i + 1
      }

      res_tab <- do.call("rbind", tab1_list)
      keep_var <- c()
      for (i in seq_along(measures_num)) {
        keep_var <-
          c(keep_var, paste0(measures_num[i], "_", unique(data2$group)))
      }

      res_tab1 <- res_tab %>%
        select(all_of(c("name", keep_var))) %>%
        mutate(variable = NA) %>%
        relocate(variable, .after = name)
    }
  }
  if (measure_style) {
    if (length(measures_num) > 3) {
      cat(
        "This option is only available, if the number of measures per observations does not exceed three. Returning the result without the option measure_style"
      )
    } else {
      res_tab1 <- num_unify_names(
        data = data,
        group_var = group_var,
        treatment_arm = treatment_arm,
        measures_num = measures_num,
        tab_num_measure = res_tab1
      )
    }
  }
  res_tab1
}






#########################################################################
# Helper function for the Tab1 functions to unify the names and style of the
# data.frame in order to merge this with the output of the categorial variables
# (function helper_categorical_summary). This function is called within the
# helper_numeric_summary function
##########################################################################

#' Helper: Unify Column Names and Style of the tab1_categorical Output
#'
#'@description
#' "This function is used within the `helper_summarize_num` function.
#' Its main purpose is to standardize column names and combine multiple summary
#' measures into a single column for cleaner output and easier processing in the
#' `Table1_flex` function.
#'
#'
#' @inheritParams Table1_flex
#' @param tab_num_measure output data.frame of the function `helper_summarize_num`
#'
#' @return A `data.frame` containing the summary measures specified in the input parameters.
#'         If more than one summary measure is chosen, the summary measures are merged into
#'         one column
#' @importFrom dplyr rename mutate select mutate_at %>%
#' @importFrom tidyr all_of
#' @importFrom rlang sym :=
#' @noRd

num_unify_names <- function(data,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            measures_num,
                            tab_num_measure) {
  # 1. Only one group
  if (is.logical(group_var)) {
    # 1. One measure
    if (length(measures_num) == 1) {
      tab_num_measure <- tab_num_measure %>%
        rename(measure = !!sym(measures_num))
    } else if (length(measures_num) == 2) {
      tab_num_measure <- tab_num_measure %>%
        mutate(measure = paste0(!!sym(measures_num[1]), " (", !!sym(measures_num[2]), ")")) %>%
        select(all_of(c("name", "variable", "measure")))
    } else {
      tab_num_measure <- tab_num_measure %>%
        mutate(measure = paste0(
          !!sym(measures_num[1]), " (", !!sym(measures_num[2]),
          "-", !!sym(measures_num[3]), ")"
        )) %>%
        select(all_of(c("name", "variable", "measure")))
    }
    # more than one group
  } else {
    if (!is.logical(treatment_arm)) {
      group_var1 <- sym(group_var)
      treatment_arm1 <- sym(treatment_arm)

      data <- data %>%
        rename(
          group = !!group_var1,
          treatment_arm = !!treatment_arm1
        ) %>%
        mutate(group = paste(treatment_arm, group, sep = " "))
    } else {
      group_var1 <- sym(group_var)
      data <- data %>%
        rename(group = !!group_var1)
    }

    # only one measure
    if (length(measures_num) == 1) {
      names_new <- colnames(tab_num_measure)
      colnames(tab_num_measure) <- sub(".*_", "measure_", names_new)
    } else if (length(measures_num) == 2) {
      var <- paste0("measure_", unique(data$group))
      component1 <- paste0(measures_num[1], "_", unique(data$group))
      component2 <- paste0(measures_num[2], "_", unique(data$group))

      for (i in seq_along(var)) {
        tab_num_measure <- tab_num_measure %>%
          mutate(!!sym(var[i]) := paste0(!!sym(component1[i]), " (", !!sym(component2[i]), ")"))
      }
      tab_num_measure <- tab_num_measure %>%
        select(all_of(c("name", "variable", var)))
    } else {
      var <- paste0("measure_", unique(data$group))
      component1 <- paste0(measures_num[1], "_", unique(data$group))
      component2 <- paste0(measures_num[2], "_", unique(data$group))
      component3 <- paste0(measures_num[3], "_", unique(data$group))

      for (i in seq_along(var)) {
        tab_num_measure <- tab_num_measure %>%
          mutate(!!sym(var[i]) := paste0(
            !!sym(component1[i]), " (", !!sym(component2[i]),
            "-", !!sym(component3[i]), ")"
          ))
      }
      tab_num_measure <- tab_num_measure %>%
        select(all_of(c("name", "variable", var)))
    }
  }
  tidy_rows <- function(x) {
    ifelse(x == "NA (NA)", NA,
           ifelse(x == "NA (NA-NA)", NA, x)
    )
  }
  var <- colnames(tab_num_measure)
  var <- var[!var %in% c("variable")]
  tab_num_measure <- tab_num_measure %>%
    mutate_at(var, tidy_rows)
  tab_num_measure
}


for (i in seq_len(3)) {
  print(i)
}
