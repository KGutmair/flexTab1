#' Helper function: Calculates Summary Statistics for Multiple Numeric Variables
#'
#' @description
#' This function is primarily used internally by `Table1_flex()`.
#' It calculates summary statistics for numerical variables.
#' One can choose to display the median, mean, standard deviation, first and third quartiles, minimum, maximum, or a combination of these.
#' It works with one or more groups and nested group comparisons for the comparison of subgroup statistics within a main group.
#'
#' @inheritParams Table1_flex
#' @param num_vec  A character vector specifying the variable names for which
#'                the summary measures should be created.
#' @param measure_style A logical value (TRUE/FALSE), default is `FALSE`. If `TRUE`, the helper function `cat_unify_names`
#'        will be called to combine the selected summary measures into a single column.
#' @return A `data.frame` containing the summary measures specified in the input parameters.
#' @importFrom stats sd quantile median
#'
#' @keywords internal
#' @export
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   # Load pbc data from the survival package
#'   pbc <- survival::pbc
#'
#'   baseline_var <- c("age", "chol", "platelet")
#'
#'   pbc[c("stage", "trt", "edema", "hepato")] <-
#'     lapply(pbc[c("stage", "trt", "edema", "hepato")], as.factor)
#'
#'   pbc <- pbc[!is.na(pbc$trt), ]
#'
#'   pbc$trt <- ifelse(pbc$trt == "1",
#'     "D-penicillamine",
#'     "Placebo"
#'   )
#'
#'   # Numerical summary measure for one group -------------------------------------------
#'
#'   helper_summarize_num(
#'     data = pbc,
#'     num_vec = c("age", "chol", "platelet"),
#'     group_var = FALSE,
#'     treatment_arm = FALSE,
#'     new_line = FALSE,
#'     measures_num = c("median", "quantile1", "quantile3"),
#'     measure_style = FALSE
#'   )
#'
#'   # Numerical summary measure summarized into one column
#'   helper_summarize_num(
#'     data = pbc,
#'     num_vec = c("age", "chol", "platelet"),
#'     group_var = FALSE,
#'     treatment_arm = FALSE,
#'     new_line = FALSE,
#'     measures_num = c("median", "quantile1", "quantile3"),
#'     measure_style = TRUE
#'   )
#'
#'   # Numerical summary measure for two groups -----------------------------------------
#'
#'   helper_summarize_num(
#'     data = pbc,
#'     num_vec = c("age", "chol", "platelet"),
#'     group_var = "trt",
#'     treatment_arm = FALSE,
#'     new_line = FALSE,
#'     measures_num = c("median", "quantile1", "quantile3"),
#'     measure_style = TRUE
#'   )
#'
#'
#'   # Numerical summary measure for a nested group structure ----------------------------
#'
#'
#'   helper_summarize_num(
#'     data = pbc,
#'     num_vec = c("age", "chol", "platelet"),
#'     group_var = "sex",
#'     treatment_arm = "trt",
#'     new_line = FALSE,
#'     measures_num = c("mean", "sd"),
#'     measure_style = TRUE
#'   )
#' }
#'
helper_summarize_num <- function(data,
                                 num_vec,
                                 group_var = FALSE,
                                 treatment_arm = FALSE,
                                 new_line = FALSE,
                                 measures_num = c("median", "min", "max"),
                                 measure_style = FALSE) {
  tab1_list <- list()
  #--------------------------------------------------
  # summarize function
  #--------------------------------------------------

  safe_num <- function(expr) {
    out <- suppressWarnings(expr)

    if (!is.finite(out)) {
      return(NA)
    }
    out
  }

  summarize_numeric <- function(x) {
    data.frame(
      mean = safe_num(round(mean(x, na.rm = TRUE), 2)),
      sd = safe_num(round(sd(x, na.rm = TRUE), 2)),
      quantile1 = safe_num(unname(round(quantile(x, 0.25, na.rm = TRUE), 2))),
      median = safe_num(round(median(x, na.rm = TRUE), 2)),
      quantile3 = safe_num(unname(round(quantile(x, 0.75, na.rm = TRUE), 2))),
      min = safe_num(round(min(x, na.rm = TRUE), 2)),
      max = safe_num(round(max(x, na.rm = TRUE), 2))
    )
  }


  #---------------------------------------------------------------------------------
  # no grouping variable
  #---------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {
    #----------------------------------------------------------------
    # if there are no numeric variables for which descriptive measures can be
    # computed, return an empty dataframe with the colnames name, variable, measure
    #----------------------------------------------------------------
    if (length(num_vec) == 0) {
      row_names <- c("name", measures_num, "variable")
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]

      #------------------------------------------------------------
      # if there are numeric variables given, calculate for them all descriptive
      # measures
      #-----------------------------------------------------------
    } else {
      tab1_list <- lapply(num_vec, function(param) {
        data1 <- cbind(
          name = param,
          summarize_numeric(data[[param]])
        )

        if (new_line) {
          empty_row <- data.frame(
            matrix(NA, nrow = 1, ncol = ncol(data1))
          )

          names(empty_row) <- names(data1)
          empty_row$name <- param
          data1 <- rbind(empty_row, data1)
        }

        data1
      })

      res_tab <- do.call(rbind, tab1_list)

      # select only the requested measures out of all measures

      res_tab1 <- res_tab[, c("name", measures_num), drop = FALSE]
      res_tab1$variable <- NA
    }

    #---------------------------------------------------------------------------------
    # descriptive measures stratified after one or two grouping variables
    #---------------------------------------------------------------------------------
  } else {
    # Preparation: renaming of column names to group
    # if we have both, treatment arm and grouping variable
    if (!is.logical(treatment_arm) & !is.logical(group_var)) {
      # merge the treatment and group variables into one variable
      data1 <- data
      data1$group <- paste(
        data[[treatment_arm]],
        data[[group_var]],
        sep = " "
      )
    } else if (!is.logical(treatment_arm)) {
      # only grouping variable
      data1 <- data
      data1$group <- data[[treatment_arm]]
    } else {
      data1 <- data
      data1$group <- data[[group_var]]
    }

    #---------------------------------------------------------------------
    # if there are no numeric variables for which descriptive measures can be
    # computed, return an empty dataframe with the colnames name, variable,
    # measure stratified by group
    #----------------------------------------------------------------------
    if (length(num_vec) == 0) {
      rwo_names1 <- expand.grid(unique(data1$group), measures_num)
      row_names2 <- sprintf("%s_%s", rwo_names1[, 2], rwo_names1[, 1])
      row_names <- c("name", row_names2, "variable")
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      #------------------------------------------------------------
      # if there are numeric variables given, calculate for them all descriptive
      # measures
      #-----------------------------------------------------------
      tab1_list <- lapply(num_vec, function(param) {
        # split variable by group
        split_x <- split(data1[[param]], data1$group)
        # calculate summaries per group
        stats <- lapply(split_x, summarize_numeric)
        # convert to one-row wide data.frame
        values <- unlist(stats)

        names(values) <- paste0(
          rep(names(stats[[1]]), times = length(stats)),
          "_",
          rep(names(stats), each = length(stats[[1]]))
        )

        data3 <- data.frame(
          name = param,
          as.list(values),
          check.names = FALSE
        )

        # optional empty line
        if (new_line) {
          empty_row <- as.data.frame(
            as.list(rep(NA, ncol(data3)))
          )
          names(empty_row) <- names(data3)
          empty_row$name <- param
          data3 <- rbind(empty_row, data3)
        }
        rownames(data3) <- NULL
        data3
      })

      res_tab <- do.call(rbind, tab1_list)

      # keep requested measures
      groups <- unique(data1$group)
      keep_var <- as.vector(
        outer(measures_num, groups, paste, sep = "_")
      )

      res_tab1 <- res_tab[, c("name", keep_var), drop = FALSE]
      res_tab1$variable <- NA
      res_tab1 <- res_tab1[
        ,
        c("name", "variable", keep_var)
      ]
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
        tab_num_measure = res_tab1,
        num_vec = num_vec
      )
    }
  } else {
    res_tab1 <- res_tab1$variable <- NULL
  }
  res_tab1
}



#' Helper: Unify Column Names and Style of the tab1_categorical Output
#'
#' @description
#' "This function is used within the `helper_summarize_num` function.
#' It standardizes column names and combine multiple summary
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
#' @noRd

num_unify_names <- function(data,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            measures_num,
                            tab_num_measure,
                            num_vec = num_vec) {
  #--------------------------------------------------------------------------------------
  # 1. Tidy data.frame without stratification/grouping variable
  #------------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {
    #----------------------------------------------------------------
    # no numeric variables given, for which descriptive measures can
    # be calculated: return empty dataframe with name, variable, measure
    #-------------------------------------------------------------
    if (length(num_vec) == 0) {
      tab_num_measure <- data.frame(
        name = character(),
        variable = character(),
        measure = character()
      )
    } else {
      #---------------------------
      # a, One descriptive measure given
      #----------------------------
      if (length(measures_num) == 1) {
        names(tab_num_measure)[names(tab_num_measure) == measures_num] <- "measure"
        tab_num_measure <- tab_num_measure[, c("name", "variable", "measure")]

        #---------------------------
        # a, Two descriptive measure given
        #----------------------------
      } else if (length(measures_num) == 2) {
        tab_num_measure$measure <- paste0(
          tab_num_measure[[measures_num[1]]],
          " (",
          tab_num_measure[[measures_num[2]]],
          ")"
        )

        tab_num_measure <- tab_num_measure[, c("name", "variable", "measure")]

        #---------------------------
        # a,Three descriptive measure given
        #----------------------------
      } else {
        tab_num_measure$measure <- paste0(
          tab_num_measure[[measures_num[1]]],
          " (",
          tab_num_measure[[measures_num[2]]],
          "-",
          tab_num_measure[[measures_num[3]]],
          ")"
        )
        tab_num_measure <- tab_num_measure[, c("name", "variable", "measure")]
      }
    }
    #--------------------------------------------------------------------------------------
    # 1. Tidy data.frame with stratification
    #------------------------------------------------------------------------------------
  } else {
    if (!is.logical(treatment_arm) & !is.logical(group_var)) {
      names(data)[names(data) == group_var] <- "group"
      names(data)[names(data) == treatment_arm] <- "treatment_arm"

      data$group <- paste(
        data$treatment_arm,
        data$group,
        sep = " "
      )
    } else if (!is.logical(treatment_arm)) {
      names(data)[names(data) == treatment_arm] <- "group"
    } else {
      names(data)[names(data) == group_var] <- "group"
    }

    #-------------------------------
    # no numeric variables given: return empty data.frame
    #-----------------------------
    if (length(num_vec) == 0 & length(measures_num) >= 1) {
      add_vars <- c("name", "variable")
      var <- paste0("measure_", unique(data$group))

      tab_num_measure <- data.frame(
        matrix(nrow = 0, ncol = length(c(add_vars, var)))
      )

      names(tab_num_measure) <- c(add_vars, var)
      #------------------------------------
      # numeric variables given, for which descriptive measures
      # had been calculated
      #----------------------------------
    } else {
      #---------------------------
      # a,One descriptive measure given
      #----------------------------
      # only one measure: replace median_1 with measure_1 or min_1 with measure_1
      if (length(measures_num) == 1) {
        names_new <- colnames(tab_num_measure)
        colnames(tab_num_measure) <- sub(".*_", "measure_", names_new)
        #---------------------------
        # a,Two descriptive measures given
        #----------------------------
      } else if (length(measures_num) == 2) {
        var <- paste0("measure_", unique(data$group))
        component1 <- paste0(measures_num[1], "_", unique(data$group))
        component2 <- paste0(measures_num[2], "_", unique(data$group))

        for (i in seq_along(var)) {
          tab_num_measure[[var[i]]] <- paste0(
            tab_num_measure[[component1[i]]], " (",
            tab_num_measure[[component2[i]]], ")"
          )
        }

        tab_num_measure <- tab_num_measure[, c("name", "variable", var)]

        #---------------------------
        # a,Three descriptive measure given
        #----------------------------
      } else {
        var <- paste0("measure_", unique(data$group))
        component1 <- paste0(measures_num[1], "_", unique(data$group))
        component2 <- paste0(measures_num[2], "_", unique(data$group))
        component3 <- paste0(measures_num[3], "_", unique(data$group))

        for (i in seq_along(var)) {
          tab_num_measure[[var[i]]] <- paste0(
            tab_num_measure[[component1[i]]], " (",
            tab_num_measure[[component2[i]]], "-",
            tab_num_measure[[component3[i]]], ")"
          )
        }

        tab_num_measure <- tab_num_measure[, c("name", "variable", var)]
      }
    }
  }
  tidy_rows <- function(x) {
    ifelse(x == "NA (NA)", NA,
      ifelse(x == "NA (NA-NA)", NA, x)
    )
  }
  var <- colnames(tab_num_measure)
  var <- var[!var %in% c("variable")]
  tab_num_measure[var] <- lapply(
    tab_num_measure[var],
    tidy_rows
  )
  tab_num_measure
}
