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
  all_measure_options <- c("absolute", "relative")
  names_data <- c("n", "relative_freq")

  #------------------------------------------------------
  # Helper function for summarizing categorical variables
  #------------------------------------------------------
  summarize_categorical <- function(x) {
    # remove missings
    x <- x[!is.na(x)]
    # all values missing of that variable
    if (length(x) == 0) {
      return(
        data.frame(
          variable = NA,
          n = NA,
          relative_freq = NA,
          stringsAsFactors = FALSE
        )
      )
    }
    # frequencies
    n <- table(x)
    # output
    data.frame(
      variable = names(n),
      n = as.numeric(n),
      relative_freq = round(
        as.numeric(n) / sum(n) * 100,
        0
      ),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }

  #---------------------------------------------------------------------------------
  # Only one group
  #---------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {
    #--------------------------------------------------
    # no categorical variables: return empty data.frame with name, variable, measure
    #-------------------------------------------------
    if (length(cat_vec) == 0) {
      keep_vars <- names_data[match(measures_cat, all_measure_options)]
      row_names <- c("name", "variable", keep_vars)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
    } else {
      #------------------------------------------------
      # categorical variables for which measures are calculated
      #-----------------------------------------------
      tab1_list <- lapply(cat_vec, function(param) {

        tab1 <- summarize_categorical(data[[param]])
        tab1$name <- param
        tab1 <- tab1[ ,c("name", "variable", "n", "relative_freq")]

        if (new_line) {
        empty_row <- data.frame(
          name = param,
          variable = NA,
          n = NA,
          relative_freq = NA)


        tab1 <- rbind(empty_row, tab1)
        }
        tab1
      })

      res_tab <- do.call("rbind", tab1_list)

      # select all the appropriate measure options
      keep_vars <-
        names_data[match(measures_cat, all_measure_options)]
      res_tab1 <- res_tab %>%
        relocate(name, .before = variable) %>%
        select(all_of(c("name", "variable", keep_vars)))

    }
  } else {
    #---------------------------------------------------------------------------------
    # grouping variable: at least two groups
    #--------------------------------------------------------------------------------
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

    #--------------------------------------------------------------------
    # no categorical variables present: return empty data.frame
    #------------------------------------------------------------------
    if (length(cat_vec) == 0) {
      keep_vars <- names_data[match(measures_cat, all_measure_options)]
      rwo_names1 <- expand.grid(unique(data1$group), keep_vars)
      row_names2 <-
        sprintf("%s_%s", rwo_names1[, 2], rwo_names1[, 1])
      row_names <- c("name", "variable", row_names2)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab1 <- res_tab1[-1, ]
      #---------------------------------------------------------
      # categorical variables present
      #--------------------------------------------------------
    } else {
      tab1_list <- lapply(cat_vec, function(param) {
        # split variable by group
        split_x <- split(
          data1[[param]],
          data1$group
        )

        # summarize within each group
        stats <- lapply(
          split_x,
          summarize_categorical)

        # all occurring categories
        all_levels <- unique(
          unlist(
            lapply(stats, function(x) x$variable)
          )
        )

        # initialize output
        out <- data.frame(
          variable = all_levels,
          name = param,
          stringsAsFactors = FALSE
        )

        # add group summaries
        for (grp in names(stats)) {

          tmp <- stats[[grp]]

          idx <- match(
            out$variable,
            tmp$variable
          )

          out[[paste0("n_", grp)]] <-
            tmp$n[idx]

          out[[paste0("relative_freq_", grp)]] <-
            tmp$relative_freq[idx]
        }

        # optional empty row
        if (new_line) {

        empty_row <- as.data.frame(
          as.list(rep(NA, ncol(out)))
        )

        names(empty_row) <- names(out)

        empty_row$name <- param

        out <- rbind(empty_row, out)
        }

        rownames(out) <- NULL

        # if the variable in one group strata is zero, then I want to remove the NA row
        remove_row <- (
          nrow(out) > 1 &&
            any(is.na(out$variable)) &&
            any(
              apply(out, 1, function(x)
                all(is.na(x[names(x) != "name"]))
              )
            )
        )

        if (remove_row) {

          keep <- !apply(
            out,
            1,
            function(x)
              is.na(x["variable"]) &&
              all(is.na(x[names(x) != "name"]))
          )

          out <- out[keep, , drop = FALSE]
        }

        out
      })

      # combine all variables
      res_tab <- do.call(rbind, tab1_list)

      # select all the appropriate measure options
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
      tab_cat_measure = res_tab1,
      cat_vec = cat_vec
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
#'
#'
#' @noRd

cat_unify_names <- function(data,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            measures_cat,
                            tab_cat_measure,
                            cat_vec = cat_vec) {

   all_measure_options <- c("absolute", "relative")
  names_data <- c("n", "relative_freq")
  keep_vars <- names_data[match(measures_cat, all_measure_options)]

  #---------------------------------------------------------------------------------
  #1. Only one group
  #---------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {

    #--------------------------------
    # no categorical variables given
    #-------------------------------
    if(length(cat_vec) == 0) {
      return(data.frame(
        name = character(),
        variable = character(),
        measure = character()
      ))
    }

    #-------------------------------------
    # 1. One measure
    #-------------------------------------
    if (length(measures_cat) == 1) {
      names(tab_cat_measure)[
        names(tab_cat_measure) == keep_vars
      ] <- "measure"
    } else {
      # two measures
      tab_cat_measure$measure <- paste0(
        tab_cat_measure[[keep_vars[1]]],
        " (",
        tab_cat_measure[[keep_vars[2]]],
        ")")

      tab_cat_measure <- tab_cat_measure[ ,c("name", "variable", "measure"), drop = FALSE]

    }
  } else{
    #------------------------------------------------------------------------------
    # 2. At least two groups
    #------------------------------------------------------------------------------
    if (!is.logical(treatment_arm) & !is.logical(group_var)) {
      names(data)[names(data) == group_var] <- "group"
      names(data)[names(data) == treatment_arm] <- "treatment_arm"

      data$group <- paste(
        data$treatment_arm,
        data$group,
        sep = " "
      )

    } else if (!is.logical(treatment_arm)){
      names(data)[names(data) == treatment_arm] <- "group"
    } else {
      names(data)[names(data) == group_var] <- "group"
    }

    #-------------------------------
    # no categorial variables given: return empty data.frame
    #-----------------------------
    if(length(cat_vec) == 0 & length(measures_cat) >= 1) {
      add_vars <- c("name", "variable")
      var <- paste0("measure_", unique(data$group))

      tab_cat_measure <- data.frame(
        matrix(nrow = 0, ncol = length(c(add_vars, var)))
      )

      names(tab_cat_measure) <- c(add_vars, var)
      return(tab_cat_measure)
    }
    #----------------------------------------
    # categorial variables given
    #-------------------------------------
    # only one measure
    if (length(measures_cat) == 1) {
      names_new <- colnames(tab_cat_measure)
      colnames(tab_cat_measure) <- sub(".*_", "measure_", names_new)
    } else {
      # more than one measure
      groups <- unique(data$group)
      var <- paste0("measure_", groups)

      component1 <- paste0(keep_vars[1], "_", groups)
      component2 <- paste0(keep_vars[2], "_", groups)

      for (i in seq_along(var)) {
        tab_cat_measure[[var[i]]] <- paste0(
          tab_cat_measure[[component1[i]]],
          " (",
          tab_cat_measure[[component2[i]]],
          ")" ) }

      tab_cat_measure <- tab_cat_measure[ ,c("name", "variable", var), drop = FALSE]
    }
  }
  tidy_rows <- function(x) ifelse(x == "NA (NA)", NA, x)
  var <- colnames(tab_cat_measure)
  var <- var[!var %in% c("variable")]
  tab_cat_measure <- tab_cat_measure %>%
    mutate_at(var, tidy_rows)
  tab_cat_measure
}
