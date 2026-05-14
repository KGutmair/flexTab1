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

helper_summarize_missings <- function(data,
                          var_vec,
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE) {
  tab1_list <- list()
  i <- 1
  all_measure_options <- c("absolute", "relative")
  names_data <- c("n", "relative_freq")


  missing_summary <- function(data, var_vec) {
    # absolute frequencies
    # Count missings for selected variables
    na_counts <- sapply(data[var_vec], function(x) sum(is.na(x)))

    # Keep only variables with at least one missing value
    na_counts <- na_counts[!(is.na(na_counts) | na_counts == 0)]

    # Create output data frame
    tab1_1 <- data.frame(
      n = as.numeric(na_counts),
      variable = names(na_counts),
      row.names = NULL
    )

    # relative frequencies
    rel_missing <- sapply(
      data[var_vec],
      function(x) round(sum(is.na(x)) / length(x) * 100, 0)
    )

    # Keep only variables with at least one missing value
    rel_missing <- rel_missing[!(is.na(rel_missing) | rel_missing == 0)]

    # Create output data frame
    tab2_2 <- data.frame(
      relative_freq = as.numeric(rel_missing),
      variable = names(rel_missing),
      row.names = NULL
    )

    # transform 0 to < 1% in the relative freqencies
    tab2_2$relative_freq[tab2_2$relative_freq == 0] <- "< 1"

    # create a new varaible n(%)
    tab3 <- merge(tab1_1, tab2_2, by = "variable")
    tab3$measure <- paste0(tab3$n, "(", tab3$relative_freq, ")")
    tab3
  }


  #--------------------------------------------------------------------------------
  # Only one group
  #-------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {

    #----------------------------------------------------
    # no  variables given or no NAs in all variables
    #---------------------------------------------------
    if(length(var_vec) == 0 | any(is.na(data[var_vec])) == FALSE) {

      keep_vars <- names_data[match(measures, all_measure_options)]
      row_names <- c("name", "variable", keep_vars)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab <- res_tab1[-1, ]

    } else {

    # calculate aboslute and relative frewuencies of missing variables
    tab3 <- missing_summary(data = data, var_vec = var_vec)

    # select all the appropriate measure options
    all_measure_options <- c("absolute", "relative")
    names_data <- c("n", "relative_freq")
    names(tab3)[names(tab3) == "variable"] <- "name"
    tab3$variable <- "missing"
    keep_vars <- names_data[match(measures, all_measure_options)]

    res_tab <- tab3[, c("name","variable", keep_vars), drop = FALSE]
    }

    #-----------------------------------------------------------------------------------
    # More than one group
    #-----------------------------------------------------------------------------------
  } else {
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

    #--------------------------------------------------------------------
    # no variables present or no NAs in any variable: return empty data.frame
    #------------------------------------------------------------------
    if (length(var_vec) == 0 | any(is.na(data[var_vec])) == FALSE) {
      keep_vars <- names_data[match(measures, all_measure_options)]
      rwo_names1 <- expand.grid(unique(data$group), keep_vars)
      row_names2 <-
        sprintf("%s_%s", rwo_names1[, 2], rwo_names1[, 1])
      row_names <- c("name", "variable", row_names2)
      res_tab1 <- as.data.frame(matrix(ncol = length(row_names)))
      colnames(res_tab1) <- row_names
      res_tab <- res_tab1[-1, ]

      #---------------------------------------------------------
      #  variables present
      #--------------------------------------------------------
    } else {
    # split variable by group
    split_x <- split(
      data,
      data$group
    )

    # summarize within each group
    stats <- lapply(
      split_x,
      missing_summary,
      var_vec = var_vec)


    # select only the measures needed
    select_measure_cols <- function(df, measures) {
      names(df)[names(df) == "variable"] <- "name"
      df$variable <- "missing"

      # select all the appropriate measure options
      all_measure_options <- c("absolute", "relative")
      names_data <- c("n", "relative_freq")
      keep_vars <- names_data[match(measures, all_measure_options)]

      res_tab <- df[, c("name","variable", keep_vars), drop = FALSE]

    }

    res_list_filtered <- lapply(
      stats,
      select_measure_cols,
      measures = measures
    )

    # apply to all column names except the first one "_group1"
    res_list <- lapply(names(res_list_filtered), function(g) {

      df <- res_list_filtered[[g]]

      # add suffix to all columns except first
      names(df)[-(1:2)] <- paste0(names(df)[-(1:2)], "_", g)

      df
    })

    res_tab <- Reduce(
      function(x, y) merge(x, y, by = c("name", "variable"), all = TRUE),
      res_list
    )
  }

}
  if (measure_style) {
    res_tab <- cat_unify_names_miss(
      data = data,
      group_var = group_var,
      treatment_arm = treatment_arm,
      measures_cat = measures,
      tab_cat_measure = res_tab,
      cat_vec = var_vec
    )
  }
  res_tab
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

cat_unify_names_miss <- function(data,
                            group_var = FALSE,
                            treatment_arm = FALSE,
                            measures_cat,
                            tab_cat_measure,
                            cat_vec = var_vec) {

  all_measure_options <- c("absolute", "relative")
  names_data <- c("n", "relative_freq")
  keep_vars <- names_data[match(measures_cat, all_measure_options)]

  #---------------------------------------------------------------------------------
  #1. Only one group
  #---------------------------------------------------------------------------------
  if (is.logical(group_var) & is.logical(treatment_arm)) {

    #---------------------------------------------------------------------
    # no categorical variables given or variables doe not have missing values
    #-------------------------------------------------------------------
    if(length(cat_vec) == 0 | any(is.na(data[cat_vec])) == FALSE) {
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
    if((length(cat_vec) == 0 | any(is.na(data[cat_vec])) == FALSE) & length(measures_cat) >= 1) {
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
