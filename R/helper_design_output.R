# Problem: when having a treatment arm and a treatment group: I merge this with an
# blanc space. Later, I filter with this: This is kind of insecure, since this can cause
# problems, if the categories of treatment arm or groups contain also blanc spaces

# This function is not be called in case of empty variables

#########################################################
# This is a helper function for the Tab1 function, which gives the Tab1 a nice layout
# and converts it into a `flextable` object, if chosen
########################################################


#' Helper: Transforming a Tab1 data.frame Into a Flextable
#'
#' @description
#' This helper function is called within the `Table1_flex` function and transforms
#' a data.frame (output of Table1_flex) into a publication-ready Table 1.
#'
#'
#' @param tab1 A data.frame output of the `Table1_flex` function
#' @inheritParams Table1_flex
#'
#'
#' @return Table 1 as flextable object
#'
#' @importFrom dplyr mutate arrange group_by mutate_at summarise rename relocate
#' row_number n
#' @importFrom stringr str_subset
#' @importFrom tidyr all_of
#' @importFrom flextable flextable bold hline set_header_labels align separate_header
#' compose align as_paragraph
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @noRd
helper_layout <- function(tab1,
                          data,
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures_cat = c("absolute", "relative"),
                          measures_num = c("median", "min", "max"),
                          cat_var = c(),
                          new_line = FALSE,
                          flextable_output = TRUE,
                          sort_rows = NULL,
                          add_measure_ident = TRUE) {
  # Ordering rows
  tab1 <- tab1 %>%
    arrange(name) %>%
    arrange(factor(name, levels = sort_rows))

  # adding summary measures identifiers to the variable names
  if (add_measure_ident == TRUE) {
    if (length(measures_cat) == 2 &&
      measures_cat[1] == "absolute" & measures_cat[2] == "relative") {
      measures_cat <- "both1"
    } else if (length(measures_cat) == 2) {
      measures_cat <- "both2"
    }

    # set the info for the measures: categorical variables
    all_measure_options_cat <-
      c("absolute", "relative", "both1", "both2")
    measure_idenifier_cat <- c(" [n]", " [%]", " [n (%)]", " [% (n)]")
    identifier_cat <-
      measure_idenifier_cat[match(measures_cat, all_measure_options_cat)]


    # set the identifiers for the function: numeric variables
    all_measure_options_num <- c(
      "mean", "sd", "median", "min", "max",
      "quantile1", "quantile3"
    )

    measure_idenifier_num <-
      c("Mean", "SD", "Median", "Min", "Max", "Q1", "Q3")

    if (length(measures_num) == 1) {
      identifier_num1 <-
        measure_idenifier_num[match(measures_num, all_measure_options_num)]
      identifier_num <- paste0(" [", identifier_num1, "]")
    } else if (length(measures_num) == 2) {
      identifier_num1 <-
        measure_idenifier_num[match(measures_num[1], all_measure_options_num)]
      identifier_num2 <-
        measure_idenifier_num[match(measures_num[2], all_measure_options_num)]
      identifier_num <-
        paste0(" [", identifier_num1, " (", identifier_num2, ")]")
    } else {
      identifier_num1 <-
        measure_idenifier_num[match(measures_num[1], all_measure_options_num)]
      identifier_num2 <-
        measure_idenifier_num[match(measures_num[2], all_measure_options_num)]
      identifier_num3 <-
        measure_idenifier_num[match(measures_num[3], all_measure_options_num)]
      identifier_num <-
        paste0(" [", identifier_num1, " (", identifier_num2, "-", identifier_num3, ")]")
    }


    # adding the summary measure identifier, ordering the rows
    tab1 <- tab1 %>%
      mutate(name = ifelse(
        .data$name %in% cat_var,
        paste0(.data$name, identifier_cat),
        paste0(.data$name, "\n", identifier_num)
      ))
  }
  #############
  # delete duplicates
  #############
  colnames <- colnames(tab1)
  dub_var <- c(
    "name",
    str_subset(colnames, pattern = "p-value"),
    str_subset(colnames, pattern = "SMD")
  )

  if (new_line == TRUE) {
    tab1 <- tab1 %>%
      group_by(name) %>%
      mutate_at(dub_var[-1], function(x) {
        ifelse(row_number() == 1, NA, x)
      }) %>%
      mutate_at(dub_var, function(x) {
        ifelse(duplicated(x), NA, x)
      })
  } else {
    tab1 <- tab1 %>%
      group_by(name) %>%
      mutate_at(dub_var, function(x) {
        ifelse(duplicated(x), NA, x)
      })
  }


  ##########################
  # calculation of sample size
  #########################

  # if only one group
  if (is.logical(group_var)) {
    number_group <- data %>%
      summarise(number = n()) %>%
      mutate(new_col = paste0("measure\n(N=", .data$number, ")"))
    names(tab1)[names(tab1) == "measure"] <- number_group$new_col
  } else {
    # more than one group
    if (!is.logical(treatment_arm)) {
      # treatment arm + treatment group
      group_var1 <- sym(group_var)
      treatment_arm1 <- sym(treatment_arm)

      data1 <- data %>%
        rename(
          group = !!group_var1,
          treatment_arm = !!treatment_arm1
        ) %>%
        mutate(group = paste(treatment_arm, group, sep = " "))
    } else {
      # only treatment groups
      group_var1 <- sym(group_var)
      data1 <- data %>%
        rename(group = !!group_var1)
    }

    # Calculate the sample size stratified by group
    number_group <- data1 %>%
      group_by(group) %>%
      summarise(number = n()) %>%
      mutate(new_col = paste0(group, "\n(N=", number, ")"))

    # matching the number per group with the colnames

    dub_var <- dub_var[dub_var != "name"]
    helper_col <-
      colnames[!colnames %in% c("name", "variable", dub_var)]
    helper_col <- gsub(".*\\_", "", helper_col)


    colnames(tab1) <- c(
      "name", "variable", number_group$new_col[match(helper_col, number_group$group)],
      dub_var
    )

    # reorder the columns
    helper_col <- colnames(tab1)
    helper_col <- helper_col[!helper_col %in% c("name", "variable")]
    new_order <- c("name", "variable", helper_col)

    tab1 <- tab1 %>%
      relocate(all_of(new_order))
  }



  #####################################
  # Creation of the flextable
  ####################################
  if (flextable_output == TRUE) {
    if (!is.logical(treatment_arm)) {
      # Remove the first underscore (this affects smd and p values only for splitting
      # nicely the header and deleting the value in p-value)
      colnames(tab1) <- sub("_", " ", colnames(tab1))
      colnames(tab1) <- gsub(" value", "", colnames(tab1))
    }

    solid_lines <- which(!is.na(tab1$name))[-1]
    dashed_lines <- which(tab1$variable == "missing")

    tab1 <- flextable(tab1) %>%
      bold(part = "header") %>%
      bold(j = 1:2, part = "body") %>%
      hline(i = solid_lines - 1, part = "body") %>%
      hline(
        i = dashed_lines - 1,
        j = 2:ncol(tab1),
        part = "body"
      ) %>%
      set_header_labels(
        name = "",
        variable = ""
      ) %>%
      align(
        j = 3:ncol(tab1),
        align = "center",
        part = "all"
      )

    if (!is.logical(treatment_arm)) {
      tab1 <- tab1 %>%
        separate_header(
          opts = "span-top",
          split = " ",
          fixed = TRUE
        ) %>%
        compose(i = 1, j = c(1, 2), part = "header", value = as_paragraph("")) %>%
        align(align = "center", part = "all")
    }
  }
  tab1
}
