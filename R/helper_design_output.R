

#' Helper function: sorting rows and columns after a prespecified order, adding summary measure
#'  identifiers, calculating sample size and transofr this into a nice `flextable()` output.
#'
#' @description
#' This helper function is called within the `Table1_flex` function and transforms
#' a data.frame (output of Table1_flex) into a publication-ready Table 1.
#'
#' @param tab1 A data.frame output of the `Table1_flex` function
#' @inheritParams Table1_flex
#'
#'
#' @return Table 1 as flextable object or `data.frame`
#'
#' @importFrom stringr str_subset
#' @importFrom flextable flextable bold hline set_header_labels align separate_header
#' compose align as_paragraph
#' @importFrom stats ave

#' @noRd

# tab1 <- out1
# data <- at
# group_var = "new_maint"
# treatment_arm = "new_treat3"
# measures_cat = c("absolute", "relative")
# measures_num = c("median", "min", "max")
# cat_var = categorial_variables
# new_line = FALSE
# flextable_output = TRUE
# sort_rows = c("ecog", "mipi", "ki67")
# add_measure_ident = TRUE
# treatment_order = NULL
# group_order = c("maintenance_started", "no_maintenance_started")

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
                          add_measure_ident = TRUE,
                          treatment_order = NULL,
                          group_order = NULL) {
  #------------------------------------------------------------------------------
  # Ordering rows
  #------------------------------------------------------------------------------
  if (is.logical(sort_rows)) {
    # to avoid strange ordering because of variable = NA, set NA to ""
    tab1$variable <- ifelse(is.na(tab1$variable), "", tab1$variable)

    tab1 <- tab1[
      order(
        tab1$name,
        tab1$variable == "missing",
        is.na(tab1$variable)
      ),
    ]


  } else{
    all_vars <- unique(tab1$name)
    tab1$name <- factor(tab1$name, levels = c(sort_rows, setdiff(all_vars, sort_rows)))
    # to avoid strange ordering because of variable = NA, set NA to ""
    tab1$variable <- ifelse(is.na(tab1$variable), "", tab1$variable)

    tab1 <- tab1[
      order(
        tab1$name,
        tab1$variable == "missing",
        is.na(tab1$variable)
      ),
    ]
  }


  #--------------------------------------------------------------------------------
  # adding summary measures identifiers to the variable names
  #-------------------------------------------------------------------------------
  if (add_measure_ident == TRUE) {
  #-----------------------------------------------------
  # a, categorical variables
  #-----------------------------------------------------


    if (length(measures_cat) == 2 && measures_cat[1] == "absolute" & measures_cat[2] == "relative") {
      measures_cat <- "both1"
    } else if (length(measures_cat) == 2) {
      measures_cat <- "both2"
    }

    all_measure_options_cat <- c("absolute", "relative", "both1", "both2")
    measure_idenifier_cat <- c(" [n]", " [%]", " [n (%)]", " [% (n)]")
    identifier_cat <-
      measure_idenifier_cat[match(measures_cat, all_measure_options_cat)]
  #-------------------------------------------------
  # numerical variables
  #-------------------------------------------------
    # Mapping
    all_measure_options_num <- c(
      "mean", "sd", "median", "min", "max",
      "quantile1", "quantile3"
    )

    measure_identifier_num <- c(
      "Mean", "SD", "Median", "Min",
      "Max", "Q1", "Q3"
    )

    measures_lab <- measure_identifier_num[
      match(measures_num, all_measure_options_num)
    ]

    # Build identifier
    identifier_num <- switch(
      length(measures_lab),

      paste0(" [", measures_lab, "]"),

      paste0(
        " [",
        measures_lab[1],
        " (",
        measures_lab[2],
        ")]"
      ),

      paste0(
        " [",
        measures_lab[1],
        " (",
        measures_lab[2],
        "-",
        measures_lab[3],
        ")]"
      )
    )


    # adding the summary measure identifier, ordering the rows
    tab1$name <- ifelse(
      tab1$name %in% cat_var,
      paste0(tab1$name, identifier_cat),
      paste0(tab1$name, "\n", identifier_num)
    )
  }

  #----------------------
  # delete duplicates
  #-----------------------
  colnames <- colnames(tab1)
  dub_var <- c(
    "name",
    str_subset(colnames, pattern = "p-value"),
    str_subset(colnames, pattern = "SMD")
  )

  #-------------------------------------------------------------------------------
  # adding a new line between variables
  #-------------------------------------------------------------------------------
  if (new_line == TRUE) {
    # tab1 <- tab1 %>%
    #   group_by(name) %>%
    #   mutate_at(dub_var[-1], function(x) {
    #     ifelse(row_number() == 1, NA, x)
    #   }) %>%
    #   mutate_at(dub_var, function(x) {
    #     ifelse(duplicated(x), NA, x)
    #   })
    # Replace first row within each group by NA
    tab1[dub_var[-1]] <- lapply(
      tab1[dub_var[-1]],
      function(x) {
        ave(
          seq_along(x),
          tab1$name,
          FUN = function(i) {
            x_group <- x[i]
            x_group[1] <- NA
            x_group
          }
        )
      }
    )

    # Replace duplicated values by NA
    tab1[dub_var] <- lapply(
      tab1[dub_var],
      function(x) {
        x[duplicated(x)] <- NA
        x
      }
    )
  } else {
    # tab1 <- tab1 %>%
    #   group_by(name) %>%
    #   mutate_at(dub_var, function(x) {
    #     ifelse(duplicated(x), NA, x)
    #   })

    tab1[dub_var] <- lapply(
      tab1[dub_var],
      function(x) {
        ave(
          x,
          tab1$name,
          FUN = function(y) {
            y[duplicated(y)] <- NA
            y
          }
        )
      }
    )
  }

  #---------------------------------------------------------------------------------
  # calculation of sample size in each group
  #---------------------------------------------------------------------------------

  # if there is no group
  if (is.logical(group_var) & is.logical(treatment_arm)) {

    number_group <- data.frame(
      number = nrow(data),
      stringsAsFactors = FALSE)

    number_group$new_col <- paste0("measure\n(N=", number_group$number, ")")
    names(tab1)[names(tab1) == "measure"] <- number_group$new_col

   } else{
     # more than one group
      if (!is.logical(treatment_arm) & !is.logical(group_var)) {
        # merge the treatment and group variables into one variable
        data1 <- data

        data1$group <- paste(
          data[[treatment_arm]],
          data[[group_var]],
          sep = " "
        )

      } else if (!is.logical(group_var)) {
        # only grouping variable
        data1 <- data
        data1$group <- data[[group_var]]

      } else {
        data1 <- data
        data1$group <- data[[treatment_arm]]
      }
     # Calculate the sample size stratified by group
      number_group <- data.frame(
       group = names(table(data1$group)),
       number = as.vector(table(data1$group)),
       stringsAsFactors = FALSE
     )

     number_group$new_col <- paste0(
       number_group$group,
       "\n(N=",
       number_group$number,
       ")"
     )


    # matching the number per group with the colnames
    dub_var <- dub_var[dub_var != "name"]

    helper_col <-
      colnames[!colnames %in% c("name", "variable", dub_var)]

    #helper_col <- gsub(".*\\_", "", helper_col)
    helper_col <- sub("^[^_]*_", "", helper_col)

    colnames(tab1) <- c(
      "name", "variable", number_group$new_col[match(helper_col, number_group$group)],
      dub_var
    )


#--------------------------------------
    # reorder the columns
    #-----------------------------------
    tab1 <- sort_columns(
      data = tab1,
      treatment_order = treatment_order,
      group_order = group_order
    )
  }

  #####################################
  # Creation of the flextable
  ####################################
  if (flextable_output == TRUE) {
    if (!is.logical(treatment_arm) & !is.logical(group_var)) {

      # Remove the first underscore (this affects smd and p values only for splitting
      # nicely the header and deleting the value in p-value)
      idx <- grepl("SMD|p-value", colnames(tab1))


      colnames(tab1)[idx] <- sub(
        "_(?=SMD|p-value)",
        " ",
        colnames(tab1)[idx],
        perl = TRUE
      )



      #colnames(tab1) <- sub("_", " ", colnames(tab1))
      colnames(tab1) <- gsub(" value", "", colnames(tab1))

      group_vec <- unique(data[[group_var]])
      treat_vec <- unique(data[[treatment_arm]])
      cols <- colnames(tab1)

      treat_vec_escaped <- gsub(
        "([][{}()+*^$|\\\\?.])",
        "\\\\\\1",
        treat_vec
      )

      pattern <- paste0("(", paste(treat_vec_escaped, collapse = "|"), ") ")

      new_cols <- sub(pattern,"\\1|",cols)
      colnames(tab1) <- new_cols
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

    if (!is.logical(treatment_arm) & !is.logical(group_var)) {

      tab1 <- tab1 %>%
        separate_header(
          opts = "span-top",
          split = "|",
          fixed = TRUE
        ) %>%
        compose(i = 1, j = c(1, 2), part = "header", value = as_paragraph("")) %>%
        align(align = "center", part = "all")
    }
  }
  tab1
}
