#############################################################################
# Wilcoxon Test test for numeric variables testing differences in baseline
# variables between groups
#############################################################################

#' Helper: Testing for Difference for Numeric Variables
#'
#' @description
#' This function tests whether a numeric variable differs between two groups and provides
#' the corresponding p-values using the Wilcoxon test. It also supports testing
#' within a nested group structure, allowing comparisons between two subgroups
#' within a larger, superior group.
#'
#'
#' @inheritParams Table1_flex
#' @param num_vec A character vector containing the names of the numerical variables
#'                for which differences should be tested and p-values displayed.
#'
#' @return A data frame with variable names and corresponding p-values from the Wilcoxon test,
#'         indicating whether the distributions of the variables differ between two groups.
#' @importFrom dplyr rename mutate filter %>%
#' @importFrom rlang sym
#' @importFrom stats wilcox.test
#' @noRd

helper_testing_num <- function(data,
                               num_vec,
                               group_var = FALSE,
                               treatment_arm = FALSE) {

# replace this with the code from numerical or cateogiral variables
  # so that either group_var or treatment arm or both is accepted as
  # grouping varaible
  if (!is.logical(treatment_arm) & !is.logical(group_var)) {
    # merge the treatment and group variables into one variable
    data1 <- data
    data1$group <- data[[group_var]]
    data1$treat_arm <- data[[treatment_arm]]

    number_arms <- length(unique(data1$treat_arm))
    number_groups <- length(unique(data1$group))


  } else if (!is.logical(group_var)) {
    # only grouping variable
    data1 <- data
    data1$group <- data[[group_var]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  } else {
    data1 <- data
    data1$group <- data[[treatment_arm]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  }





#-------------------------------------------------------------------------------------
# 1. Comparisons of two groups without nesting (those two groups can be placed in the group
  # as well as in the arm argument)
#------------------------------------------------------------------------------------
  if (number_groups == 2 & number_arms == 1) {
    #----------------------------------------
    # a, no numerical variables available
    #---------------------------------------
    if (length(num_vec) == 0) {
      col_names <- c("name", "p-value")
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
     #------------------------------------
    # b, numerical variables available: calculate corresponding p-values
    #------------------------------------
    } else {
      # if one group has only NA in a specific variable, return NA
      p_values <-
        unlist(lapply(data1[, num_vec], function(x) {

          tryCatch({

            # remove rows with missing values in x or group
            tmp <- data.frame(x = x, group = data1$group)
            tmp <- tmp[complete.cases(tmp), ]

            # check whether both groups contain observations
            if(length(unique(tmp$group)) < 2) {
              return(NA_real_)
            }

            wilcox.test(x ~ group, data = tmp)$p.value

          }, error = function(e) {
            NA_real_
          })

        }))
      res <-
        data.frame(num_vec, ifelse(p_values >= 0.1, round(p_values, 2), round(p_values, 3)))
      colnames(res) <- c("name", "p-value")
      all_p <- res
      all_p$`p-value` = ifelse(all_p$`p-value` < 0.001, "< 0.001", all_p$`p-value`)

      res <-
        data.frame(num_vec, ifelse(p_values >= 0.1, round(p_values, 2), round(p_values, 3)))
      colnames(res) <- c("name", "p-value")
      all_p <- res
      all_p$`p-value` = ifelse(all_p$`p-value` < 0.001, "< 0.001", all_p$`p-value`)
    }
    #---------------------------------------------------------
    #2. Comparisons of more than two groups without nesting
    #--------------------------------------------------------
  } else if ((number_groups > 2 & number_arms == 1)) {
    return("this method is not implemented yet")
  } else {
    #--------------------------------------------------------
    # 3. Comparison of more than one arm, so nested structur
    #----------------------------------------------------------

    #---------------------------------------------
    # a0: if number of groups > 2: p-value calculation not implemented yet
    #--------------------------------------------

    if (number_groups > 2) {
      return("this method is not implemented yet")
    }

    #---------------------------------------------
    # a, no numeric variables there
    #---------------------------------------------
    else if (length(num_vec) == 0) {
      col_names <- paste(unique(data1$treat_arm), "p-value", sep = "_")
      col_names <- c("name", col_names)
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
      #------------------------------------------
      # b, numeric variables available and more than one treatment arm:
      # So calculate within every treatment arm the comparisons p-values
      #----------------------------------------
    } else {
      arms_list <- list()
      arms <- unique(data1$treat_arm)

      for (k in seq_along(arms)) {
        p_list <- list()
        i <- 1

        data2 <- data1[data1$treat_arm == arms[k], ]

        p_values <-
          unlist(lapply(data2[, num_vec], function(x) {

            tryCatch({

              # remove rows with missing values in x or group
              tmp <- data.frame(x = x, group = data2$group)
              tmp <- tmp[complete.cases(tmp), ]

              # check whether both groups contain observations
              if(length(unique(tmp$group)) < 2) {
                return(NA_real_)
              }

              wilcox.test(x ~ group, data = tmp)$p.value

            }, error = function(e) {
              NA_real_
            })

          }))


        res <-
          data.frame(num_vec, ifelse(p_values >= 0.1, round(p_values, 2), round(p_values, 3)))
        colnames(res) <- c("name", "p-value")
        res_tab <- res
        res_tab$`p-value` = ifelse(res_tab$`p-value` < 0.001, "< 0.001", res_tab$`p-value`)

        names(res_tab)[names(res_tab) == "p-value"] <-
          paste0(arms[k], "_p-value")
        arms_list[[k]] <- res_tab
      }
      all_p <- Reduce(function(x, y) {
        merge(x, y, by = "name")
      }, arms_list)
      colnames_p <- colnames(all_p)[-1]
    }
  }
  all_p
}




#############################################################################
# fisher exact test for categorical vatiables testing differences in baseline
# variables between groups
#############################################################################

#' Helper: Testing for Differences in Categorical Variables
#'
#' @description
#' This function tests whether a numeric variable differs between two groups and provides
#' the corresponding p-values using the Wilcoxon test. It also supports testing
#' within a nested group structure, allowing comparisons between two subgroups
#' within a larger, superior group.
#'
#' @inheritParams Table1_flex
#' @param cat_vec A character vector containing the names of the categorical variables
#'                for which differences should be tested and p-values displayed.
#'
#' @return A data frame with variable names and corresponding p-values from the Fisher´s exact test,
#'         indicating whether the distributions of the variables differ between two groups.
#'
#' @noRd

helper_testing_cat <- function(data,
                               cat_vec,
                               group_var = FALSE,
                               treatment_arm = FALSE) {

  #-----------------------------------------------------------------------
  # Function for computing fishers exact test or wilcoxon test depending on
  # the expected count. Saved in a vector with the variable name and the p-value
  #----------------------------------------------------------------------

  compute_cat_p <- function(data, var, group_var) {

    data_sub <- data[!is.na(data[[var]]) & !is.na(data[[group_var]]), ]
    tab <- table(data_sub[[group_var]], data_sub[[var]])

    # Remove empty rows/columns
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]

    # Need at least 2 rows and 2 columns
    if (nrow(tab) < 2 || ncol(tab) < 2) {
      return(c(var, NA))
    }

    p_val <- tryCatch({
      chi_test <- chisq.test(tab)

      if (any(chi_test$expected < 5)) {
        fisher.test(tab)
      } else {
        chi_test
      }
    }, error = function(e) NULL)

    p_out <- if (is.null(p_val)) {
      NA
    } else if (p_val$p.value >= 0.1) {
      round(p_val$p.value, 2)
    } else {
      round(p_val$p.value, 3)
    }
    c(var, p_out)
  }

  if (!is.logical(treatment_arm) & !is.logical(group_var)) {

    data1 <- data
    data1$group <- data[[group_var]]
    data1$treat_arm <- data[[treatment_arm]]

    number_arms <- length(unique(data1$treat_arm))
    number_groups <- length(unique(data1$group))


  } else if (!is.logical(group_var)) {
    # only grouping variable
    data1 <- data
    data1$group <- data[[group_var]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  } else {
    data1 <- data
    data1$group <- data[[treatment_arm]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  }


  #-------------------------------------------------------------------------------------
  # 1. Comparisons of two groups without nesting (those two groups can be placed in the group
  # as well as in the arm argument)
  #------------------------------------------------------------------------------------
  if (number_groups == 2 & number_arms == 1) {
    #----------------------------------------------------------
    # a, no cateogorial variables
    #---------------------------------------------------------
    if (length(cat_vec) == 0) {
      col_names <- c("name", "p-value")
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
      all_p <- all_p[-1, ]
    #------------------------------------------------------
      # categorial variables available
      #--------------------------------------------------
    } else {
      p_list <- lapply(
        cat_vec,
        compute_cat_p,
        data = data1,
        group_var = "group"
      )

      res_tab <- (do.call("rbind", p_list))
      res_tab <- as.data.frame(res_tab)
      colnames(res_tab) <- c("name", "p-value")
      all_p <- res_tab

      all_p$`p-value` <- ifelse(
        all_p$`p-value` < 0.001,
        "< 0.001",
        all_p$`p-value`
      )

    }
    #---------------------------------------------------------
    #2. Comparisons of more than two groups without nesting
    #--------------------------------------------------------
  } else if ((number_groups > 2 & number_arms == 1)) {
    return("this method is not implemented yet")
  } else {

    #--------------------------------------------------------
    # 3. Comparison of more than one arm, so nested structure
    #----------------------------------------------------------

    #---------------------------------------------
    # a0: if number of groups > 2: p-value calculation not implemented yet
    #--------------------------------------------

    if (number_groups > 2) {
      return("this method is not implemented yet")
    }

    #---------------------------------------------
    # a, no numeric variables there
    #---------------------------------------------
    else if (length(cat_vec) == 0) {
      col_names <- c("name", paste(unique(data1$treat_arm), "p-value", sep = "_"))
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
      all_p <- all_p[-1, ]

    #--------------------------------------------
      # numeric variables, compare between groups within treatment arms
    #--------------------------------------------
    } else {
      arms_list <- list()
      arms <- unique(data1$treat_arm)

      # loop over every treatment arm
      for (k in seq_along(arms)) {
        p_list <- list()
        i <- 1

        data2 <- data1[data1$treat_arm == arms[k], ]

        # calculate within every treatment arm
        p_list <- lapply(
          cat_vec,
          compute_cat_p,
          data = data2,
          group_var = "group"
        )

        res_tab <- (do.call("rbind", p_list))
        res_tab <- as.data.frame(res_tab)
        colnames(res_tab) <- c("name", "p-value")
        res_tab$`p-value` <- ifelse(
          res_tab$`p-value` < 0.001,
          "< 0.001",
          res_tab$`p-value`
        )
        names(res_tab)[names(res_tab) == "p-value"] <- paste0(arms[k], "_p-value")
        arms_list[[k]] <- res_tab
      }
      all_p <- Reduce(function(x, y) merge(x, y, by = "name"), arms_list)
      colnames_p <- colnames(all_p)[-1]
    }
  }
  all_p
}




#############################################################################
# SMD for numeric and categorial variables
#############################################################################

#' Helper: Calculates Standardized mean differences
#'
#' @description
#' This function calculated the standardized mean differences of a variable between two groups.
#' It also supports testing within a nested group structure, allowing comparisons
#' between two subgroups within a larger, superior group.
#'
#'
#' @inheritParams Table1_flex
#' @return A data frame with variable names and standardized mean differences.
#' @importFrom smd smd
#' @noRd

helper_smd <- function(data,
                       variables,
                       group_var = FALSE,
                       treatment_arm = FALSE) {


  #-------------------------------------------------------------------------------
  # Function for calculating the SMD for a vector of variable
  #------------------------------------------------------------------------------

  compute_smd <- function(data, variables, group_var = "group") {

    # Identify invalid variables
    invalid_vars <- variables[
      sapply(variables, function(v) {

        any(
          tapply(
            data[[v]],
            data[[group_var]],
            function(x) all(is.na(x))
          )
        )
      })
    ]

    # Variables usable for SMD
    valid_vars <- setdiff(variables, invalid_vars)

    # Compute SMD
    if (length(valid_vars) > 0) {

      smd_data <- data[, c(valid_vars, group_var)]

      md <- smd(
        x = smd_data,
        g = smd_data[[group_var]],
        na.rm = TRUE,
        std.error = TRUE
      )

      md <- md[, c("variable", "estimate")]
      names(md)[names(md) == "estimate"] <- "SMD"

      md$SMD <- round(md$SMD, 3)

    } else {

      md <- data.frame(
        variable = character(0),
        SMD = numeric(0)
      )
    }

    # Add invalid variables
    if (length(invalid_vars) > 0) {

      md_na <- data.frame(
        variable = invalid_vars,
        SMD = NA
      )

      md <- rbind(md, md_na)
    }

    # Restore original order
    md <- md[match(variables, md$variable), ]

    rownames(md) <- NULL

    md
  }

  #--------------------------------------------------------
  # Setting group names
  #------------------------------------------------------------
  if (!is.logical(treatment_arm) & !is.logical(group_var)) {

    data1 <- data
    data1$group <- data[[group_var]]
    data1$treat_arm <- data[[treatment_arm]]

    number_arms <- length(unique(data1$treat_arm))
    number_groups <- length(unique(data1$group))


  } else if (!is.logical(group_var)) {
    # only grouping variable
    data1 <- data
    data1$group <- data[[group_var]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  } else {
    data1 <- data
    data1$group <- data[[treatment_arm]]
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  }

  #-------------------------------------------------------------------------------------
  # 1. Comparisons of two groups without nesting (those two groups can be placed in the group
  # as well as in the arm argument)
  #------------------------------------------------------------------------------------

    if (number_groups == 2 & number_arms == 1) {
      #----------------------------------------------------------
      # a, no  variables
      #---------------------------------------------------------
      if (length(variables) == 0) {
        col_names <- c("name", "SMD")
        all_smd <- as.data.frame(matrix(ncol = length(col_names)))
        colnames(all_smd) <- col_names
        md <- all_smd[-1, ]
        #------------------------------------------------------
        # b, categorical variables available
        #--------------------------------------------------
      } else {
        md <- compute_smd(
          data = data1,
          variables = variables,
          group_var = "group"
        )
      }
      #----------------------------------------------------------------
      # 2. Comparison of more than two groups without nesting
      #---------------------------------------------------------------
    } else if (number_groups > 2 & number_arms == 1) {
      return("this method is not implemented yet")
    } else {
      #--------------------------------------------------------
      # 3. Comparison of more than one arm, so nested structure
      #----------------------------------------------------------

      #---------------------------------------------
      # a0: if number of groups > 2: p-value calculation not implemented yet
      #--------------------------------------------

      if (number_groups > 2) {
        return("this method is not implemented yet")
      }
      #---------------------------------------------
      # a, no variables there
      #---------------------------------------------
      else if (length(variables) == 0) {
        col_names <- c("name", paste(unique(data1$treat_arm), "SMD", sep = "_"))
        all_smd <- as.data.frame(matrix(ncol = length(col_names)))
        colnames(all_smd) <- col_names
        md <- all_smd[-1, ]

        #--------------------------------------------
        # b, variables, compare between groups within treatment arms
        #--------------------------------------------

    } else {
      arms <- unique(data1$treat_arm)
      arms_list <- lapply(arms, function(a) {

        data_arm <- data1[data1$treat_arm == a, ]

        md_res <- compute_smd(
          data = data_arm,
          variables = variables,
          group_var = "group"
        )

        names(md_res)[names(md_res) == "SMD"] <-
          paste0(a, "_SMD")

        md_res
      })

      md <- Reduce(
        function(x, y) merge(x, y, by = "variable", all = TRUE),
        arms_list
      )

    }
    }
    md
  }
