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
#' @noRd

helper_testing_num <- function(data,
                               num_vec,
                               group_var,
                               treatment_arm = FALSE) {
  if (is.logical(treatment_arm)) {
    # no treatment arm
    group_var1 <- sym(group_var)
    data1 <- data %>%
      rename(group = !!group_var1)
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  } else {
    # multiple treatment arms
    group_var1 <- sym(group_var)
    arm_var1 <- sym(treatment_arm)

    data1 <- data %>%
      rename(
        group = !!group_var1,
        treat_arm = !!arm_var1
      )
    number_arms <- length(unique(data1$treat_arm))
    number_groups <- length(unique(data1$group))
  }


  if (number_groups == 2 & number_arms == 1) {
    # Comparison of two groups (no treatment arm)
    if (length(num_vec) == 0) {
      col_names <- c("name", "p-value")
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
    } else {
      p_values <-
        unlist(lapply(data1[, num_vec], function(x) {
          (wilcox.test(x ~ group, data = data1))$p.value
        }))
      res <-
        data.frame(num_vec, ifelse(p_values >= 0.1, round(p_values, 2), round(p_values, 3)))
      colnames(res) <- c("name", "p-value")
      all_p <- res %>%
        mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`))
    }
  } else if (number_groups > 2 & number_arms == 1) {
    print("this method is not implemented yet")
  } else {
    if (length(num_vec) == 0) {
      col_names <- paste(unique(data1$treat_arm), "p-value", sep = "_")
      col_names <- c("name", col_names)
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
    } else {
      arms_list <- list()
      arms <- unique(data1$treat_arm)

      for (k in seq_along(arms)) {
        p_list <- list()
        i <- 1

        data2 <- data1 %>%
          filter(treat_arm == arms[k])

        p_values <-
          unlist(lapply(data2[, num_vec], function(x) {
            (wilcox.test(
              x ~ group,
              data = data2
            ))$p.value
          }))
        res <-
          data.frame(num_vec, ifelse(p_values >= 0.1, round(p_values, 2), round(p_values, 3)))
        colnames(res) <- c("name", "p-value")
        res_tab <- res %>%
          mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`))
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

#' @noRd

helper_testing_cat <- function(data, cat_vec, group_var, treatment_arm = FALSE) {
  require(janitor)

  if (is.logical(treatment_arm)) {
    group_var1 <- sym(group_var)
    data1 <- data %>%
      rename(group = !!group_var1)
    number_arms <- 1
    number_groups <- length(unique(data1$group))
  } else {
    group_var1 <- sym(group_var)
    arm_var1 <- sym(treatment_arm)

    data1 <- data %>%
      rename(
        group = !!group_var1,
        treat_arm = !!arm_var1
      )
    number_arms <- length(unique(data1$treat_arm))
    number_groups <- length(unique(data1$group))
  }

  # Comparison of two groups (only group_var, no treatment arms)
  if (number_groups == 2 & is.logical(treatment_arm)) {
    if (length(cat_vec) == 0) {
      col_names <- c("name", "p-value")
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
      all_p <- all_p[-1, ]
    } else {
      p_list <- list()
      i <- 1
      for (param in (cat_vec)) {
        param1 <- sym(param)
        p_val <- data1 %>%
          filter(!is.na(!!param1)) %>%
          tabyl(group, !!param1) %>%
          janitor::fisher.test()
        p_list[[i]] <- c(param, ifelse(p_val$p.value >= 0.1, round(p_val$p.value, 2),
                                       round(p_val$p.value, 3)
        ))
        i <- i + 1
      }
      res_tab <- (do.call("rbind", p_list))
      res_tab <- as.data.frame(res_tab)
      colnames(res_tab) <- c("name", "p-value")
      all_p <- res_tab %>%
        mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`))
    }
  } else if (number_groups > 2 & is.logical(treatment_arm)) {
    print("this method is not implemented yet")
  } else {
    # meaning if there are more than one treatment arm
    # what is not covered: group var >2 wihin a treatment arm
    if (length(cat_vec) == 0) {
      col_names <- c("name", paste(unique(data1$treat_arm), "p-value", sep = "_"))
      all_p <- as.data.frame(matrix(ncol = length(col_names)))
      colnames(all_p) <- col_names
      all_p <- all_p[-1, ]
    } else {
      arms_list <- list()
      arms <- unique(data1$treat_arm)
      for (k in seq_along(arms)) {
        p_list <- list()
        i <- 1

        data2 <- data1 %>%
          filter(treat_arm == arms[k])

        for (param in (cat_vec)) {
          param1 <- sym(param)
          p_val <- data2 %>%
            filter(!is.na(!!param1)) %>%
            tabyl(group, !!param1) %>%
            janitor::fisher.test()

          p_list[[i]] <- c(param, ifelse(p_val$p.value >= 0.1, round(p_val$p.value, 2),
                                         round(p_val$p.value, 3)
          ))
          i <- i + 1
        }
        res_tab <- (do.call("rbind", p_list))
        res_tab <- as.data.frame(res_tab)
        colnames(res_tab) <- c("name", "p-value")
        res_tab <- res_tab %>%
          mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`))
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
#' @noRd

helper_smd <-
  function(data,
           variables,
           group_var,
           treatment_arm = FALSE) {
    require(smd)

    if (is.logical(treatment_arm)) {
      # No group
      group_var1 <- sym(group_var)
      data1 <- data %>%
        rename(group = !!group_var1)
      number_arms <- 1
      number_groups <- length(unique(data1$group))
    } else {
      # More than one group
      group_var1 <- sym(group_var)
      arm_var1 <- sym(treatment_arm)

      data1 <- data %>%
        rename(
          group = !!group_var1,
          treat_arm = !!arm_var1
        )
      number_arms <- length(unique(data1$treat_arm))
      number_groups <- length(unique(data1$group))
    }

    if (number_groups == 2 & number_arms == 1) {
      data2 <- data1 %>%
        select(all_of(c(variables, "group")))
      md <- smd(
        x = data2,
        g = data2$group,
        na.rm = TRUE,
        std.error = TRUE
      )

      md <- md %>%
        select(variable, estimate) %>%
        rename(SMD = estimate) %>%
        mutate(SMD = round(SMD, 3))
    } else if (number_groups > 2 & number_arms == 1) {
      print("this method is not implemented yet")
    } else {
      arms_list <- list()
      arms <- unique(data1$treat_arm)

      for (k in seq_along(arms)) {
        p_list <- list()
        i <- 1

        data2 <- data1 %>%
          filter(treat_arm == arms[k]) %>%
          select(all_of(c(variables, "group")))

        md_res <- smd(
          x = data2,
          g = data2$group,
          na.rm = TRUE,
          std.error = TRUE
        )

        md_res <- md_res %>%
          select(variable, estimate) %>%
          rename(SMD = estimate) %>%
          mutate(SMD = round(SMD, 3))
        names(md_res)[names(md_res) == "SMD"] <- paste0(arms[k], "_SMD")

        arms_list[[k]] <- md_res
      }
      md <- Reduce(function(x, y) merge(x, y, by = "variable"), arms_list)
    }
    if (length(variables) == 0) {
      md <- md[-1, ]
    }
    md
  }
