if (requireNamespace("survival", quietly = TRUE)) {
 # Load pbc data from the survival package
pbc <- survival::pbc

baseline_var <- c("age", "chol", "platelet")

pbc[c("stage", "trt", "edema", "hepato")] <-
  lapply(pbc[c("stage", "trt", "edema", "hepato")], as.factor)

pbc <- pbc[!is.na(pbc$trt), ]

pbc$trt <- ifelse(pbc$trt == "1",
                  "D-penicillamine",
                  "Placebo")

 # Missing summary measure for one group -------------------------------------------

helper_summarize_missings(data = pbc,
                          var_vec = baseline_var,
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)

# Missing summary measure summarized into one column
helper_summarize_missings(data = pbc,
                          var_vec = baseline_var,
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

# Missing summary measure for two groups -----------------------------------------

helper_summarize_missings(data = pbc,
                          var_vec = baseline_var,
                          group_var = "trt",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


# Missing summary measure for a nested group structure ----------------------------

helper_summarize_missings(data = pbc,
                          var_vec = baseline_var,
                          group_var = "sex",
                          treatment_arm = "trt",
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)
 }
