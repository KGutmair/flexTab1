#----------------------------------------------------------------------------------
# Testing now the whole Table1
#----------------------------------------------------------------------------------

main_path <- "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository"
at <- readRDS(file = paste0(main_path,  "/02_data/created_data/triangle_at.rds"))
arm_A <- at[at$rnd == "A", ]
num_vec1 <- c("ki67", "score", "wbc")
categorial_variables <- c("ecog", "p53", "mipi", "test_cat")
all_vars <- c(num_vec1, categorial_variables)

arm_A <- arm_A %>%
  mutate(test_cat = ifelse(ecog == "0", "opp",
                           ifelse(ecog == "1", "pepe", NA)),
         test_cat = as.factor(test_cat))

num_vec1 <- c("ki67", "score", "wbc")
categorial_variables <- c("ecog", "p53", "mipi", "test_cat")
all_vars <- c(num_vec1, categorial_variables)
#-------------------------------------------------------------------------------
# 1. Only one group
#--------------------------------------------------------------------------------

Table1_flex(data = arm_A,
            variables = c("mipi", "ki67", "ecog", "happy"),
            group_var = FALSE,
            treatment_arm = FALSE,
            new_line = FALSE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median", "min" ,"max"),
            display_pvalue = FALSE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ki67", "ecog", "happy", "mipi"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)


