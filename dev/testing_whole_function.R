
#-------------------------------------
# Problem missing values: when re. freq < 1%: in rel feq table the row with this variable is omitted so I
# cannot match the tables of absolute and realtive freq.
# Update:solved
#---------------------------------------
library(dplyr)
triangle2 <- readRDS(file = "/nfsmb/koll/katja.gutmair/MULTIPLY/data_request/MULTIPLY_queries/liquid_MULTIPLY/data_liquid_MULTIPLY2.rds")

triangle2 <- triangle2 %>%
  rename(`MIPI score` = score,
         `MIPI category` = mipi) %>%
  mutate(cyt = case_when(cyt == "0" ~  "small cell/classical",
                         cyt == "1" ~ "blastoid/pleomorph",
                         .default = NA),
         cyt = factor(cyt, levels = c("small cell/classical", "blastoid/pleomorph")),
         p53_status = case_when(p53_status == "0" ~  "p53 <= 50%",
                                p53_status == "1" ~ "p53 > 50%",
                                .default = NA),
         p53_status = factor(p53_status, levels = c("p53 <= 50%", "p53 > 50%")),
         stage = factor(stage, levels = c("1", "2", "3", "4")),
         mipi_c = case_when(mipi_c == "0" ~ "low",
                            mipi_c == "1" ~ "low-intermediate" ,
                            mipi_c == "2" ~ "high-intermediate",
                            mipi_c == "3" ~ "high"),
         mipi_c = factor(mipi_c, levels = c("low", "low-intermediate", "high-intermediate", "high")),
         ldh = ifelse(ldh == "1", "LDH > ULDH", "LDH <= ULDH"),
         ldh = factor(ldh, levels = c("LDH > ULDH", "LDH <= ULDH")),
         maint_started = ifelse(maint_started == "0", "no", "yes"),
         maint_started = factor(maint_started, levels = c("no", "yes"))) %>%
  mutate(group = ifelse(group == "IGH", "IGH Data", "no IGH Data"),
         group = as.factor(group))

sort_rows <- c("age", "sex", "ecog", "stage", "ldh", "mrd_infiltration","wbc", "MIPI score", "MIPI category", "cyt","ki67", "p53_status", "p53_category",
               "mipi_c", "rnd", "maint_started", "CR_induction", "OR_induction", "FFS24")


output <- flexTab1::Table1_flex(
  data = triangle2,
  group_var = "group",
  variables = c("rnd", "maint_started",  "FFS24",  "CR_induction", "OR_induction",
                "age", "sex", "MIPI score", "MIPI category", "mipi_c", "wbc", "ki67", "ecog", "stage",
                "cyt", "ldh","p53_status", "p53_category", "mrd_infiltration"),
  display_missings = TRUE,
  display_pvalue = TRUE,
  sort_rows = sort_rows,
  group_order = c("no IGH Data", "IGH Data"),
  flextable_output = FALSE
)



source("/nfsmb/koll/katja.gutmair/MULTIPLY/data_request/MULTIPLY_queries/liquid_MULTIPLY/missin_fun.R")
library(magrittr)
missing_total <- helper_summarize_missings(
  data = triangle2,
  var_vec = c("rnd", "maint_started",  "FFS24",  "CR_induction", "OR_induction",
              "age", "sex", "MIPI score", "MIPI category", "mipi_c", "wbc", "ki67", "ecog", "stage",
              "cyt", "ldh","p53_status", "p53_category", "mrd_infiltration"),
  group_var = "group") %>%
  mutate(variable = factor(variable, levels = sort_rows)) %>%
  arrange(variable)

table(triangle2$group)
test <- helper_summarize_missings(triangle2,
                          var_vec = c("rnd", "maint_started",  "FFS24",  "CR_induction", "OR_induction",
                                      "age", "sex", "MIPI score", "MIPI category", "mipi_c", "wbc", "ki67", "ecog", "stage",
                                      "cyt", "ldh","p53_status", "p53_category", "mrd_infiltration"),
                          group_var = "group",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

#-----------------------------------
# Problem 2: the order is not as it should be when comparing two groups within several
# treatment arms. Often, the p-values of group comparisons does not come right after the measures,
# but at the end of the table.
# solved
#-----------------------------------

itt <- readRDS(file = "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository/02_data/created_data/triangle_itt.rds")

numeric_variables <- c("age", "ldhr", "wbc", "score",  "ki67")

categorial <- c("sex", "race", "stage", "bsympt", "ecog",
                "ldh", "mipi", "asct_result", "cyt", "ki67bi", "p53")

variables1 <- c(numeric_variables, categorial)


arm_A_AI_itt <- flexTab1::Table1_flex(data = itt,
                       variables = variables1,
                       group_var = "maint_started",
                       treatment_arm = "rnd",
                       new_line = FALSE,
                       measures_cat = c("absolute", "relative"),
                       measures_num = c("median", "min", "max"),
                       display_pvalue = TRUE,
                       display_smd = FALSE,
                       display_missings = FALSE,
                       flextable_output = TRUE,
                       group_order = c("0", "1"),
                       treatment_order = c("I", "A", "A+I"))
library(stringr)


itt <- itt %>%
  mutate(rnd123 = case_when(rnd == "A" ~ "test 1",
                            rnd == "A+I" ~ "test test 2",
                            rnd == "I" ~ "test3"))

arm_A_AI_itt <- flexTab1::Table1_flex(data = itt,
                                      variables = variables1,
                                      group_var = "maint_started",
                                      treatment_arm = "rnd",
                                      new_line = FALSE,
                                      measures_cat = c("absolute", "relative"),
                                      measures_num = c("median", "min", "max"),
                                      display_pvalue = TRUE,
                                      display_smd = FALSE,
                                      display_missings = FALSE,
                                      flextable_output = TRUE,
                                      group_order = c("0", "1"),
                                      treatment_order = c("I", "A+I", "A"))


itt <- itt %>%
  rename(MIPI_score = score,
         MIPI_category = mipi) %>%
  mutate(response = case_when(rnd != "I" ~ asct_result,
                              rnd == "I" ~ ind_response)) %>%
  mutate(maint_RM = case_when(maint_started == "0" ~ "noRM",
                              maint_started == "1" ~ "RM"))

variables1 <- c("age", "ldhr", "wbc", "MIPI_score",  "ki67", "sex",
                "stage", "bsympt", "ecog",
                "ldh", "MIPI_category", "response", "cyt", "ki67bi", "p53")

sort_rows1 <- c("age", "sex", "bsympt", "ecog", "stage", "ldh", "ldhr","wbc",
                "MIPI_score" ,"MIPI_category",
                "cyt", "ki67", "ki67bi", "p53", "response")



tab1_output <- flexTab1::Table1_flex(
  data = itt,
  group_var = "maint_RM",
  treatment_arm = "rnd",
  variables = variables1,
  display_missings = TRUE,
  display_pvalue = TRUE,
  sort_rows = sort_rows1,
  treatment_order = c("A", "A+I", "I"),
  group_order = c("noRM", "RM")
)
