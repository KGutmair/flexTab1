#----------------------------------------------------------------------------------
# Testing now the whole Table1
#----------------------------------------------------------------------------------
library(checkmate)
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
            variables = all_vars,
            group_var = FALSE,
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median", "min" ,"max"),
            display_pvalue = FALSE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ki67", "ecog", "happy", "mipi"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)



#-----------------------------------------------------------------------------
# Two groups
#-----------------------------------------------------------------------------

Table1_flex(data = arm_A,
            variables = all_vars,
            group_var = "maint_started",
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("relative", "absolute"),
            measures_num = c("min" ,"max", "median"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ki67", "ecog", "mipi"),
            add_measure_ident = FALSE,
            treatment_order = NULL,
            group_order = c("1", "0"))



#-----------------------------------------------------------------------------------
# Two groups with different specialities in its variables
#-----------------------------------------------------------------------------------

# one variable had entierly missings
arm_A1 <- arm_A
arm_A1$ki67 <- NA
arm_A1$ecog <- NA

# one variable has a small fracion of missings
arm_A2 <- arm_A
arm_A2$score[5] <- NA
arm_A2$ecog[10] <- NA

# one variable has missing in a certain group
arm_A3 <- arm_A
arm_A3$score[arm_A3$maint_started == "0"] <- NA
arm_A3$ecog[arm_A3$maint_started == "0"] <- NA

# one categorial variable has missings only in one category in one group
arm_A4 <- arm_A
arm_A4$ecog[arm_A4$ecog == "0" & arm_A4$maint_started == "1"] <- NA

# one categorial variable has missings only in one category in both groups
arm_A5 <- arm_A
arm_A5$ecog[arm_A5$ecog == "0"] <- NA

Table1_flex(data = arm_A5,
            variables = all_vars,
            group_var = "maint_started",
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("min" ,"max", "median"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ki67", "ecog", "mipi"),
            add_measure_ident = FALSE,
            treatment_order = NULL,
            group_order = NULL)


#---------------------------------------------------------------------------------
# two groups: treatment arm instead of group_var
#---------------------------------------------------------------------------------

Table1_flex(data = arm_A,
            variables = c(),
            group_var = "maint_started",
            treatment_arm = FALSE,
            new_line = FALSE,
            measures_cat = c("relative", "absolute"),
            measures_num = c("min" ,"max", "median"),
            display_pvalue = FALSE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ki67", "ecog", "mipi"),
            add_measure_ident = FALSE,
            treatment_order = NULL,
            group_order = NULL)


Table1_flex(data = arm_A,
            variables = all_vars,
            group_var = FALSE,
            treatment_arm = "maint_started",
            new_line = TRUE,
            measures_cat = c("relative", "absolute"),
            measures_num = c("min" ,"max", "median"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ki67", "ecog", "mipi"),
            add_measure_ident = TRUE,
            treatment_order = c("1", "0"),
            group_order = NULL)






#-----------------------------------------------------------------------------------
# Three groups
#----------------------------------------------------------------------------------

Table1_flex(data = at,
            variables = c(),
            group_var = FALSE,
            treatment_arm = "rnd",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = FALSE,
            treatment_order = c("I", "A", "A+I"),
            group_order = c("I", "A", "A+I"))





#-----------------------------------------------------------------------------------
# Three groups with different specialities in its variables
#----------------------------------------------------------------------------------
# one variable had entierly missings
at1 <- at
at1$ki67 <- NA
at1$ecog <- NA

# one variable has a small fracion of missings
at2 <- at
at2$score[5] <- NA
at2$ecog[10] <- NA

# one variable has missing in a certain group
at3 <- at
at3$score[at3$rnd == "A"] <- NA
at3$ecog[at3$rnd == "A"] <- NA

# one varaible is missing on two groups
at33 <- at
at33$score[at33$rnd != "A"] <- NA
at33$ecog[at33$rnd != "A"] <- NA

# one categorial variable has missings only in one category in one group
at4 <- at
at4$ecog[at4$ecog == "0" & at4$rnd == "A"] <- NA

at44 <- at
at44$ecog[at44$ecog == "0" & at44$rnd != "A"] <- NA

# one categorial variable has missings only in one category in both groups
at5 <- at
at5$ecog[at5$ecog == "0"] <- NA

Table1_flex(data = at1,
            variables = all_vars,
            group_var = "rnd",
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = FALSE,
            #treatment_order = c("I", "A", "A+I"),
            group_order = c("I", "A", "A+I"))




#-----------------------------------------------------------------------------------
# Nested structure
#-----------------------------------------------------------------------------------
class(at$rnd)
at$rnd <- as.factor(at$rnd)
class(at$maint_started)

num_vec1 <- c("ki67", "score", "wbc")
categorial_variables <- c("ecog", "p53", "mipi")
all_vars <- c(num_vec1, categorial_variables)


Table1_flex(data = at,
            variables = c("score"),
            group_var = "maint_started",
            treatment_arm = "rnd",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = c("I", "A", "A+I"),
            group_order = c("1", "0"))


Table1_flex(data = at,
            variables = all_vars,
            group_var = "rnd",
            treatment_arm = "maint_started",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            #sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            group_order = c("I", "A", "A+I"),
            treatment_order = c("1", "0"))


# to be tested: group names that have a "_" or a " " in their name


#-----------------------------------------------------------------------------------
# other variable names
#-----------------------------------------------------------------------------------

at$new_treat <- ifelse(at$rnd == "A", "arm A",
                       ifelse(at$rnd == "A+I", "arm A+I",
                              "arm I"))
table(at$new_treat)

at$new_treat1 <- ifelse(at$rnd == "A", "arm_A",
                       ifelse(at$rnd == "A+I", "arm_A+I",
                              "arm_I"))

at$new_treat2 <- ifelse(at$rnd == "A", "arm A",
                        ifelse(at$rnd == "A+I", "arm:A+I",
                               "arm-I"))

at$new_treat3 <- ifelse(at$rnd == "A", "arm:A",
                        ifelse(at$rnd == "A+I", "arm:A+I",
                               "arm:I"))

at$new_maint <- ifelse(at$maint_started == "0", "no maintenance started",
                       "maintenance started")
table(at$new_maint)

at$new_maint1 <- ifelse(at$maint_started == "0", "no:maintenance started",
                       "maintenance.started")
table(at$new_maint1)

at$new_maint2 <- ifelse(at$maint_started == "0", "no-maintenance-tarted",
                        "maintenance-started")
table(at$new_maint1)

num_vec1 <- c("ki67", "score", "wbc")
categorial_variables <- c("ecog", "p53", "mipi")
all_vars <- c(num_vec1, categorial_variables)


# 3 groups
Table1_flex(data = at,
            variables = all_vars,
            group_var = FALSE,
            treatment_arm = "new_treat1",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = FALSE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = FALSE,
            #treatment_order = c("arm I", "arm A", "arm A+I"),
            group_order = NULL)
# error when having an unterstrich

# 2 groups
Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint",
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = TRUE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = c("no maintenance started", "maintenance started"))

Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint2",
            treatment_arm = FALSE,
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)
# error when having a unterstrich

Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint2",
            treatment_arm = "rnd",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)


Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint2",
            treatment_arm = "new_treat",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)

# here is a beauty error in the flextable (it splits also on the free space in treatmet arm)


Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint2",
            treatment_arm = "new_treat2",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)

# here also schoenheitsfehler


Table1_flex(data = at,
            variables = all_vars,
            group_var = "new_maint1",
            treatment_arm = "new_treat2",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)

# So I need a rule that those characters in threatment should not get splitted
# in the header, the only split should be between treatment_var and group_var


# other way round
Table1_flex(data = at,
            variables = all_vars,
            treatment_arm = "new_maint1",
            group_var = "new_treat3",
            new_line = TRUE,
            measures_cat = c("absolute", "relative"),
            measures_num = c("median","min" ,"max"),
            display_pvalue = TRUE,
            display_smd = FALSE,
            display_missings = TRUE,
            flextable_output = TRUE,
            sort_rows = c("ecog", "mipi", "ki67"),
            add_measure_ident = TRUE,
            treatment_order = NULL,
            group_order = NULL)
# here the same: this splits on every Sonderzeichen, that is not numeric or alphabetic

# result:
# I only have difficulties, when I have a unterstich. This is by design, since I
# use the Unterstrich to append p_value names etc (arm A_p-value) and then use this also
# as identifier. if I then have a _ before, my programm splits on the wrong side.
# There are several approaches: in the very first step before doing any other computations,
# I could remove those "_" and add it than later in the final version. Or I ask specifically to
# split only on the last "_" in my code.

# and I have the Schoenheitsfehler in my flextable variable, since when I have several
# non alphanumeric characters within one category, it splits there several times.
# But not always, with : or - not
