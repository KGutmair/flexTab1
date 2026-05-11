utils::globalVariables(c("variable", "group", "name", "number", "estimate", "SMD",
                         "p-value", "treat_arm"))

#---------------------------------------------------------------------------------------------
# Testing numerical variables
#---------------------------------------------------------------------------------------------

main_path <- "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository"
at <- readRDS(file = paste0(main_path,  "/02_data/created_data/triangle_at.rds"))
arm_A <- at[at$rnd == "A", ]

# in one group, the numeric value is zero
arm_A$wbc[arm_A$maint_started] <- 0

# in one group, there are only missing values
arm_A1 <- arm_A
arm_A1$wbc[arm_A1$maint_started == "0"] <- NA

# now, with several groups
# in one group, the numeric value is zero
at1 <- at
at1$wbc[at1$maint_started == "0" & at1$rnd == "A"] <- 0

# in one group, there are only missing values
at2 <- at
at2$wbc[at2$maint_started == "0" & at2$rnd == "A"] <- NA

at3 <- at
at3$wbc[at3$rnd == "A"] <- NA


num_vec1 <- c("ki67", "score", "wbc", "ldhr")



#-------------------------------------------------------------
# 1. Test: only one group
#-------------------------------------------------------------
helper_summarize_num(data = arm_A,
                     num_vec = num_vec1,
                                 group_var = FALSE,
                                 treatment_arm = FALSE,
                                 new_line = TRUE,
                                 measures_num = c("min", "median", "max"),
                                 measure_style = TRUE)


helper_summarize_num(data = arm_A,
                     num_vec = num_vec1,
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("mean", "min"),
                     measure_style = TRUE)




# 2. Test: variables as vecorr directly in the command

helper_summarize_num(data = arm_A,
                     num_vec = c("ki67", "score", "wbc", "ldhr"),
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("max", "mean"),
                     measure_style = TRUE)

# 3. Test: only one/no variables

helper_summarize_num(data = arm_A,
                     num_vec = c("ki67"),
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median"),
                     measure_style = TRUE)


 helper_summarize_num(data = arm_A,
                     num_vec = c(),
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("min", "max"),
                     measure_style = TRUE)

#----------------------------------------------------------------------
# 2. Two or more groups
#----------------------------------------------------------------------

# 2 groups
 helper_summarize_num(data = arm_A,
                     num_vec = num_vec1,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median","max", "min"),
                     measure_style = TRUE)



# one group has missing values
helper_summarize_num(data = arm_A1,
                     num_vec = num_vec1,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median", "max"),
                     measure_style = TRUE)



helper_summarize_num(data = arm_A1,
                     num_vec = c(),
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median", "min", "max"),
                     measure_style = TRUE)



# 3. groups
helper_summarize_num(data = at1,
                     num_vec = num_vec1,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median", "min"),
                     measure_style = TRUE)

# with one gorup = NA
helper_summarize_num(data = at3,
                     num_vec = num_vec1,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_num = c("median", "min", "max"),
                     measure_style = TRUE)



#----------------------------------------------------------
# Nested groups
#-----------------------------------------------------------

helper_summarize_num(data = at,
                     num_vec = num_vec1,
                     group_var = "maint_started",
                     treatment_arm = "rnd",
                     new_line = TRUE,
                     measures_num = c("median"),
                     measure_style = TRUE)

helper_summarize_num(data = at,
                     num_vec = c(),
                     group_var = "maint_started",
                     treatment_arm = "rnd",
                     new_line = TRUE,
                     measures_num = c("min"),
                     measure_style = TRUE)

# one group is zero
helper_summarize_num(data = at2,
                     num_vec = num_vec1,
                     group_var = "maint_started",
                     treatment_arm = "rnd",
                     new_line = FALSE,
                     measures_num = c("median", "min", "max"),
                     measure_style = TRUE)


helper_summarize_num(data = at3,
                     num_vec = num_vec1,
                     group_var = "maint_started",
                     treatment_arm = "rnd",
                     new_line = FALSE,
                     measures_num = c("median", "min", "max"),
                     measure_style = TRUE)



helper_summarize_num(data = at3,
                     num_vec = c(),
                     group_var = "maint_started",
                     treatment_arm = "rnd",
                     new_line = TRUE,
                     measures_num = c("median", "min", "max"),
                     measure_style = TRUE)


# My function should lso work, when I only put a value at treatment arm

helper_summarize_num(data = at2,
                     num_vec = num_vec1,
                     treatment_arm = "rnd",
                     new_line = FALSE,
                     measures_num = c("median"),
                     measure_style = FALSE)

helper_summarize_num(data = at2,
                     num_vec = c(),
                     treatment_arm = "rnd",
                     new_line = FALSE,
                     measures_num = c("min", "max", "median"),
                     measure_style = TRUE)
