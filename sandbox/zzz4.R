#-----------------------------------------------------------------------------------
# Testing standardized mean differences
#----------------------------------------------------------------------------------

main_path <- "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository"
at <- readRDS(file = paste0(main_path,  "/02_data/created_data/triangle_at.rds"))
arm_A <- at[at$rnd == "A", ]
num_vec1 <- c("ki67", "score", "wbc")
categorial_variables <- c("ecog", "p53", "mipi")
all_vars <- c(num_vec1, categorial_variables)

#--------------------------------------------------------------
# One group
#--------------------------------------------------------------

helper_smd(data = arm_A,
           variables = all_vars,
           group_var,
           treatment_arm = FALSE)
# error



#----------------------------------------------------------------
# Two gorups
#---------------------------------------------------------------

helper_smd(data = arm_A,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)

# no variables there
helper_smd(data = arm_A,
           variables = c(),
           group_var = "maint_started",
           treatment_arm = FALSE)

# numeric variables: one variable is entirely missing
arm_A1 <- arm_A
arm_A1$ki67 <- NA
table(arm_A1$ki67)

helper_smd(data = arm_A1,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)


# numeric variables: one variable is entirely missing for one group
arm_A2 <- arm_A
arm_A2$ki67[arm_A2$maint_started == "1"] <- NA


helper_smd(data = arm_A2,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)


# categorial variables: one variable is entierly missing
arm_A3 <- arm_A
arm_A3$ecog <- NA

helper_smd(data = arm_A3,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)


# categorial variables: one variable is entierly missing for one group
arm_A4 <- arm_A
arm_A4$ecog[arm_A4$maint_started == "1"] <- NA

helper_smd(data = arm_A4,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)

# categorial variables_ one category is entierly missing for one group
arm_A5 <- arm_A
arm_A5$ecog[arm_A5$maint_started == "1" & arm_A5$ecog == "2"] <- NA

helper_smd(data = arm_A5,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = FALSE)


# using treatment instead of group varaible
helper_smd(data = arm_A,
           variables = all_vars,
           group_var = FALSE,
           treatment_arm = "maint_started")


#-----------------------------------------------------------------------------------
# Three groups
#----------------------------------------------------------------------------------

helper_smd(data = at,
           variables = all_vars,
           group_var = "rnd",
           treatment_arm = FALSE)


helper_smd(data = at,
           variables = c(),
           group_var = "rnd",
           treatment_arm = FALSE)


helper_smd(data = at,
           variables = all_vars,
           group_var = FALSE,
           treatment_arm = "rnd")

#----------------------------------------------------------------------------------
# Nested groups
#----------------------------------------------------------------------------------

helper_smd(data = at,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")

# no variables
helper_smd(data = at,
           variables = c(),
           group_var = "maint_started",
           treatment_arm = "rnd")

# numeric variable: one variable is entierly missing
at1 <- at
at1$ki67 <- NA

helper_smd(data = at1,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")
# error

# numeric variable: one variable is entierly missing in one group
at2 <- at
at2$ki67[at2$maint_started == "1" & rnd == "A+I"] <- NA

helper_smd(data = at2,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")


at3 <- at
at3$ki67[at3$maint_started == "1"] <- NA

helper_smd(data = at3,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")
# error

# categorial variable: one variable is entierly missing
at4 <- at
at4$ecog <- NA

helper_smd(data = at4,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")
# error

# categorial variable: one variable is entierly missing in one group

at5 <- at
at5$ecog[at5$maint_started == "1" & at5$rnd == "A+I"] <- NA

helper_smd(data = at5,
           variables = all_vars,
           group_var = "maint_started",
           treatment_arm = "rnd")
# error

# change group and treatment
helper_smd(data = at,
           variables = all_vars,
           group_var = "rnd",
           treatment_arm = "maint_started")
# wrong result

helper_smd(data = at,
           variables = c(),
           group_var = "rnd",
           treatment_arm = "maint_started")
