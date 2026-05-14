#----------------------------------------------------------------------------------
# Testing the helper function for "testing differences"
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# a, Testing numerical variables
#---------------------------------------------------------------------------------------


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


#-------------------------------------------------
# comparing two groups
#------------------------------------------------

helper_testing_num(data = arm_A,
                   num_vec = num_vec1,
                   group_var = "maint_started",
                   treatment_arm = FALSE)

# no numeric variables available
helper_testing_num(data = arm_A,
                   num_vec = c(),
                   group_var = "maint_started",
                   treatment_arm = FALSE)



# one group has only NA in one variable
helper_testing_num(data = arm_A1,
                   num_vec = num_vec1,
                   group_var = "maint_started",
                   treatment_arm = FALSE)

# no using treatment arm instead of group argument
helper_testing_num(data = arm_A,
                   num_vec = num_vec1,
                   group_var = FALSE,
                   treatment_arm = "maint_started")

# no numeric variable available
helper_testing_num(data = arm_A,
                   num_vec = c(),
                   group_var = FALSE,
                   treatment_arm = "maint_started")

helper_testing_num(data = arm_A1,
                   num_vec = num_vec1,
                   group_var = FALSE,
                   treatment_arm = "maint_started")

#-------------------------------------------------------------------------------------
# comparing three groups
#------------------------------------------------------------------------------------

helper_testing_num(data = at,
                   num_vec = num_vec1,
                   group_var = "rnd",
                   treatment_arm = FALSE)

# no numeric variables available
helper_testing_num(data = at,
                   num_vec = c(),
                   group_var = "rnd",
                   treatment_arm = FALSE)

# one group has only NA in one variable
helper_testing_num(data = at2,
                   num_vec = num_vec1,
                   group_var = "rnd",
                   treatment_arm = FALSE)


helper_testing_num(data = at,
                   num_vec = num_vec1,
                   group_var = FALSE,
                   treatment_arm = "rnd")

helper_testing_num(data = at,
                   num_vec = c(),
                   group_var = FALSE,
                   treatment_arm = "rnd")

helper_testing_num(data = at2,
                   num_vec = num_vec1,
                   group_var = FALSE,
                   treatment_arm = "rnd")

#--------------------------------------------------------------------------------------
# comparing nested groups
#------------------------------------------------------------------------------------

helper_testing_num(data = at,
                   num_vec = num_vec1,
                   group_var = "maint_started",
                   treatment_arm = "rnd")


helper_testing_num(data = at,
                   num_vec = c(),
                   group_var = "maint_started",
                   treatment_arm = "rnd")



# missing values in one group
helper_testing_num(data = at2,
                   num_vec = num_vec1,
                   group_var = "maint_started",
                   treatment_arm = "rnd")

# other way round (now we have three groups nested)
helper_testing_num(data = at,
                   num_vec = num_vec1,
                   group_var = "rnd",
                   treatment_arm = "maint_started")



helper_testing_num(data = at,
                   num_vec = num_vec1,
                   group_var = "rnd",
                   treatment_arm = "mipi")
