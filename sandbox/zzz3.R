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


#------------------------------------------------
# comparing one group
#------------------------------------------------


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



#-----------------------------------------------------------------------------------------
# Testing categorial variables
#--------------------------------------------------------------------------------------

main_path <- "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository"
at <- readRDS(file = paste0(main_path,  "/02_data/created_data/triangle_at.rds"))
A_arm <- at[at$rnd == "A", ]
categorial_variables <- c("ecog", "p53", "cyt", "mipi")
num_vec1 <- c("ki67", "wbc")


#----------------------------------------------
# Only one group
#-----------------------------------------------
# One variable category is completely missing
A_arm4 <- A_arm
A_arm4$ecog[A_arm4$ecog == 1] <- NA
table(A_arm4$ecog, useNA = "always")

# 2. One varaible is completely missing
A_arm5 <- A_arm
A_arm5$ecog <- NA
table(A_arm5$ecog, useNA = "always")



#----------------------------------------------
# Two groups
#----------------------------------------------
# 1. One category is not present in one group
A_arm1 <- A_arm
A_arm1$ecog[A_arm1$ecog == 2 & A_arm1$maint_started == "0"] <- "1"
table(A_arm1$ecog, A_arm1$maint_started)

# 2. One category is completely missing in one groups
A_arm2 <- A_arm
A_arm2$ecog[A_arm2$ecog == 1 & A_arm2$maint_started == "0"] <- NA
table(A_arm2$ecog, A_arm2$maint_started, useNA = "always")

# 3. One variable in one group is completely missing
A_arm3 <- A_arm
A_arm3$ecog[A_arm3$maint_started == "0"] <- NA
table(A_arm3$ecog, A_arm3$maint_started, useNA = "always")


#------------------------------------------------
# Three groups
#------------------------------------------------

# 1. One category is not present in one group
at1 <- at
at1$ecog[at1$ecog == 2 & at1$rnd == "A"] <- "1"
table(at1$ecog, at1$rnd)

# 2. One category is completely missing in one groups
at2 <- at
at2$ecog[at2$ecog == 1 & at2$rnd == "A"] <- NA
table(at2$ecog, at2$rnd, useNA = "always")

# 3. One variable in one group is completely missing
at3 <- at
at3$ecog[at3$rnd == "A"] <- NA
table(at3$ecog, at3$rnd, useNA = "always")


#-------------------------------------------------
# Nested groups
#------------------------------------------------

# 1. One category is not present in one group
at4 <- at
at4$ecog[at4$ecog == 2 & at4$maint_started == "0"] <- "1"


# 2. One category is completely missing in one groups
at5 <- at
at5$ecog[at5$ecog == 1 & at5$maint_started == "0"] <- NA
table(at5$ecog, at5$maint_started, useNA = "always")

# 3. One variable in one group is completely missing
at6 <- at
at6$ecog[at6$maint_started == "0"] <- NA
table(at6$ecog, at6$maint_started, useNA = "always")



#--------------------------------------------------------------------------------
# Comparing one group
#--------------------------------------------------------------------------------

# so this should not be possible
helper_testing_cat(data = A_arm,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = FALSE)
# error



#-----------------------------------------------------------------------------------
# comparing two groups
#---------------------------------------------------------------------------------


helper_testing_cat(data = A_arm,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = FALSE)

# no categorial variables available
helper_testing_cat(data = A_arm,
                   cat_vec = c(),
                   group_var = "maint_started",
                   treatment_arm = FALSE)



# One category is not present in one group
A_arm1 <- A_arm
A_arm1$ecog[A_arm1$ecog == 2 & A_arm1$maint_started == "0"] <- "1"
table(A_arm1$ecog, A_arm1$maint_started)

helper_testing_cat(data = A_arm1,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = FALSE)





# 2. One category is completely missing in one groups
A_arm2 <- A_arm
A_arm2$ecog[A_arm2$maint_started == "0"] <- NA
table(A_arm2$ecog, A_arm2$maint_started, useNA = "always")

helper_testing_cat(data = A_arm2,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = FALSE)




# using treatment arm instead of group argument
helper_testing_cat(data = A_arm,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = "maint_started")



# no numeric variable available
helper_testing_cat(data = A_arm,
                   cat_vec = c(),
                   group_var = FALSE,
                   treatment_arm = "maint_started")



#-----------------------------------------------------------------------------------
# comparing three groups
#----------------------------------------------------------------------------------


helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = "rnd",
                   treatment_arm = FALSE)
# error

# No categorial variables given
helper_testing_cat(data = at,
                   cat_vec = c(),
                   group_var = "rnd",
                   treatment_arm = FALSE)
# error

# one group has only NA in one variable
helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = "rnd",
                   treatment_arm = FALSE)


# 2. One category is completely missing in one groups
at2 <- at
at2$ecog[at2$ecog == 1 & at2$rnd == "A"] <- NA
table(at2$ecog, at2$rnd, useNA = "always")

helper_testing_cat(data = at2,
                   cat_vec = categorial_variables,
                   group_var = "rnd",
                   treatment_arm = FALSE)


# one group has only NA in one variable
at3 <- at
at3$ecog[at3$rnd == "A"] <- NA
table(at3$ecog, at3$rnd, useNA = "always")

helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = "rnd",
                   treatment_arm = FALSE)


# change group and treatment arms
helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = "rnd")

# No categorial variables given
helper_testing_cat(data = at,
                   cat_vec = c(),
                   group_var = FALSE,
                   treatment_arm = "rnd")


# one group has only NA in one variable
helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = "rnd")


# 2. One category is completely missing in one groups
at2 <- at
at2$ecog[at2$ecog == 1 & at2$rnd == "A"] <- NA
table(at2$ecog, at2$rnd, useNA = "always")

helper_testing_cat(data = at2,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = "rnd")


# one group has only NA in one variable
at3 <- at
at3$ecog[at3$rnd == "A"] <- NA
table(at3$ecog, at3$rnd, useNA = "always")

helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = FALSE,
                   treatment_arm = "rnd")



#----------------------------------------------------------------------------
# comparing nested groups
#---------------------------------------------------------------------------

helper_testing_cat(data = at,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = "rnd")

# one group has in every category only missing values
at3 <- at
at3$ecog[at3$rnd == "A"] <- NA
table(at3$ecog, at3$rnd, useNA = "always")

helper_testing_cat(data = at3,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = "rnd")


# one group has only in one category missing values
at2 <- at
at2$ecog[at2$ecog == 1 & at2$rnd == "A"] <- NA
table(at2$ecog, at2$rnd, useNA = "always")

helper_testing_cat(data = at2,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = "rnd")


# only missing values in maint_started == 1
at4 <- at
at4$ecog[at4$maint_started == "1"] <- NA
table(at4$ecog, at4$rnd, useNA = "always")

helper_testing_cat(data = at4,
                   cat_vec = categorial_variables,
                   group_var = "maint_started",
                   treatment_arm = "rnd")




