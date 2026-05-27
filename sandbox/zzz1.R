#---------------------------------------------------------------------------------------------
# Testing categorial variables
#---------------------------------------------------------------------------------------------

main_path <- "/nfsmb/koll/katja.gutmair/MULTIPLY/rituximab_maintenance/repository"
at <- readRDS(file = paste0(main_path,  "/02_data/created_data/triangle_at.rds"))
A_arm <- at[at$rnd == "A", ]
categorial_variables <- c("sex", "ecog", "stage")
table(A_arm$ecog, A_arm$maint_started)

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



#-----------------------------------------------------------------------------------
# One groups
#-----------------------------------------------------------------------------------
test <- helper_summarize_cat(data = A_arm,
                     cat_vec = categorial_variables,
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     #measures_cat = c("absolute"),
                     measure_style = FALSE)

# only one variable
test1 <- helper_summarize_cat(data = A_arm,
                     cat_vec = c("ecog"),
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     #measures_cat = c("absolute"),
                     measure_style = TRUE)


helper_summarize_cat(data = A_arm,
                     cat_vec = c(),
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = TRUE,
                     measures_cat = c("relative", "absolute"),
                     measure_style = FALSE)


helper_summarize_cat(data = A_arm,
                     cat_vec = categorial_variables,
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = TRUE,
                     measures_cat = c("relative"),
                     measure_style = TRUE)

# One variable missing
helper_summarize_cat(data = A_arm5,
                     cat_vec = categorial_variables,
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute"),
                     measure_style = TRUE)

# Category of one variable completely missing
helper_summarize_cat(data = A_arm4,
                     cat_vec = categorial_variables,
                     group_var = FALSE,
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)



#------------------------------------------------------------------------------------
# Two groups
#------------------------------------------------------------------------------------

helper_summarize_cat(data = A_arm,
                     cat_vec = categorial_variables,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = TRUE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)



helper_summarize_cat(data = A_arm,
                     cat_vec = c("ecog"),
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("relative"),
                     measure_style = TRUE)

helper_summarize_cat(data = A_arm,
                     cat_vec = c(),
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("relative","absolute"),
                     measure_style = FALSE)




# 1. One category is not present in one group
helper_summarize_cat(data = A_arm1,
                     cat_vec = categorial_variables,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

# 2. One category is completely missing in one groups
helper_summarize_cat(data = A_arm2,
                     cat_vec = categorial_variables,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

# 3. One variable in one group is completely missing
helper_summarize_cat(data = A_arm3,
                     cat_vec = categorial_variables,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)
# error

# 4. One varaible is completely missing in both groups
A_arm123 <- A_arm
A_arm123$ecog <- NA

helper_summarize_cat(data = A_arm123,
                     cat_vec = categorial_variables,
                     group_var = "maint_started",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

#------------------------------------------------------------------------------------
# Three groups
#-----------------------------------------------------------------------------------

helper_summarize_cat(data = at,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

helper_summarize_cat(data = at,
                     cat_vec = c(),
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute"),
                     measure_style = TRUE)


# 1. One category is not present in one group
helper_summarize_cat(data = at1,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

# 2. One category is completely missing in one groups
helper_summarize_cat(data = at2,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)


# 3. One variable in one group is completely missing
helper_summarize_cat(data = at3,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)
# error

helper_summarize_cat(data = at3,
                     cat_vec = c(),
                     group_var = "rnd",
                     treatment_arm = FALSE,
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)
#---------------------------------------------------------------------------------
# Nested groups
#---------------------------------------------------------------------------------

helper_summarize_cat(data = at,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

helper_summarize_cat(data = at,
                     cat_vec = c(),
                     group_var = "rnd",
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

# 1. One category is not present in one group
helper_summarize_cat(data = at4,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)

# 3. One variable in one group is completely missing
helper_summarize_cat(data = at6,
                     cat_vec = categorial_variables,
                     group_var = "rnd",
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = TRUE)
# error


#-------------------------------------------------------------------------------------
# Use only tretment_arm but not group_var
#-------------------------------------------------------------------------------------

helper_summarize_cat(data = at6,
                     cat_vec = categorial_variables,
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = FALSE)

helper_summarize_cat(data = at6,
                     cat_vec = c(),
                     treatment_arm = "maint_started",
                     new_line = FALSE,
                     measures_cat = c("absolute", "relative"),
                     measure_style = FALSE)
