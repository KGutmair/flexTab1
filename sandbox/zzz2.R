#---------------------------------------------------------------------------------------------
# Testing missing values function
#---------------------------------------------------------------------------------------------

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



#----------------------------------------------------------------------------------
# One group
#----------------------------------------------------------------------------------

helper_summarize_missings(data = A_arm,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

arm1 <- A_arm
arm1$wbc[1] <- NA

helper_summarize_missings(data = arm1,
                          var_vec = "ldh",
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


data = arm1
var_vec = "wbc"
group_var = FALSE
treatment_arm = FALSE
measures = c("absolute", "relative")
measure_style = TRUE





helper_summarize_missings(data = A_arm,
                          var_vec = c(),
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("relative", "absolute"),
                          measure_style = TRUE)
# Check: if measure = relative, than the structure should not be measure but rel_freq
# How is this handeled in the categorial function?



helper_summarize_missings(data = A_arm,
                          var_vec = c("ecog", "wbc"),
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("relative", "absolute"),
                          measure_style = TRUE)


# One variable category is completely missing
helper_summarize_missings(data = A_arm4,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("relative"),
                          measure_style = TRUE)

# 2. One varaible is completely missing
helper_summarize_missings(data = A_arm5,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = FALSE,
                          treatment_arm = FALSE,
                          measures = c("relative"),
                          measure_style = TRUE)



#----------------------------------------------------------------------------------
# Two groups
#----------------------------------------------------------------------------------


helper_summarize_missings(data = A_arm,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("relative", "absolute"),
                          measure_style = TRUE)





# I have to solve this here als well as the empty string with only one group
helper_summarize_missings(data = A_arm,
                          var_vec = c(),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


helper_summarize_missings(data = A_arm,
                          var_vec = c("wbc", "ldh"),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("relative", "absolute"),
                          measure_style = TRUE)











# 1. One category is not present in one group
helper_summarize_missings(data = A_arm1,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)

# 2. One category is completely missing in one groups
helper_summarize_missings(data = A_arm2,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

# 3. One variable in one group is completely missing
helper_summarize_missings(data = A_arm3,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "maint_started",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


#-----------------------------------------------------------------------------------
# Three groups
#-----------------------------------------------------------------------------------

helper_summarize_missings(data = at,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)

helper_summarize_missings(data = at,
                          var_vec = c(),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

helper_summarize_missings(data = at,
                          var_vec = c("wbc", "ldh"),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


# 1. One category is not present in one group
helper_summarize_missings(data = at1,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)

# 2. One category is completely missing in one groups
helper_summarize_missings(data = at2,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


# 3. One variable in one group is completely missing
helper_summarize_missings(data = at3,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "rnd",
                          treatment_arm = FALSE,
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


#-----------------------------------------------------------------------------------
# Nested groups
#-----------------------------------------------------------------------------------

helper_summarize_missings(data = at,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "maint_started",
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)

helper_summarize_missings(data = at,
                          var_vec = c(categorial_variables, num_vec1),
                          group_var = "rnd",
                          treatment_arm = "maint_started",
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


helper_summarize_missings(data = at,
                          var_vec = c(),
                          group_var = "maint_started",
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


helper_summarize_missings(data = at,
                          var_vec = c("wbc", "score"),
                          group_var = "maint_started",
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


#-------------------------------------------------------------------------------
# Only treatment arm
#----------------------------------------------------------------------------

helper_summarize_missings(data = at,
                          var_vec = c(categorial_variables, num_vec1),
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)


helper_summarize_missings(data = at,
                          var_vec = c(),
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = TRUE)


helper_summarize_missings(data = at,
                          var_vec = c("wbc", "score"),
                          treatment_arm = "rnd",
                          measures = c("absolute", "relative"),
                          measure_style = FALSE)

