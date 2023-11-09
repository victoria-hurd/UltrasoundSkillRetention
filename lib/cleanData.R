# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/09/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Clean all data. Read in files, output dfK, dfB, and dfSurveys
# ---------------------------------------------------------------
### CLEANING BLADDER DATA ###
cleanBladder <- function() {
  
  df <- data.frame()
}



# ---------------------------------------------------------------
### CLEANING KIDNEY DATA ###
cleanKidney <- function() {
  df <- data.frame()
}



# ---------------------------------------------------------------
### CLEANING SURVEY DATA ###
cleanSurveys <- function() {
  # Read in datafiles 
  df1 <- read_excel("./data/ExitSurveys.xlsx", sheet = "Exit Surveys - 1")
  df2 <- read_excel("./data/ExitSurveys.xlsx", sheet = "Exit Surveys - 2")
  df3 <- read_excel("./data/ExitSurveys.xlsx", sheet = "Exit Surveys - 3")
  dfgroup <- read_excel("./data/SubjectExpGroup.xlsx")
  # Begin Cleaning
  # Start by assigning timepoints (T1,T2, or T3) to each observation
  df1 = cbind(df1[,1],rep('T1',nrow(df1)),df1[,2:ncol(df1)])
  names(df1)[2] <- "Timepoint"
  df2 = cbind(df2[,1],rep('T2',nrow(df2)),df2[,2:ncol(df2)])
  names(df2)[2] <- "Timepoint"
  df3 = cbind(df3[,1],rep('T3',nrow(df3)),df3[,2:ncol(df3)])
  names(df3)[2] <- "Timepoint"
  df <- rbind(df1,df2,df3)
  
  # Map confidence answers from to 0-4
  df[df == 'not at all confident'] <- "0"
  df[df == 'slightly confident'] <- "1"
  df[df == 'somewhat confident'] <- "2"
  df[df == 'fairly confident'] <- "3"
  df[df == 'completely confident'] <- "4"
  
  # Attach Exp Group to df
  # Initialize ExpGroup column
  df = cbind(df[,1],rep('NaN',nrow(df)),df[,2:ncol(df)])
  names(df)[1] <- "Subject"
  names(df)[2] <- "Exp_Group"
  
  for (n in 1:nrow(df)) {
    for (m in 1:nrow(dfgroup)) {
      if (df[n,1] == dfgroup[m,1]) {
        df[n,2] <- dfgroup[m,2]
      }
    }
  }
  
  # Alter data structure
  df$Exp_Group <- factor(df$Exp_Group , levels=c("Manual", "AI-guided"))
  df$Timepoint = as.factor(df$Timepoint)
  df[,13:22] = as.numeric(unlist(df[,13:22]))
  
  # Return DF
  df
}
