# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/09/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Clean all data. Read in files, output dfK, dfB, and dfSurveys
# ---------------------------------------------------------------
### CLEANING BLADDER DATA ###
cleanBladder <- function() {
  # Read in datafiles
  dfai <- read_excel("./data/BladderAI Task Data - Rescored, unclean.xlsx")
  dfman <- read_excel("./data/BladderMan Task Data - Rescored, unclean.xlsx")
  dfBAIWO <- read_excel("./data/Wrong Organ Analysis - BladderAI.xlsx")
  dfBManWO <- read_excel("./data/Wrong Organ Analysis - BladderMan.xlsx")
  # Cleaning
  # Wrong Organ DF for BAI - Replace label names, split into independent variable 
  # groups, remove unnecessary columns
  names(dfBAIWO) <- c("Label","URL","Date","Mean_Score","Notes")
  # Keep certain columns
  dfBAIWO = subset(dfBAIWO, select = c(Label, Date))
  # Remove gold standard measurements
  (dfBAIWO <-subset(dfBAIWO, grepl("Gold",dfBAIWO$Label)==FALSE))
  # Add column for Exp_Group  - all in dfBAIWO will be 1
  dfBAIWO[, 'Exp_Group'] = 1
  # Add column for wrong organs - all values will be 1 since incorrect
  dfBAIWO[, 'Wrong_Organ'] = 1
  # Wrong Organ DF for BMan - Replace label names, split into independent variable 
  # groups, remove unnecessary columns
  names(dfBManWO) <- c("Label","Label2","Timepoint","Exp_Group","Subject","SP",
                       "Q1","Q2","AnonLabel","URL","Mean_Score","Wrong_Organ")
  # Keep certain columns
  dfBManWO = subset(dfBManWO, select = c(Label, Timepoint, Wrong_Organ))
  # Remove gold standard measurements
  (dfBManWO <-subset(dfBManWO, grepl("Gold",dfBManWO$Label)==FALSE))
  # Add column for Exp_Group  - all in dfBManWO will be 0
  dfBManWO[, 'Exp_Group'] = 0
  # Subset where wrong organ is true
  (dfBManWO <-subset(dfBManWO, dfBManWO$Wrong_Organ==TRUE))
  # Add column for wrong organs - all values will be 1 since incorrect
  dfBManWO[, 'Wrong_Organ'] = 1
  # Convert data to time object
  dfBAIWO[['Date']] <- as.POSIXct(dfBAIWO[['Date']],
                                format = "%Y-%m-%dT%H:%M:%S")
  dfBAIWO$Date <- as.Date(dfBAIWO$Date)
  class(dfBAIWO$Date)
  temp1 <- dfBAIWO[dfBAIWO$Date > "2023-02-01" &    # Extract data frame subset
                     dfBAIWO$Date < "2023-02-11", ]
  temp1 <- cbind(temp1[,1:2],rep('T1',nrow(temp1)), temp1[,3:ncol(temp1)])
  names(temp1)[3] <- "Timepoint"
  temp2 <- dfBAIWO[dfBAIWO$Date > "2023-02-15" &    # Extract data frame subset
                     dfBAIWO$Date < "2023-02-25", ]
  temp2 <- cbind(temp2[,1:2],rep('T2',nrow(temp2)), temp2[,3:ncol(temp2)])
  names(temp2)[3] <- "Timepoint"
  temp3 <- dfBAIWO[dfBAIWO$Date > "2023-02-25" &    # Extract data frame subset
                     dfBAIWO$Date < "2023-04-25", ]
  temp3 <- cbind(temp3[,1:2],rep('T3',nrow(temp3)), temp3[,3:ncol(temp3)])
  names(temp3)[3] <- "Timepoint"
  # rebind
  dfBAIWO <- rbind(temp1,temp2,temp3)
  # Remove date
  dfBAIWO = subset(dfBAIWO, select = -c(Date))
  # Concatenate into one wrong organ df
  dfWO = rbind(dfBManWO,dfBAIWO)
  
  # MANUAL DATA
  # Rename columns
  names(dfman) <- c("Reviewer","Label","Date","URL","Q1","Q2","Q3",
                      "Q4","Q5","Q6","Total_Score","Mean_Score","Discrepancy")
  # Remove reviewer, url, total score, discrepancy on manual rescored
  dfman = subset(dfman, select = -c(Reviewer, Total_Score, Discrepancy))
  # subset the gold standards and the subject measurements
  #dfgoldsman <-subset(dfBman, grepl("Gold",dfBman$Label))
  dfman <-subset(dfman, grepl("Gold",dfman$Label)==FALSE)
  # Remove nans - rescored data is in a helpful format where we can just remove
  # these rows and be done with cleaning. However, same issue as above with 
  # erroneous FAST exam and missing val need to be dealt with first:
  # Erroneous FAST scan at 373!
  # There is also a missing Q6 value at 363
  apply(is.na(dfman), 2, which)
  ind = apply(is.na(dfman), 2, which)
  ind = ind[["Q6"]]
  (ind = ind[1]) # row we're adding the 0 to
  dfman$Q6[ind] <- 0
  ind <- apply(is.na(dfman), 2, which) 
  ind = ind[["Q6"]]
  # Remove erroneous FAST exam - there are four dodo.lyra.off.b.1, two are kidney
  #FASTlabel <-  dfman$Label[ind]
  # remove based on time - first two are kidney
  FASTtime <-  dfman$Date[ind]
  dfman <- subset(dfman, dfman$Date != FASTtime)
  # Number of images post removal
  nrow(dfman)
  apply(is.na(dfman), 2, which) # Check nan is gone
  
  # Manual data timepoint assignment - Rescored
  # Convert data to time object
  dfman[['Date']] <- as.POSIXct(dfman[['Date']],
                                  format = "%m/%d/%Y, %H:%M")
  dfman$Date <- as.Date(dfman$Date)
  class(dfman$Date)
  temp1 <- dfman[dfman$Date > "2023-02-01" &    # Extract data frame subset
                   dfman$Date < "2023-02-11", ]
  temp1 <- cbind(temp1[,1:2],rep('T1',nrow(temp1)), temp1[,3:ncol(temp1)])
  names(temp1)[3] <- "Timepoint"
  temp2 <- dfman[dfman$Date > "2023-02-15" &    # Extract data frame subset
                   dfman$Date < "2023-02-25", ]
  temp2 <- cbind(temp2[,1:2],rep('T2',nrow(temp2)), temp2[,3:ncol(temp2)])
  names(temp2)[3] <- "Timepoint"
  temp3 <- dfman[dfman$Date > "2023-02-25" &    # Extract data frame subset
                   dfman$Date < "2023-04-25", ]
  temp3 <- cbind(temp3[,1:2],rep('T3',nrow(temp3)), temp3[,3:ncol(temp3)])
  names(temp3)[3] <- "Timepoint"
  # rebind
  dfman <- rbind(temp1,temp2,temp3)
  
  # Make scores numeric - Q1 is type char rn
  str(dfman)
  dfman$Q1 <- as.numeric(dfman$Q1)
  str(dfman)
  
  # Sum question scores
  # calculate the mean per image - rescored
  dfman = dfman[order(dfman$Label),]
  hist(dfman$Mean_Score)
  # renumber
  row.names(dfman) <- 1:nrow(dfman)
  # length of unique names - 
  # An issue here - there are duplicate labels per timepoint...
  # To overcome this, append the timepoint value to the label
  dfman$Label = paste(dfman$Label,dfman$Timepoint)
  (length(unique(dfman$Label)))
  # Remove date
  dfman = subset(dfman, select = -c(Date))
  # allocate space
  dfavgb = data.frame(matrix(ncol = length(names(dfman)), nrow = 0))
  names(dfavgb) = names(dfman)
  i = 1
  for (n in 1:(nrow(dfman)-1)) {
    if (dfman[n,1] == dfman[n+1,1]) {
      temp = rbind(dfman[n,4:9],dfman[n+1,4:9])
      meanScores = colMeans(temp)
      dfavgb[i, ] = c(dfman[n,1:3], meanScores, dfman[n,10])
      i = i+1
    }
  }
  dfman <- dfavgb
  # Pull subject names
  dfman$Subject <- str_extract_all(dfman$Label, "[^.]+",simplify=TRUE)[ ,1]
  # Pull SP names
  dfman$SP <- str_extract_all(dfman$Label, "[^.]+",simplify=TRUE)[ ,2]
  # Capitalize SP names
  dfman$SP <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",dfman$SP,
                     perl = TRUE)
  # Check tabulation of subjects
  colSums(is.na(dfman))
  table(dfman$Subject) # looks great! Crab has 14 due to erroneous FAST 
  # Add Exp_Group - all are manual
  dfman$Exp_Group <- "Manual"
  
  # AI DATA
  # Rename columns
  names(dfai) <- c("Reviewer","Label","URL","Date","Q1","Q2",
                     "Total_Score","Mean_Score","Discrepancy")
  # Remove reviewer on manual data
  dfai = subset(dfai, select = -c(Reviewer, Total_Score, Discrepancy))
  # Check structure 
  str(dfai)
  # Check for NaNs
  colSums(is.na(dfai)) # normal for half of rescored to have nans
  # subset the gold standards and the subject measurements
  #dfgoldsai <-subset(dfBai, grepl("Gold",dfBai$Label))
  dfai <-subset(dfai, grepl("Gold",dfai$Label)==FALSE)
  # Pull subject names
  dfai$Subject <- str_extract_all(dfai$Label, "[^.]+",simplify=TRUE)[ ,1]
  # Pull SP names
  dfai$SP <- str_extract_all(dfai$Label, "[^.]+",simplify=TRUE)[ ,2]
  # Capitalize SP names
  dfai$SP <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",dfai$SP,
                    perl = TRUE)
  # Duck.venus.off.k.1, Panda.orion.off.k.2, and Rhino.io.off.k.2 were 
  # erroneously graded under the AI category even though they're .off. Remove
  # these. \\b enforces full match. Find where .off is not in label
  dfai <- subset(dfai, grepl("\\b.off",dfai$Label)==FALSE)
  # Kiwi.libra.on.b.1 issue: kiwi took two bladder images and Butterfly graded
  # both. We only want the later one, so remove the instances with earlier
  # timestamp. 
  dfai <- subset(dfai,grepl("2023-02-19T20:56:24.448000Z",dfai$Date)==FALSE)
  # Hawk.draco.on.b.4 issue: hawk took two bladder images and Butterfly graded
  # both. We only want the later one, so remove the instances with earlier
  # timestamp. 
  dfai <- subset(dfai,grepl("2023-02-18T21:39:38.761000Z",dfai$Date)==FALSE)
  # Tabulate subjects
  table(dfai$Subject) # One koala image never got graded
  # Add Exp_Group - all are AI
  dfai$Exp_Group <- "AI"
  
  # AI data timepoint assignment - Rescored Data
  # Convert data to time object
  dfai[['Date']] <- as.POSIXct(dfai[['Date']],
                                 format = "%Y-%m-%dT%H:%M:%S")
  dfai$Date <- as.Date(dfai$Date)
  class(dfai$Date)
  temp1 <- dfai[dfai$Date > "2023-02-01" &    # Extract data frame subset
                  dfai$Date < "2023-02-11", ]
  temp1 <- cbind(temp1[,1:2],rep('T1',nrow(temp1)), temp1[,3:ncol(temp1)])
  names(temp1)[3] <- "Timepoint"
  temp2 <- dfai[dfai$Date > "2023-02-15" &    # Extract data frame subset
                  dfai$Date < "2023-02-25", ]
  temp2 <- cbind(temp2[,1:2],rep('T2',nrow(temp2)), temp2[,3:ncol(temp2)])
  names(temp2)[3] <- "Timepoint"
  temp3 <- dfai[dfai$Date > "2023-02-25" &    # Extract data frame subset
                  dfai$Date < "2023-04-25", ]
  temp3 <- cbind(temp3[,1:2],rep('T3',nrow(temp3)), temp3[,3:ncol(temp3)])
  names(temp3)[3] <- "Timepoint"
  # rebind
  dfai <- rbind(temp1,temp2,temp3)
  # Remove temps
  rm(temp1,temp2,temp3)
  
  # Calculate mean per question
  # Sum question scores
  # calculate the mean per image - rescored
  dfai = dfai[order(dfai$Label),]
  hist(dfai$Mean_Score)
  # renumber
  row.names(dfai) <- 1:nrow(dfai)
  # length of unique names
  # An issue here - there are duplicate labels per timepoint...
  # To overcome this, append the timepoint value to the label
  dfai$Label = paste(dfai$Label,dfai$Timepoint)
  (length(unique(dfai$Label)))
  # Remove date
  dfai = subset(dfai, select = -c(Date))
  # allocate space
  dfavgb = data.frame(matrix(ncol = length(names(dfai)), nrow = 0))
  names(dfavgb) = names(dfai)
  i = 1
  for (n in 1:(nrow(dfai)-1)) {
    if (dfai[n,1] == dfai[n+1,1]) {
      temp = rbind(dfai[n,4:5],dfai[n+1,4:5])
      meanScores = colMeans(temp)
      dfavgb[i, ] = c(dfai[n,1:3], meanScores, dfai[n,6:9])
      i = i+1
    }
  }
  dfai <- dfavgb
  
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
  
  # Initialize ExpGroup column
  df = cbind(df[,1],rep('NaN',nrow(df)),df[,2:ncol(df)])
  names(df)[1] <- "Subject"
  names(df)[2] <- "Exp_Group"
  
  # Attach Exp Group to df
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
