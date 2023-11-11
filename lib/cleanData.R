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
  dfaiWO <- read_excel("./data/Wrong Organ Analysis - BladderAI.xlsx")
  dfmanWO <- read_excel("./data/Wrong Organ Analysis - BladderMan.xlsx")
  # session 1 volumes
  t1.pluto.1 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Pluto1")
  t1.titan.1 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Titan1")
  t1.mars.1 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Mars1")
  t1.earth.1 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Earth1")
  t1.venus.1 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Venus1")
  t1.mars.2 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Mars2")
  t1.virgo.2 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Virgo2")
  t1.hydra.2 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Hydra2")
  t1.aries.2 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Aries2")
  t1.io.2 <- read_excel("./Data/Session1_worksheets.xlsx", sheet = "Io2")
  # session 2 volumes
  t2.ursa.1 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Ursa1")
  t2.sun.1 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Sun1")
  t2.pluto.1 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Pluto1")
  t2.draco.1 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Draco1")
  t2.titan.1 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Titan1")
  t2.leo.2 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Leo2")
  t2.orion.2 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Orion2")
  t2.libra.2 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Libra2")
  t2.ursa.2 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Ursa2")
  t2.moon.2 <- read_excel("./Data/Session2_worksheets.xlsx", sheet = "Moon2")
  # session 3 volumes 
  t3.ursa.1 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Ursa1")
  t3.sun.1 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Sun1")
  t3.mars.1 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Mars1")
  t3.draco.1 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Draco1")
  t3.dune.1 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Dune1")
  t3.sun.2 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Sun2")
  t3.orion.2 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Orion2")
  t3.titan.2 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Titan2")
  t3.hoth.2 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Hoth2")
  t3.dune.2 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Dune2")
  t3.cetus.3 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Cetus3")
  t3.lyra.3 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Lyra3")
  t3.canis.3 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Canis3")
  t3.ursa.3 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Ursa3")
  t3.lepus.3 <- read_excel("./Data/Session3_worksheets.xlsx", sheet = "Lepus3")
  
  # Cleaning
  # Wrong Organ DF for BAI - Replace label names, split into independent variable 
  # groups, remove unnecessary columns
  names(dfaiWO) <- c("Label","URL","Date","Mean_Score","Notes")
  # Keep certain columns
  dfaiWO = subset(dfaiWO, select = c(Label, Date))
  # Remove gold standard measurements
  dfaiWO <-subset(dfaiWO, grepl("Gold",dfaiWO$Label)==FALSE)
  # Add column for Exp_Group  - all in dfaiWO will be 1
  dfaiWO[, 'Exp_Group'] = 1
  # Add column for wrong organs - all values will be 1 since incorrect
  dfaiWO[, 'Wrong_Organ'] = 1
  # Wrong Organ DF for BMan - Replace label names, split into independent variable 
  # groups, remove unnecessary columns
  names(dfmanWO) <- c("Label","Label2","Timepoint","Exp_Group","Subject","SP",
                       "Q1","Q2","AnonLabel","URL","Mean_Score","Wrong_Organ")
  # Keep certain columns
  dfmanWO = subset(dfmanWO, select = c(Label, Timepoint, Wrong_Organ))
  # Remove gold standard measurements
  dfmanWO <-subset(dfmanWO, grepl("Gold",dfmanWO$Label)==FALSE)
  # Add column for Exp_Group  - all in dfmanWO will be 0
  dfmanWO[, 'Exp_Group'] = 0
  # Subset where wrong organ is true
  dfmanWO <-subset(dfmanWO, dfmanWO$Wrong_Organ==TRUE)
  # Add column for wrong organs - all values will be 1 since incorrect
  dfmanWO[, 'Wrong_Organ'] = 1
  # Convert data to time object
  dfaiWO[['Date']] <- as.POSIXct(dfaiWO[['Date']],
                                format = "%Y-%m-%dT%H:%M:%S")
  dfaiWO$Date <- as.Date(dfaiWO$Date)
  class(dfaiWO$Date)
  temp1 <- dfaiWO[dfaiWO$Date > "2023-02-01" &    # Extract data frame subset
                     dfaiWO$Date < "2023-02-11", ]
  temp1 <- cbind(temp1[,1:2],rep('T1',nrow(temp1)), temp1[,3:ncol(temp1)])
  names(temp1)[3] <- "Timepoint"
  temp2 <- dfaiWO[dfaiWO$Date > "2023-02-15" &    # Extract data frame subset
                     dfaiWO$Date < "2023-02-25", ]
  temp2 <- cbind(temp2[,1:2],rep('T2',nrow(temp2)), temp2[,3:ncol(temp2)])
  names(temp2)[3] <- "Timepoint"
  temp3 <- dfaiWO[dfaiWO$Date > "2023-02-25" &    # Extract data frame subset
                     dfaiWO$Date < "2023-04-25", ]
  temp3 <- cbind(temp3[,1:2],rep('T3',nrow(temp3)), temp3[,3:ncol(temp3)])
  names(temp3)[3] <- "Timepoint"
  # rebind
  dfaiWO <- rbind(temp1,temp2,temp3)
  # Remove date
  dfaiWO = subset(dfaiWO, select = -c(Date))
  # Concatenate into one wrong organ df
  dfWO = rbind(dfmanWO,dfaiWO)
  
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
  ind = ind[1] # row we're adding the 0 to
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
  # renumber
  row.names(dfman) <- 1:nrow(dfman)
  # length of unique names - 
  # An issue here - there are duplicate labels per timepoint...
  # To overcome this, append the timepoint value to the label
  dfman$Label = paste(dfman$Label,dfman$Timepoint)
  length(unique(dfman$Label))
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
  # renumber
  row.names(dfai) <- 1:nrow(dfai)
  # length of unique names
  # An issue here - there are duplicate labels per timepoint...
  # To overcome this, append the timepoint value to the label
  dfai$Label = paste(dfai$Label,dfai$Timepoint)
  length(unique(dfai$Label))
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
  # Concatenate dfai and dfman - first need to match DF number of fields
  # Apply round down method to Q1-3
  #dfai$Q1 <- floor(dfai$Q1)
  #dfai$Q2 <- floor(dfai$Q2)
  # Average AI questions Q1 and Q2
  dfai$Quality <- floor(rowMeans(cbind(dfai$Q1,dfai$Q2)))
  # Average and round down combination of Q2 and Q3 for manual
  dfman$Quality <- floor(rowMeans(cbind(dfman$Q1,dfman$Q2,dfman$Q3)))
  # Rearrange
  dfai <- select(dfai,Label,Exp_Group,Timepoint,Subject,SP,Quality,URL)
  dfman <- select(dfman,Label,Exp_Group,Timepoint,Subject,SP,Quality,URL)
  # Combine data
  df <- rbind(dfai,dfman)
  # Tabulate quality by Exp Group
  table(df$Exp_Group,df$Quality)
  # Tabulate quality by Timepoint
  table(df$Timepoint,df$Quality)
  
  # WRONG ORGANS
  # Find if label from df exists within dfWO
  # An issue here - there are duplicate labels per timepoint...
  # To overcome this, append the timepoint value to the label
  dfWO$Label = paste(dfWO$Label,dfWO$Timepoint)
  # Mark df(i) Wrong Organ column with a 1
  df$Wrong_Organ <- df$Label %in% c(dfWO$Label)
  # Rearrange
  df <- select(df,Label,Exp_Group,Timepoint,Subject,SP,Quality,Wrong_Organ,URL)
  # Tabulate to check. There should be 79 true
  table(df$Wrong_Organ)
  # Display some prelim results
  table(df$Exp_Group,df$Wrong_Organ)
  table(df$Timepoint,df$Wrong_Organ)

  # BLADDER VOLUMES
  # DATA APPENDING
  # Session 1
  filenames = c("t1.pluto.1","t1.titan.1","t1.mars.1","t1.venus.1","t1.earth.1",
                "t1.mars.2","t1.virgo.2","t1.hydra.2","t1.aries.2","t1.io.2")
  # Append column for SP name
  t1.pluto.1 <- cbind(t1.pluto.1[,1],rep('Pluto',nrow(t1.pluto.1)),
                      t1.pluto.1[,2:ncol(t1.pluto.1)])
  t1.titan.1 <- cbind(t1.titan.1[,1],rep('Titan',nrow(t1.titan.1)),
                      t1.titan.1[,2:ncol(t1.titan.1)])
  t1.mars.1 <- cbind(t1.mars.1[,1],rep('Mars',nrow(t1.mars.1)),
                     t1.mars.1[,2:ncol(t1.mars.1)])
  t1.venus.1 <- cbind(t1.venus.1[,1],rep('Venus',nrow(t1.venus.1)),
                      t1.venus.1[,2:ncol(t1.venus.1)])
  t1.earth.1 <- cbind(t1.earth.1[,1],rep('Earth',nrow(t1.earth.1)),
                      t1.earth.1[,2:ncol(t1.earth.1)])
  t1.mars.2 <- cbind(t1.mars.2[,1],rep('Mars',nrow(t1.mars.2)),
                     t1.mars.2[,2:ncol(t1.mars.2)])
  t1.virgo.2 <- cbind(t1.virgo.2[,1],rep('Virgo',nrow(t1.virgo.2)),
                      t1.virgo.2[,2:ncol(t1.virgo.2)])
  t1.hydra.2 <- cbind(t1.hydra.2[,1],rep('Hydra',nrow(t1.hydra.2)),
                      t1.hydra.2[,2:ncol(t1.hydra.2)])
  t1.aries.2 <- cbind(t1.aries.2[,1],rep('Aries',nrow(t1.aries.2)),
                      t1.aries.2[,2:ncol(t1.aries.2)])
  t1.io.2 <- cbind(t1.io.2[,1],rep('Io',nrow(t1.io.2)),
                   t1.io.2[,2:ncol(t1.io.2)])
  names(t1.pluto.1)[2] = "SP"
  names(t1.titan.1)[2] = "SP"
  names(t1.mars.1)[2] = "SP"
  names(t1.venus.1)[2] = "SP"
  names(t1.earth.1)[2] = "SP"
  names(t1.mars.2)[2] = "SP"
  names(t1.virgo.2)[2] = "SP"
  names(t1.hydra.2)[2] = "SP"
  names(t1.aries.2)[2] = "SP"
  names(t1.io.2)[2] = "SP"
  # Session 2
  t2.ursa.1 <- cbind(t2.ursa.1[,1],rep('Ursa',nrow(t2.ursa.1)),
                     t2.ursa.1[,2:ncol(t2.ursa.1)])
  t2.sun.1 <- cbind(t2.sun.1[,1],rep('Sun',nrow(t2.sun.1)),
                    t2.sun.1[,2:ncol(t2.sun.1)])
  t2.pluto.1 <- cbind(t2.pluto.1[,1],rep('Pluto',nrow(t2.pluto.1)),
                      t2.pluto.1[,2:ncol(t2.pluto.1)])
  t2.draco.1 <- cbind(t2.draco.1[,1],rep('Draco',nrow(t2.draco.1)),
                      t2.draco.1[,2:ncol(t2.draco.1)])
  t2.titan.1 <- cbind(t2.titan.1[,1],rep('Titan',nrow(t2.titan.1)),
                      t2.titan.1[,2:ncol(t2.titan.1)])
  t2.leo.2 <- cbind(t2.leo.2[,1],rep('Leo',nrow(t2.leo.2)),
                    t2.leo.2[,2:ncol(t2.leo.2)])
  t2.orion.2 <- cbind(t2.orion.2[,1],rep('Orion',nrow(t2.orion.2)),
                      t2.orion.2[,2:ncol(t2.orion.2)])
  t2.libra.2 <- cbind(t2.libra.2[,1],rep('Libra',nrow(t2.libra.2)),
                      t2.libra.2[,2:ncol(t2.libra.2)])
  t2.ursa.2 <- cbind(t2.ursa.2[,1],rep('Ursa2',nrow(t2.ursa.2)),
                     t2.ursa.2[,2:ncol(t2.ursa.2)])
  t2.moon.2 <- cbind(t2.moon.2[,1],rep('Moon',nrow(t2.moon.2)),
                     t2.moon.2[,2:ncol(t2.moon.2)])
  names(t2.ursa.1)[2] = "SP"
  names(t2.sun.1)[2] = "SP"
  names(t2.pluto.1)[2] = "SP"
  names(t2.draco.1)[2] = "SP"
  names(t2.titan.1)[2] = "SP"
  names(t2.leo.2)[2] = "SP"
  names(t2.orion.2)[2] = "SP"
  names(t2.libra.2)[2] = "SP"
  names(t2.ursa.2)[2] = "SP"
  names(t2.moon.2)[2] = "SP"
  
  # Session 3
  # DAY 3 CHANGES
  t3.ursa.1 <- cbind(t3.ursa.1[,1],rep('Ursa',nrow(t3.ursa.1)),
                     t3.ursa.1[,2:ncol(t3.ursa.1)])
  t3.sun.1 <- cbind(t3.sun.1[,1],rep('Sun',nrow(t3.sun.1)),
                    t3.sun.1[,2:ncol(t3.sun.1)])
  t3.mars.1 <- cbind(t3.mars.1[,1],rep('Mars',nrow(t3.mars.1)),
                     t3.mars.1[,2:ncol(t3.mars.1)])
  t3.draco.1 <- cbind(t3.draco.1[,1],rep('Draco',nrow(t3.draco.1)),
                      t3.draco.1[,2:ncol(t3.draco.1)])
  t3.dune.1 <- cbind(t3.dune.1[,1],rep('Dune',nrow(t3.dune.1)),
                     t3.dune.1[,2:ncol(t3.dune.1)])
  t3.sun.2 <- cbind(t3.sun.2[,1],rep('Sun2',nrow(t3.sun.2)),
                    t3.sun.2[,2:ncol(t3.sun.2)])
  t3.orion.2 <- cbind(t3.orion.2[,1],rep('Orion',nrow(t3.orion.2)),
                      t3.orion.2[,2:ncol(t3.orion.2)])
  t3.titan.2 <- cbind(t3.titan.2[,1],rep('Titan',nrow(t3.titan.2)),
                      t3.titan.2[,2:ncol(t3.titan.2)])
  t3.hoth.2 <- cbind(t3.hoth.2[,1],rep('Hoth',nrow(t3.hoth.2)),
                     t3.hoth.2[,2:ncol(t3.hoth.2)])
  t3.dune.2 <- cbind(t3.dune.2[,1],rep('Dune2',nrow(t3.dune.2)),
                     t3.dune.2[,2:ncol(t3.dune.2)])
  t3.cetus.3 <- cbind(t3.cetus.3[,1],rep('Cetus',nrow(t3.cetus.3)),
                      t3.cetus.3[,2:ncol(t3.cetus.3)])
  t3.lyra.3 <- cbind(t3.lyra.3[,1],rep('Lyra',nrow(t3.lyra.3)),
                     t3.lyra.3[,2:ncol(t3.lyra.3)])
  t3.canis.3 <- cbind(t3.canis.3[,1],rep('Canis',nrow(t3.canis.3)),
                      t3.canis.3[,2:ncol(t3.canis.3)])
  t3.ursa.3 <- cbind(t3.ursa.3[,1],rep('Ursa2',nrow(t3.ursa.3)),
                     t3.ursa.3[,2:ncol(t3.ursa.3)])
  t3.lepus.3 <- cbind(t3.lepus.3[,1],rep('Lepus',nrow(t3.lepus.3)),
                      t3.lepus.3[,2:ncol(t3.lepus.3)])
  
  # DAY 3 CHANGES
  names(t3.ursa.1)[2] = "SP"
  names(t3.sun.1)[2] = "SP"
  names(t3.mars.1)[2] = "SP"
  names(t3.draco.1)[2] = "SP"
  names(t3.dune.1)[2] = "SP"
  names(t3.sun.2)[2] = "SP"
  names(t3.orion.2)[2] = "SP"
  names(t3.titan.2)[2] = "SP"
  names(t3.hoth.2)[2] = "SP"
  names(t3.dune.2)[2] = "SP"
  names(t3.cetus.3)[2] = "SP"
  names(t3.lyra.3)[2] = "SP"
  names(t3.canis.3)[2] = "SP"
  names(t3.ursa.3)[2] = "SP"
  names(t3.lepus.3)[2] = "SP"
  
  ## BINDING DATA ###
  # Session 1
  # Bind
  df1all <- rbind(t1.pluto.1,t1.titan.1,t1.mars.1,t1.venus.1,t1.earth.1,t1.mars.2,
                  t1.virgo.2,t1.hydra.2,t1.aries.2,t1.io.2)
  # Remove unnecessary columns
  df1all = subset(df1all, select = -c(Notes,Bt_AI,Kt_AI,Bt_man,Kt_man))
  # subset the gold standards and the subject measurements
  df1all <-subset(df1all, AI != 2)
  # Initialize full matrix
  df1 = data.frame(matrix(ncol = 10, nrow = nrow(df1all)))
  names(df1) <- c("Subject","SP","Session","Day","Group","Order","Exp_Group",
                  "Volume","BFreeze","KFreeze")
  # reorder
  row.names(df1) <- 1:nrow(df1)
  # Add data into new full matrix - adjust volume position based on exp group
  df1[ ,1:7] <- df1all[ ,1:7]
  df1[ ,9:10] <- df1all[ ,13:14]
  for(n in 1:(nrow(df1))) {
    if (df1[n,7] == 0){
      df1[n,8] <- df1all[n,8]*df1all[n,9]*df1all[n,10]*0.52
    }
    if (df1[n,7] == 1){
      df1[n,8] <- df1all[n,12]
    }
  }
  # Session 2
  # Bind
  df2all <- rbind(t2.ursa.1,t2.sun.1,t2.pluto.1,t2.draco.1,t2.titan.1,t2.leo.2,
                  t2.orion.2,t2.libra.2,t2.ursa.2,t2.moon.2)
  # Remove unnecessary columns
  df2all = subset(df2all, select = -c(Notes,Bt_AI,Kt_AI,Bt_man,Kt_man))
  # subset the gold standards and the subject measurements
  df2all <-subset(df2all, AI != 2)
  # Initialize full matrix
  df2 = data.frame(matrix(ncol = 10, nrow = nrow(df2all)))
  names(df2) <- c("Subject","SP","Session","Day","Group","Order","Exp_Group",
                  "Volume","BFreeze","KFreeze")
  # reorder
  row.names(df2) <- 1:nrow(df2)
  # Add data into new full matrix - adjust volume position based on exp group
  df2[ ,1:7] <- df2all[ ,1:7]
  df2[ ,9:10] <- df2all[ ,13:14]
  for(n in 1:(nrow(df2))) {
    if (df2[n,7] == 0){
      df2[n,8] <- df2all[n,8]*df2all[n,9]*df2all[n,10]*0.52
    }
    if (df2[n,7] == 1){
      df2[n,8] <- df2all[n,12]
    }
  }
  # Session 3
  # DAY 3 CHANGES
  # Bind
  df3all <- rbind(t3.ursa.1,t3.sun.1,t3.mars.1,t3.draco.1,t3.dune.1,t3.sun.2,
                  t3.orion.2,t3.titan.2,t3.hoth.2,t3.dune.2,t3.cetus.3,t3.lyra.3,
                  t3.canis.3,t3.ursa.3,t3.lepus.3)
  # df3all <- rbind(t3.ursa.1,t3.sun.1,t3.mars.1,t3.draco.1,t3.dune.1,t3.sun.2,
  #                 t3.orion.2,t3.titan.2,t3.hoth.2,t3.dune.2)
  # Remove unnecessary columns
  df3all = subset(df3all, select = -c(Notes,Bt_AI,Kt_AI,Bt_man,Kt_man))
  # subset the gold standards and the subject measurements
  df3golds <-subset(df3all, AI == 2)
  df3all <-subset(df3all, AI != 2)
  # Initialize full matrix
  df3 = data.frame(matrix(ncol = 10, nrow = nrow(df3all)))
  names(df3) <- c("Subject","SP","Session","Day","Group","Order","Exp_Group",
                  "Volume","BFreeze","KFreeze")
  # reorder
  row.names(df3) <- 1:nrow(df3)
  # Add data into new full matrix - adjust volume position based on exp group
  df3[ ,1:7] <- df3all[ ,1:7]
  df3[ ,9:10] <- df3all[ ,13:14]
  for(n in 1:(nrow(df3))) {
    if (df3[n,7] == 0){
      df3[n,8] <- df3all[n,8]*df3all[n,9]*df3all[n,10]*0.52
    }
    if (df3[n,7] == 1){
      df3[n,8] <- df3all[n,12]
    }
  }
  # Bind all
  dfVols = rbind(df1,df2,df3)
  # Make label column
  # Change session to timepoint
  dfVols$Session[dfVols$Session == 1] <- 'T1'
  dfVols$Session[dfVols$Session == 2] <- 'T2'
  dfVols$Session[dfVols$Session == 3] <- 'T3'
  colnames(dfVols)[colnames(dfVols) == "Session"] ="Timepoint"
  # Remove excess variables
  rm(list=setdiff(ls(),c("df","dfVols")))
  # Make match columns in dfVols - need to match volume data to quality data
  # Remove 2 that occurs after some SP names
  dfVols$SP <- gsub("2*", "", dfVols$SP)
  dfVols$Match <- paste(dfVols$Subject,".",dfVols$SP,".",dfVols$Timepoint, sep = "")
  df$Match <- paste(df$Subject,".",df$SP,".",df$Timepoint, sep = "")
  # merge - only keeps observations from x that have a matching key in y
  df <- merge(df, dfVols, by = "Match")
  # Remove duplicates
  df <- subset(df, select=-c(Match,Subject.y,SP.y,Timepoint.y,Exp_Group.y,
                      KFreeze,BFreeze))
  # Rename
  names(df) <- c("Label","Exp_Group","Timepoint","Subject","SP","Quality",
                      "Wrong_Organ","URL","Day","Group","Order","Volume")
  # Reorder
  df <- select(df,Label,Exp_Group,Timepoint,Subject,SP,Group,Order, Wrong_Organ,
               Quality,Volume,URL)
  # Done!
  df
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
