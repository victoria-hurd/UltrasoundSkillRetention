# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/09/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Clean all data. Read in files, output dfK, dfB, and dfSurveys
# ---------------------------------------------------------------

demographics <- function() {
  # Read in datafiles
  dfSub <- read_excel("./data/Demographics.xlsx")
  dfSP <- read_excel("./data/SPDemographics.xlsx")
  
  # ---------------------------------------------------------------
  ### DATA CLEAN ###
  str(dfSP)
  dfControl <- subset(dfSub, group == 0)
  dfExp <- subset(dfSub, group == 1)
  
  # ---------------------------------------------------------------
  ### TABULATE ###
  # Subjects (All)
  table(dfSub$sex)
  # calculate ages
  round(mean(na.omit(dfSub$age)))
  round(sd(na.omit(dfSub$age)))
  median(na.omit(dfSub$age))
  range(na.omit(dfSub$age))
  
  # Tabulate for IRB extension
  table(dfSub$sex,dfSub$race)
  table(dfSub$sex,dfSub$ethnicity)
  
  # Subjects (Baseline)
  # Sex
  table(dfControl$sex)
  round(table(dfControl$sex)/nrow(dfControl)*100)
  # Age
  round(mean(na.omit(dfControl$age)))
  round(sd(na.omit(dfControl$age)))
  median(na.omit(dfControl$age))
  range(na.omit(dfControl$age))
  # Race
  table(dfControl$race)
  round(table(dfControl$race)/nrow(dfControl)*100)
  # Ethnicity
  table(dfControl$ethnicity)
  round(table(dfControl$ethnicity)/nrow(dfControl)*100)
  # Education Level
  table(dfControl$education)
  round(table(dfControl$education)/nrow(dfControl)*100)
  # Major
  table(dfControl$major)
  round(table(dfControl$major)/nrow(dfControl)*100)
  # Anatomy Experience
  table(dfControl$anatomy_exp)
  round(table(dfControl$anatomy_exp)/nrow(dfControl)*100)
  # Anatomy Rate
  table(dfExp$anatomy_rate)
  round(table(dfExp$anatomy_rate)/nrow(dfExp)*100)
  
  # Subjects (Exp)
  # Sex
  table(dfExp$sex)
  round(table(dfExp$sex)/nrow(dfControl)*100)
  # Age
  round(mean(dfExp$age))
  round(sd(dfExp$age))
  median(na.omit(dfExp$age))
  range(na.omit(dfExp$age))
  
  # Race
  table(dfExp$race)
  round(table(dfExp$race)/nrow(dfExp)*100)
  # Ethnicity
  table(dfExp$ethnicity)
  round(table(dfExp$ethnicity)/nrow(dfExp)*100)
  # Education Level
  table(dfExp$education)
  round(table(dfExp$education)/nrow(dfExp)*100)
  # Major
  table(dfExp$major)
  round(table(dfExp$major)/nrow(dfExp)*100)
  # Anatomy Experience
  table(dfExp$anatomy_exp)
  round(table(dfExp$anatomy_exp)/nrow(dfExp)*100)
  # Anatomy Rate
  table(dfExp$anatomy_rate)
  round(table(dfExp$anatomy_rate)/nrow(dfExp)*100)

  # SPs
  # Sex
  table(dfSP$sex)
  round(table(dfSP$sex)/nrow(dfSP)*100)
  # Age
  round(mean(dfSP$age))
  round(sd(dfSP$age))
  # Race
  table(dfSP$race)
  round(table(dfSP$race)/nrow(dfSP)*100)
  # Ethnicity
  table(dfSP$ethnicity)
  round(table(dfSP$ethnicity)/nrow(dfSP)*100)
}