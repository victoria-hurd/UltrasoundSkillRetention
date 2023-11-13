# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/12/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Organ acquisition rate statistical analysis

# ---------------------------------------------------------------
### STATISTICAL ANALYSIS ###
# After performing Chi Square Test of Independence
# If any cell values are less than 5, use Fisher's

# Contingency table:

#        | Correct | Incorrect |
#        |_________|___________|
# AI on  |____A____|_____B_____|
# AI off |____C____|_____D_____|
# --------------------------------
orgAcqAgg <- function(dfB) {
  # Print contingency table
  (BConTable = table(dfB$Exp_Group,dfB$Wrong_Organ))
  # Aggregated
  chisq.test(BConTable)
}

orgAcqT1 <- function(dfB) {
  # Per Timepoint - T1
  dfBT1 <-subset(dfB, dfB$Timepoint=="T1")
  (BConTableT1 = table(dfBT1$Exp_Group,dfBT1$Wrong_Organ))
  chisq.test(BConTableT1)
}

orgAcqT2 <- function(dfB) {
  # Per Timepoint - T2
  dfBT2 <-subset(dfB, dfB$Timepoint=="T2")
  (BConTableT2 = table(dfBT2$Exp_Group,dfBT2$Wrong_Organ))
  chisq.test(BConTableT2)
}

orgAcqT3 <- function(dfB) {
  # Per Timepoint - T3
  dfBT3 <-subset(dfB, dfB$Timepoint=="T3")
  (BConTableT3 = table(dfBT3$Exp_Group,dfBT3$Wrong_Organ))
  chisq.test(BConTableT3)
}