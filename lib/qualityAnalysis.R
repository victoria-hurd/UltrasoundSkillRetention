# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/12/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Analyze image quality data
# ---------------------------------------------------------------
### ANALYSIS - BLADDER ###
# Contingency table:

#        | Score:0 |  Score: 1 |
#        |_________|___________|
# AI on  |____A____|_____B_____|
# AI off |____C____|_____D_____|

bQualAll <- function(dfB) {
  # --------------------------------
  # WRONG ORGANS REMOVED
  # Print contingency table
  dfSlim <- subset(dfB, dfB$Wrong_Organ==FALSE)
  (ConTableWO = table(dfSlim$Exp_Group,dfSlim$Quality))
  # Aggregated
  chisq.test(ConTableWO)
  # Per Timepoint - T1
  dfBT1 <-subset(dfSlim, dfSlim$Timepoint=="T1")
  (ConTableT1 = table(dfBT1$Exp_Group,dfBT1$Quality))
  chisq.test(ConTableT1)
  # Per Timepoint - T2
  dfBT2 <-subset(dfSlim, dfSlim$Timepoint=="T2")
  (ConTableT2 = table(dfBT2$Exp_Group,dfBT2$Quality))
  chisq.test(ConTableT2)
  # Per Timepoint - T3
  dfBT3 <-subset(dfSlim, dfSlim$Timepoint=="T3")
  (ConTableT3 = table(dfBT3$Exp_Group,dfBT3$Quality))
  chisq.test(ConTableT3)

  # --------------------------------
  # WRONG ORGANS INCLUDED
  # Print contingency table
  print('Criteria 1: Wrong organs included. 0 represents no point, 1 represents point given.')
  (ConTable = table(dfB$Exp_Group,dfB$Quality))
  # Aggregated
  chisq.test(ConTable)
  # Per Timepoint - T1
  dfBT1 <-subset(dfB, dfB$Timepoint=="T1")
  (ConTableT1 = table(dfBT1$Exp_Group,dfBT1$Quality))
  chisq.test(ConTableT1)
  # Per Timepoint - T2
  dfBT2 <-subset(dfB, dfB$Timepoint=="T2")
  (ConTableT2 = table(dfBT2$Exp_Group,dfBT2$Quality))
  chisq.test(ConTableT2)
  # Per Timepoint - T3
  dfBT3 <-subset(dfB, dfB$Timepoint=="T3")
  (ConTableT3 = table(dfBT3$Exp_Group,dfBT3$Quality))
  chisq.test(ConTableT3)
}
