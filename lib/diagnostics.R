# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/09/23
# LAST EDITED:  11/13/23
# PROJECT:      POCUS Skill Retention Study
# TASK:         Script for bladder volume accuracy stats & viz
# ---------------------------------------------------------------

diagnostics <- function(dfB) {
  # Remove wrong organs
  # Subset
  dfWOrem <- subset(dfB,Wrong_Organ==FALSE)
  # How many observations did this remove?
  (nrow(dfWOrem)/nrow(dfB)*100)
  # Remove poor image quality?
  # Subset
  dfWOIQrem <- subset(dfWOrem,Quality==1)
  # How many observations did this remove?
  (nrow(dfWOIQrem)/nrow(dfWOrem)*100)
  (nrow(dfWOIQrem)/nrow(dfB)*100)
  # Calculate mean per group
  # For each SP, session, and group, volume needs averaged and subtracted
  # For wrong organs
  df_listWO <- dfWOIQrem %>%
    group_by(SP,Timepoint,Day, Group) %>%
    mutate(meanVol = (mean(Volume))) %>%
    mutate(volVar = (Volume - mean(Volume))) %>%
    group_split()
  vars = names(dfWOIQrem)
  dfWOIQrem = data.frame(matrix(ncol = ncol(dfWOIQrem)+2, nrow = nrow(dfWOIQrem)))
  names(dfWOIQrem) <- c(vars, "meanVol","volVar")
  i=1
  for (n in 1:(length(df_listWO))) {
    rowNum = nrow(as.data.frame(df_listWO[[n]]))
    dfWOIQrem[i:(i+rowNum-1), ] <- as.data.frame(df_listWO[[n]])
    i = i+rowNum
  }
  # Remove volumes below 100mL
  nOrig = nrow(dfWOIQrem)
  dfFull_noWO <- subset(dfWOIQrem,meanVol > 100)
  dfEmpty <- subset(dfWOIQrem,meanVol < 100)
  nAfter = nrow(dfFull_noWO)
  print(nOrig)
  print(nAfter)
  print(nOrig-nAfter)
  print(round(((nAfter)*100)/nOrig))
  print(round(((nAfter)*100)/nrow(dfB)))
  # Robutsness
  dfClean <- dfFull_noWO
  dfViz <- dfFull_noWO
  
  # STATISTICAL ANALYSIS
  ## WRONG ORGANS EXCLUDED
  # Need to perform Levene's between specific subsets of data to determine exactly
  # which groups differ from each other and how
  table(dfClean$Timepoint)
  dfClean$Timepoint <- as.factor(dfClean$Timepoint)
  dfClean$Exp_Group <- as.factor(dfClean$Exp_Group)
  # AGGREGATED:
  leveneTest(volVar ~ Exp_Group,data = dfClean)
  # COMBO 1: If experimental groups differ at T1
  dfT1 <- subset(dfClean,Timepoint=='T1')
  table(dfT1$Exp_Group)
  leveneTest(volVar ~ Exp_Group,data = dfT1)
  # COMBO 2: If experimental groups differ at T2
  dfT2 <- subset(dfClean,Timepoint=='T2')
  leveneTest(volVar ~ Exp_Group,data = dfT2)
  # COMBO 3: If experimental groups differ at T3
  dfT3 <- subset(dfClean,Timepoint=='T3')
  leveneTest(volVar ~ Exp_Group,data = dfT3)
  # COMBO 4: If timepoints differ for AI on
  dfon <- subset(dfClean,Exp_Group=='AI')
  leveneTest(volVar ~ Timepoint,data = dfon)
  # COMBO 5: If timepoints differ for AI off
  dfoff <- subset(dfClean,Exp_Group=='Manual')
  leveneTest(volVar ~ Timepoint,data = dfoff)
  
  # VISUALIZATIONS
  # create dummy data for 4 and 6 week measures
  dfplot = rbind(dfViz, c("dummy","dummy",'TX',"dummy","dummy",-1,-1,-1,FALSE,1,-1000,"dummy",-1000,-1000))
  dfplot = rbind(dfplot, c("dummy","dummy",'TX',"dummy","dummy",-1,-1,-1,FALSE,1,-1000,"dummy",-1000,-1000))
  dfplot = rbind(dfplot, c("dummy","dummy",'TY',"dummy","dummy",-1,-1,-1,FALSE,1,-1000,"dummy",-1000,-1000))
  dfplot = rbind(dfplot, c("dummy","dummy",'TY',"dummy","dummy",-1,-1,-1,FALSE,1,-1000,"dummy",-1000,-1000))
  #dfplot$Session <- factor(dfplot$Session , levels=c(1,2,4,5,3))
  dfplot$Timepoint <- factor(dfplot$Timepoint, levels=c('T1','T2','TX','TY','T3'))
  dfplot$Exp_Group <- factor(dfplot$Exp_Group , levels=c('Manual','AI'))
  dfplot$volVar <- as.numeric(dfplot$volVar)
  str(dfplot)
  
  dodge <- position_dodge(width = 0.5)
  
  ggplot(dfplot) + 
    geom_violin(aes(x=Timepoint,y = volVar,fill=Exp_Group),position = dodge) + 
    labs(x="Time Elapsed Since Training",
         y="Bladder Volume Variance [mL]",
         title="Bladder Volume Variance",
         fill="Experimental Group:") + 
    scale_fill_manual(
      values = c("red", "darkblue"),
      labels=c('Manual', 'AI Assistance')) +
    ylim(-400,400) + 
    scale_x_discrete(labels=c("T1" = "0 Weeks", "T2" = "2 Weeks","TX" = "4 Weeks","TY" = "6 Weeks","T3" = "8 Weeks")) +
    theme(plot.title = element_text(size = 20,hjust = 0.5,face = "bold"),
          axis.title = element_text(size = 18),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black",
                                      fill = NA,
                                      linewidth = 1),
          legend.position="bottom", 
          legend.box = "horizontal",
          panel.grid.major = element_line(color = "grey",
                                          linewidth = 0.25,
                                          linetype = 2),
          axis.text.x = element_text(size=14)) 
  
  ggplot(dfClean) + 
    geom_violin(aes(x=Exp_Group,y = volVar,fill=Exp_Group),position = dodge) + 
    labs(x="Experimental Group",
         y="Bladder Volume Variance [mL]",
         title="Bladder Volume Variance",
         fill="Experimental Group:") + 
    #scale_fill_manual(
      #values = c("red", "darkblue"),
      #labels=c('Manual', 'AI Assistance')) +
    ylim(-400,400) + 
    #scale_x_discrete(labels=c('Manual'='Manual','AI'='AI Assistance')) +
    theme(plot.title = element_text(size = 20,hjust = 0.5,face = "bold"),
          axis.title = element_text(size = 18),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black",
                                      fill = NA,
                                      linewidth = 1),
          legend.position="bottom", 
          legend.box = "horizontal",
          panel.grid.major = element_line(color = "grey",
                                          linewidth = 0.25,
                                          linetype = 2),
          axis.text.x = element_text(size=14)) 
}
