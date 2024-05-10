library(ggplot2)
library(ggpmisc)
library(plyr)

setwd("~/Desktop/Bangla")

#averages <- read.csv('output.csv', header=FALSE)
averages <- read.csv('output_Tark2.csv', header=FALSE)

tc <- read.csv('output_TC_Tark2.csv', header=FALSE)

colnames(averages) <- c('Subject', 'Trial', 'CondPrefix', 'FileName', 'WordFreq', 'StemFreq', 'ItemNo', 'Condition', 'Word', 'dSPM')
colnames(tc) <- c('Times', 'Subject', 'Trial', 'CondPrefix', 'FileName', 'WordFreq', 'StemFreq', 'ItemNo', 'Condition', 'Word', 'dSPM')

averages <- averages[averages$Condition != 'SemViol' & averages$Condition != 'CatViol',]
averages <- averages[averages$FileName != 'dSPM-rh.stc',]

#tc <- tc[tc$Condition != 'SemViol' & tc$Condition != 'CatViol',]
#tc <- tc[tc$FileName != 'dSPM-rh.stc',]

averages_byItem <- ddply(averages, .(ItemNo, StemFreq, WordFreq), function(df)
                         {mean(df$dSPM)})

averages_tc_byItem <- ddply(tc, .(ItemNo, Times, StemFreq, WordFreq), function(df) {mean(df$dSPM)})

ggplot(averages_byItem, aes(x = log(StemFreq), y = V1)) + geom_point(alpha=0.2) + theme_minimal() + stat_poly_line() + stat_poly_eq(use_label(c("adj.R2", "p")))
ggplot(averages_byItem, aes(x = log(WordFreq)/log(StemFreq), y = V1)) + geom_point(alpha=0.2) + theme_minimal() + stat_poly_line() + stat_poly_eq(use_label(c("adj.R2", "p")))

corTC <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(corTC) <- c('Times', 'tValue')

for(time in unique(averages_tc_byItem$Times)){
  tValue <- cor.test(log(averages_tc_byItem[averages_tc_byItem$Times == time,]$WordFreq)/log(averages_tc_byItem[averages_tc_byItem$Times == time,]$StemFreq), 
                     averages_tc_byItem[averages_tc_byItem$Times == time,]$V1)$statistic
    corTC[nrow(corTC) + 1,] <- c(time, tValue)
}

#corTC = corTC[0,]

corTC$Times <- unique(averages_tc_byItem$Times)

cor.test(log(averages_tc_byItem[averages_tc_byItem$Times == -0.100,]$WordFreq)/log(averages_tc_byItem[averages_tc_byItem$Times == -0.100,]$StemFreq), 
    averages_tc_byItem[averages_tc_byItem$Times == -0.100,]$V1)$statistic

#ggplot(averages, aes(x = log(WordFreq)/log(StemFreq), y = dSPM)) + geom_point(alpha=0.2) + theme_minimal() + stat_poly_line() + stat_poly_eq(use_label(c("adj.R2", "p")))

ggplot(corTC, aes(x = Times, y = tValue)) + geom_line() + theme_minimal() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
