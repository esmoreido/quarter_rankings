Sys.setlocale("LC_ALL","Russian")
library(ggplot2)
setwd('C:/Users/morey/Documents/R_Projects/quarter_rankings/')

sjr <- read.csv(file = 'scimagojr 2020.csv', sep = ';', na.strings = '-')
jcr <- read.csv(file = 'wos-jcr 2021-June-30.csv')  
rec <- read.csv('gelfan.txt', fileEncoding = 'UTF-8', fill = F, allowEscapes = F,
                header = TRUE, stringsAsFactors = FALSE, check.names = F)
library(RefManageR)
rec <- ReadBib('gelfan.bib', .Encoding = "UTF-8", check = F)
results.list <- lapply(strsplit(readLines(con = "gelfan.txt", encoding = "UTF-8"), 
                                split = ",", fixed = T), as.character)
df <- do.call(rbind, results.list[-1])


df <- merge(rec[,c(1, 3, 4, 5, 13)], sjr[, c(3, 7)], by.x ='Название источника', by.y = 'Title')


sum(df$SJR.Best.Quartile == 'Q1', na.rm = T)
library(dplyr)
df %>%
  group_by(df$SJR.Best.Quartile) %>%
  summarise(articles = n())

ggplot(df, aes(x=SJR.Best.Quartile, fill=SJR.Best.Quartile)) + 
  geom_histogram(stat = 'count') + 
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
  labs(title = paste('SCIMAGO JR ', min(rec$Год), '-', max(rec$Год)), x='Квартиль', y='Количество',
       fill='Квартиль') 

sud <- read.csv('sudheer2016-2021.csv', fileEncoding = 'UTF-8', 
                header = TRUE, stringsAsFactors = FALSE, check.names = F)

df1 <- merge(sud, sjr[, c(3, 7)], by.x ='Название источника', by.y = 'Title')

sum(df1$SJR.Best.Quartile == 'Q1')
