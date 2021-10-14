Sys.setlocale("LC_ALL","Russian")
library(ggplot2)
library(dplyr)
library(bibliometrix)
setwd('C:/Users/morey/Documents/R_Projects/quarter_rankings/')

sjr <- read.csv(file = 'd:/YandexDisk/ИВПРАН/РНФ/заявки/2021/63/sjr/scimagojr 2020.csv', sep = ';', na.strings = '-')
sjr$Title <- toupper(sjr$Title)

jcr <- read.csv(file = 'wos-jcr 2021-June-30.csv')  
rec <- read.csv('tananaev.csv', fileEncoding = 'UTF-8', fill = F, allowEscapes = F,
                header = TRUE, stringsAsFactors = FALSE, check.names = F)

rec <- bibliometrix::convert2df(file = 'd:/YandexDisk/ИВПРАН/РНФ/заявки/2021/63/sjr/moreydo.bib', dbsource = "scopus", format = "bibtex")
rownames(rec) <- NULL
df <- rec %>% 
  dplyr::select(SO, AU, PY, DI, TI) %>%
  dplyr::left_join(y = sjr, by = c("SO"="Title")) %>%
  dplyr::select(PY,AU,TI,SO,DI,SJR.Best.Quartile) %>%
  `colnames<-`(c('Год','Авторы','Название','Журнал','DOI','Квартиль'))  %>%
  dplyr::arrange(desc(`Год`))

df <- merge(rec[,c(1, 4, 5, 10, 13)], sjr[, c(3, 7)], by.x ='SO', by.y = 'Title', all.x = T)


sum(df$SJR.Best.Quartile == 'Q1', na.rm = T)

df %>%
  group_by(df$SJR.Best.Quartile) %>%
  summarise(articles = n())

ggplot(df, aes(x=SJR.Best.Quartile, fill=SJR.Best.Quartile)) + 
  geom_histogram(stat = 'count') + 
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
  labs(title = paste('SCIMAGO JR ', min(rec$PY), '-', max(rec$PY)), x='Квартиль', y='Количество',
       fill='Квартиль') 

sud <- read.csv('sudheer2016-2021.csv', fileEncoding = 'UTF-8', 
                header = TRUE, stringsAsFactors = FALSE, check.names = F)

df1 <- merge(sud, sjr[, c(3, 7)], by.x ='Название источника', by.y = 'Title')

sum(df1$SJR.Best.Quartile == 'Q1')
