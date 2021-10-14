library(shiny)
library(ggplot2)
library(bibliometrix)
library(DT)
library(dplyr)

ui <- fluidPage(
    titlePanel("Подсчет количества статей в журналах различных квартилей SJR"),
    fluidRow(
        column(4,
               fileInput("csvfile", 
                         "Выберите файл *.bib", 
                         buttonLabel = "Выбрать...", 
                                           placeholder = "",
                                           accept = "bib"
                                 )
               ),
        column(8,
               br(),
               "Для загрузки используйте файлы, полученные 
               экспортом интересующих записей из поиска Scopus
               в формате BibTeX.")
    ),
    fluidRow(
        column(6,
               plotOutput("plot")
                    ),
        column(6, 
               tableOutput("count")
               )
        ),
    fluidRow(
        column(12,
               DT::dataTableOutput("table")     
               )
    )
)

server <- function(input, output, session) {
    sjr <- read.csv(file = 'scimagojr 2020.csv', sep = ';', na.strings = '-')
    sjr$Title <- toupper(sjr$Title)
    
    outTable <- reactive({
        inFile <- input$csvfile
        
        if (is.null(inFile))
            return(NULL)
        
        rec <- bibliometrix::convert2df(inFile$datapath, dbsource = "scopus", format = "bibtex")
        rownames(rec) <- NULL
        df <- merge(rec[,c(1, 4, 5, 10, 13)], sjr[, c(3, 7)], by.x ='SO', by.y = 'Title', all.x = T)
        df <- df[order(df$PY, decreasing = T),]
        rownames(df) <- NULL
        print(colnames(df))
        colnames(df) <- c('Журнал', 'Авторы', 'DOI', 'Название', 'Год', 'Квартиль')
        # print(summary(df))
        return(df)
    })
    
    output$table <- DT::renderDataTable(
        outTable(),
        options = list(
            language = list(url = "https://cdn.datatables.net/plug-ins/1.11.3/i18n/ru.json")
        )
        )
    
    output$plot <- renderPlot({
        df <- outTable()
        ggplot(df, aes(x=`Квартиль`, fill=`Квартиль`)) +
            geom_histogram(stat = 'count') +
            stat_count(aes(y=..count..,label=..count..),geom="text",vjust=1) +
            labs(title = paste('SCIMAGO JR ', min(df$`Год`, na.rm = T), '-', 
                               max(df$`Год`, na.rm = T)), x='Квартиль', y='Количество статей',
                 fill='Квартиль') + theme_void(base_size = 20)
    })
    output$count <- renderTable({
        df <- outTable()
        df %>%
            dplyr::group_by(df$`Квартиль`) %>%
            dplyr::summarise(articles = dplyr::n()) %>%
            `colnames<-`(c('Квартиль', 'Количество статей'))
        
    })
}

shinyApp(ui = ui, server = server)
