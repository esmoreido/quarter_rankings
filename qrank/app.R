library(shiny)
library(ggplot2)
library(bibliometrix)
library(DT)
library(dplyr)
library(lubridate)

ui <- fluidPage(
    titlePanel("Подсчет количества статей в журналах различных квартилей SJR"),
    fluidRow(
        column(4,
               fileInput("bibfile", 
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
               uiOutput("years"),
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
        req(input$bibfile)
        inFile <- input$bibfile
        rec <- bibliometrix::convert2df(inFile$datapath, dbsource = "scopus", format = "bibtex")
        rownames(rec) <- NULL
        rec %>% 
          dplyr::select(SO, AU, PY, DI, TI) %>%
          dplyr::left_join(y = sjr, by = c("SO"="Title")) %>%
          dplyr::select(PY,AU,TI,SO,DI,SJR.Best.Quartile) %>%
          `colnames<-`(c('Год','Авторы','Название','Журнал','DOI','Квартиль'))  %>%
          dplyr::arrange(desc(`Год`))
    })
    
    output$years <- renderUI({
      sliderInput(inputId = "take_years", 
                  min = min(outTable()$`Год`, na.rm = T), 
                  max = max(outTable()$`Год`, na.rm = T), 
                  label = "Годы для учета", 
                  value = c(1950, year(Sys.Date())), step = 1, sep = ""
      )
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
                               max(df$`Год`, na.rm = T)), 
                 x='Квартиль', y='Количество статей',
                 fill='Квартиль') + theme_minimal(base_size = 20)
    })
    
    output$count <- renderTable({
      if (is.null(outTable))
        return(NULL)
        df <- outTable()
        df %>%
            dplyr::group_by(df$`Квартиль`) %>%
            dplyr::summarise(articles = dplyr::n()) %>%
            `colnames<-`(c('Квартиль', 'Количество статей'))
        
    })
}

shinyApp(ui = ui, server = server)
