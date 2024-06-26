Sys.setlocale("LC_ALL","Russian")
library(shiny)
library(ggplot2)
library(bib2df)
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
    sjr <- read.csv(file = 'scimagojr 2022.csv', sep = ';', na.strings = '-')
    # sjr$Title <- toupper(sjr$Title)
    sjr$EIssn <- gsub(pattern = ',.*', replacement = '', x = sjr$Issn)
    
    outTable <- reactive({
        req(input$bibfile)
        validate(need(tools::file_ext(input$bibfile$datapath) == c("bib"), 
                    "Пожалуйста, загрузите файл в формате *.bib"))
        inFile <- input$bibfile$datapath
        # inFile <- 'd:/YandexDisk/ИВПРАН/минобр/конкурсы/2024/заявка/публикации/соломатин/Scopus Solomatine ALL-v02.bib'
        # print(inFile)
        rec <- bib2df(inFile)
        
        if("ISSN" %in% names(rec)){
          
          rec$ISSN <- gsub(pattern = '-', replacement = '', x = rec$ISSN)
          if("EISSN" %in% names(rec)){
            rec$EISSN <- gsub(pattern = '-', replacement = '', x = rec$EISSN)
          }
          
          rec$Quartile <- ''
          for(i in 1:nrow(rec)){
            # print(i)
            ss <- sjr %>%
              filter(grepl(rec[i,]$ISSN, Issn, fixed = TRUE))
            if(nrow(ss) == 0){
              ss <- sjr %>%
                filter(grepl(rec[i,]$EISSN, Issn, fixed = TRUE))
            }
            
            # print(ss$SJR.Best.Quartile)
            rec[i,]$Quartile <- ss$SJR.Best.Quartile
          }
        }else{
          rec <- rec %>%
            dplyr::select(YEAR,AUTHOR,TITLE,JOURNAL,DOI) %>%
            left_join(sjr, by = c('JOURNAL'='Title')) %>%
            rename('Quartile' = 'SJR.Best.Quartile')
        }
        
        rec %>%
          dplyr::select(YEAR,AUTHOR,TITLE,JOURNAL,DOI,Quartile) %>%
          `colnames<-`(c('Год','Авторы','Название','Журнал','DOI','Квартиль'))  %>%
          dplyr::arrange(desc(`Год`))
    })
    
    output$years <- renderUI({
      req(outTable())
      sliderInput(inputId = "take_years", 
                  min = min(min(outTable()$`Год`, na.rm = T), year(Sys.Date()) - 5), 
                  max = max(max(outTable()$`Год`, na.rm = T), year(Sys.Date())), 
                  label = "Годы для учета", 
                  value = c(min(outTable()$`Год`, na.rm = T), max(outTable()$`Год`, na.rm = T)), step = 1, sep = ""
      )
    })
    
    filtered <- reactive({
      req(outTable())
      df <- outTable() %>%
          dplyr::filter(between(`Год`,input$take_years[1],input$take_years[2]))
        # dplyr::filter(`Год` %in% seq(from = input$take_years[1], to = input$take_years[2], by = 1))
    })
    
    output$table <- DT::renderDataTable(
        filtered(),
        options = list(
            language = list(url = "https://cdn.datatables.net/plug-ins/1.11.3/i18n/ru.json")
        )    
        )
    
    output$plot <- renderPlot({
        df <- filtered()
        ggplot(df, aes(x=`Квартиль`, fill=`Квартиль`)) +
            geom_histogram(stat = 'count') +
            stat_count(aes(y=..count..,label=..count..),geom="text",vjust=1) +
            labs(title = paste('SCIMAGO JR ', min(df$`Год`, na.rm = T), '-', 
                               max(df$`Год`, na.rm = T)), 
                 x='Квартиль', y='Количество статей',
                 fill='Квартиль') + theme_minimal(base_size = 20)
    })
    
    output$count <- renderTable({
        print(input$take_years)
        df <- filtered()
        df %>%
            dplyr::group_by(df$`Квартиль`) %>%
            dplyr::summarise(articles = dplyr::n()) %>%
            `colnames<-`(c('Квартиль', 'Количество статей'))
        
    })
}

shinyApp(ui = ui, server = server)
