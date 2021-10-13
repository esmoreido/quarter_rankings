library(shiny)
library(ggplot2)
library(bibliometrix)
library(DT)

ui <- fluidPage(
    titlePanel("Подсчет количества статей в журналах различных квартилей SJR"),
    sidebarLayout(
        sidebarPanel(
            fileInput("csvfile", "Выберите файл *.bib", buttonLabel = "Выбрать...", 
                      placeholder = "",                      
                      accept = "bib"
            )
            # tags$hr(),
            # checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            DT::dataTableOutput("table"),
            plotOutput("plot"),
            tableOutput("count")
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
        # print(summary(df))
        return(df)
    })
    
    output$table <- DT::renderDataTable({
        outTable()
        })
    
    output$plot <- renderPlot({
        df <- outTable()
        ggplot(df, aes(x=SJR.Best.Quartile, fill=SJR.Best.Quartile)) +
            geom_histogram(stat = 'count') +
            stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
            labs(title = paste('SCIMAGO JR ', min(df$PY), '-', max(df$PYD)), x='Квартиль', y='Количество',
                 fill='Квартиль') + theme_light(base_size = 20)
    })
    output$count <- renderTable({
        df <- outTable()
        df %>%
            group_by(df$SJR.Best.Quartile) %>%
            summarise(articles = n())
        
    })
}

shinyApp(ui = ui, server = server)
