library(shiny)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Подсчет количества статей в журналах различных квартилей SJR"),
    sidebarLayout(
        sidebarPanel(
            fileInput("csvfile", "Выберите файл *.csv", buttonLabel = "Выбрать...", 
                      placeholder = "",                      
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            )
            # tags$hr(),
            # checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tableOutput("table"),
            plotOutput("plot"),
            tableOutput("count")
        )
    )
)

server <- function(input, output, session) {
    sjr <- read.csv(file = 'scimagojr 2020.csv', sep = ';', na.strings = '-')
    outTable <- reactive({
        inFile <- input$csvfile
        
        if (is.null(inFile))
            return(NULL)
        
        rec <- read.csv(inFile$datapath, header = T, fileEncoding = 'UTF-8', 
                             stringsAsFactors = FALSE, check.names = F)
        df <- merge(rec[,c(1, 3, 4, 5, 13)], sjr[, c(3, 7)], by.x ='Название источника', by.y = 'Title')
        df <- df[order(df$Год, decreasing = T),]
        # print(summary(df))
        return(df)
    })
    
    output$table <- renderTable({
        head(outTable())
        })
    
    output$plot <- renderPlot({
        df <- outTable()
        ggplot(df, aes(x=SJR.Best.Quartile, fill=SJR.Best.Quartile)) +
            geom_histogram(stat = 'count') +
            stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
            labs(title = paste('SCIMAGO JR ', min(df$Год), '-', max(df$Год)), x='Квартиль', y='Количество',
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
