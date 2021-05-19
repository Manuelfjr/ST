#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
new_cases_data = read.csv('https://raw.githubusercontent.com/Manuelfjr/ST/main/R/project/R/data/confirmed/data_brasil_new_confirmed.csv')
new_deaths_data = read.csv('https://raw.githubusercontent.com/Manuelfjr/ST/main/R/project/R/data/deaths/data_brasil_new_deaths.csv')

new_cases_data = new_cases_data[-1,]; colnames(new_cases_data) = c('date', 'BR')
new_deaths_data = new_deaths_data[-1,]; colnames(new_deaths_data) = c('date', 'BR')
date = new_cases_data['date']

#vars <- setdiff(names(iris), "Species")
ui <- fluidPage(
    pageWithSidebar(
        headerPanel('Série temporal dos novos casos de Covid-19 no Brasil'),
        sidebarPanel(
            selectInput('x', 'Data inicial', date),
            selectInput('y', 'Data final', date, selected = date[100,]),
            selectInput('tp', 'Casos ou Mortes', c('Casos', 'Mortes'), selected = 'casos')#,
            #numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
        ),
        mainPanel(
            plotOutput('plot1')
        )
    ),
    plotOutput(
        'acf'
    )
)
server <- function(input, output, session) {
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        #print(seq(xinit, yrelease))
        if (input$tp == 'Casos'){
            as.integer(new_cases_data[seq(which(input$x == date), 
                                          which(input$y == date)),]$BR)
        }else{
            as.integer(new_deaths_data[seq(which(input$x == date), 
                                          which(input$y == date)),]$BR)
        }
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        #palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
        #          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        #print('TESTANDO')
        xinit=paste(strsplit(input$x, '/')[[1]][3], 
                    strsplit(input$x, '/')[[1]][2],
                    strsplit(input$x, '/')[[1]][1],sep='-')
        yinit=paste(strsplit(input$y, '/')[[1]][3], 
                    strsplit(input$y, '/')[[1]][2],
                    strsplit(input$y, '/')[[1]][1],sep='-')
        
        d <- seq(as.Date(xinit), as.Date(yinit), "day")
        months <- seq(min(d), max(d), "month")
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot.ts(selectedData(),
                pch = 20, cex = 3, type = 'l',
                xlab='Tempo', ylab=input$tp);grid()
        #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$acf <- renderPlot({
        par(mfrow=c(1,2))
        acf(selectedData(),main='Gráfico de autocorrelação',
            xlab='defasagem', ylab='autocorrelacoes');grid()
        pacf(selectedData(),main='Gráfico de autocorrelação parcial',
             xlab='defasagem', ylab='autocorrelacoes');grid()
    })
    
}



shinyApp(ui, server)
