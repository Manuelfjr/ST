#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shinydashboard')
library(shiny)
library(shinydashboard)
library(geobr)
library(dplyr)
library(ggplot2)
#'%!in%' <- Negate('%in%')
#shape_UF <- read_state(code_state='all')

#length(df)

#set_host <- function(){
#    options(shiny.host = '0.0.0.0')
#    options(shiny.port = 8888)
#}
#try(
#    set_host()
#)
# new_cases_data

# TRabalho
#x = dim(new_cases_data_all)
#for (i in 1:(length(new_cases_data_all$Brasil))){
#    x[i] = as.integer(new_cases_data_all$Brasil[i])
#}
#y.c = ts(x[seq(1,length(x) - 100, 1)])
#model = auto.arima( y.c , ic = 'aic', d = 0, stationary=T, seasonal = F)
#model

#tsdiag(model)

new_cases_data_all['Pernambuco']

new_cases_data_all = read.csv('https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/transp-confirmed-new.csv', header = F)
new_deaths_data_all = read.csv('https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/transp-deaths-new.csv', header = F)

colnames(new_cases_data_all) = c(new_cases_data_all[1,]);colnames(new_cases_data_all)[1] = 'Data'
colnames(new_deaths_data_all) = c(new_deaths_data_all[1,]);colnames(new_deaths_data_all)[1] = 'Data'

new_cases_data_all = new_cases_data_all[-c(1,2),]
new_deaths_data_all = new_deaths_data_all[-c(1,2),]

locais=c('Todos',colnames(new_deaths_data_all)[-1])

date = data.frame(date = new_cases_data_all['Data'][seq(dim(new_cases_data_all)[1],1),])

state = colnames(new_cases_data_all) 
###########
# df = read.csv('https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/transp-confirmed-new.csv', header = F)
# df <- df[-2,]; df[1,1] = 'Estado'
# rownames(df) <- df[,1]; df <- df[,-1]
# 
# #View(df)
# df = df[df['Estado',] %!in% c('Data','Brasil','Norte','Nordeste','Sul','Sudeste','Centro-Oeste')]
# total_casos = colSums(apply(df[2:dim(df)[1],], 2, as.numeric))
# df_cases = data.frame('Casos' = total_casos, 'Estado' = state[state %!in% c('Data','Brasil','Norte','Nordeste','Sul','Sudeste','Centro-Oeste')])
# rownames(df_cases) = seq(1,dim(df_cases)[1])
# 
# shape_UF
# #cases <- df_cases %>%
#     #select(Estado, Casos)
# 
# mapa_UF <- 
#     dplyr::left_join(
#         x=shape_UF,
#         y=df_cases,
#         by=c('name_state' = 'Estado')
#     )
# 
# ggplot(shape_UF) + 
#     geom_sf(aes(fill=code_state))
# 
###########

ui <- dashboardPage(
    title = 'Dashboards',
    skin = 'purple',
    header=dashboardHeader(
        #icon('graduation-cap'),
        title='Séries Temporais'
    ),
    sidebar=dashboardSidebar(
        sidebarMenu(
            menuItem(
                'Home',
                icon=icon('chart-bar'),
                tabName = "aba1"
            ),
            menuItem(
                'Dados',
                icon=icon('database'),
                tabName = "aba2"
            )
        )
    ),
    body=dashboardBody(
        tabItems(
            tabItem(tabName = 'aba1',
                    h1('Brasil'),
                    fluidRow(
                        infoBoxOutput(outputId = 'ncasos', width = 4),
                        infoBoxOutput(outputId = 'nmortes', width = 4),
                        infoBoxOutput(outputId = 'taxa', width = 4)
                    ),
                    fluidPage( 
                        pageWithSidebar(
                            headerPanel('COVID-19'),
                            sidebarPanel(
                                selectInput('local', 'Estado/região inicial', locais[-1], selected='Brasil'),
                                selectInput('x', 'Data inicial', date, selected=date[dim(date)[1],]),
                                selectInput('y', 'Data final', date, selected = date[1,]),
                                selectInput('tp', 'Casos ou Mortes', c('Casos', 'Mortes'), selected = 'casos')
                                #tabItem(tabName = 'aba1')
                            ),
                            box(
                                width = 8,
                                title = 'Série temporal',
                                status='primary',
                                footer = 'Fonte: Ministério da Saúde official announcements',
                                solidHeader = T,
                                collapsible=T,
                                collapsed=F,
                                plotOutput('plot1')
                            )
                        ),
                        box(
                            width=12,
                            title = 'Gráficos de autocorrelações',
                            footer = 'Fonte: Ministério da Saúde official announcements',
                            status='primary',
                            solidHeader = T,
                            collapsible=T,
                            collapsed=F,
                            plotOutput(
                                'acf'
                            )
                        )
                    )
            ),
            tabItem(tabName = 'aba2',
                    h1('Dados'),
                    fluidPage(
                        column(6,
                            selectInput('local2', 'Estado/região inicial', locais, selected='Todos'),
                            selectInput('x2', 'Data inicial', date, selected = date[dim(date)[1],])),
                        column(6,
                            selectInput('tp2', 'Casos ou Mortes', c('Casos', 'Mortes'), selected = 'Casos'),
                            selectInput('y2', 'Data final', date, selected = date[1,])),
                        box(width=13,
                            footer = 'Fonte: Ministério da Saúde official announcements',
                            dataTableOutput('summary')
                        )
                    ),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    selectedData <- reactive({
        #print(seq(xinit, yrelease))
        if (input$tp == 'Casos'){

            df = data.frame(Data = date[seq(which(input$y == date), 
                                                which(input$x == date)),])
            
            df[input$local] = data.frame(new_cases_data_all[seq(dim(new_cases_data_all)[1], 1),][seq(which(input$y == date), 
                                                                    which(input$x == date)), ][input$local])#input$local])
            
            as.numeric(df[,input$local])

        }else{
            df = data.frame(Data = date[seq(which(input$y == date), 
                                                which(input$x == date)),])
            df[input$local] = data.frame(new_deaths_data_all[seq(dim(new_deaths_data_all)[1], 1),][seq(which(input$y == date), 
                                                                     which(input$x == date)), ][input$local])#input$local])
            
            as.numeric(df[,input$local])
        }
    })
    selectedData2 <- reactive({
        if (input$local2 == 'Todos'){
            if (input$tp2 == 'Casos'){
                date2 = new_cases_data_all['Data']
                new_cases_data_all[seq(which(input$y2 == date2), 
                                       which(input$x2 == date2)), ][seq(11)]#input$local])
            }else{
                date2 = new_cases_data_all['Data']
                new_deaths_data_all[seq(which(input$y2 == date2), 
                                        which(input$x2 == date2)), ][seq(11)]#input$local])
                
            }
        }else{
            if (input$tp2 == 'Casos'){
                df = data.frame(Data = date[seq(which(input$y2 == date), 
                                                which(input$x2 == date)),])
                df[input$local2] = data.frame(new_cases_data_all[seq(which(input$x2 == date), 
                                                                    which(input$y2 == date)), ][input$local2])#input$local])
                df
            }else{
                df = data.frame(Data = date[seq(which(input$y2 == date), 
                                                which(input$x2 == date)),])
                df[input$local2] = data.frame(new_deaths_data_all[seq(which(input$x2 == date), 
                                                                     which(input$y2 == date)), ][input$local2])#input$local])
                df
            }
        }
    })
    
    output$ncasos <- renderInfoBox(
        infoBox(
            title = 'Casos confirmados',
            value = sum(as.integer(new_cases_data_all[,input$local])[which(new_cases_data_all$Data == input$x):which(new_cases_data_all$Data == input$y)]),
            #subtitle = 'testando',
            icon = icon('viruses'),
            color= 'green'
        )
    )
    output$nmortes <- renderInfoBox(
        infoBox(
            title = 'Mortes confirmados',
            value = sum(as.integer(new_deaths_data_all[,input$local])[which(new_deaths_data_all$Data == input$x):which(new_deaths_data_all$Data == input$y)]),
            #subtitle = 'testando',
            icon = icon('skull-crossbones'),
            color= 'red'
        )
    )
    output$taxa <- renderInfoBox(
        infoBox(
            title = 'Taxa de letalidade',
            value=paste(round(sum(as.integer(new_deaths_data_all[,input$local])[which(new_deaths_data_all$Data == input$x):which(new_deaths_data_all$Data == input$y)])/sum(as.integer(new_cases_data_all[,input$local])[which(new_cases_data_all$Data == input$x):which(new_cases_data_all$Data == input$y)]),4)*100, '%'),
            #subtitle = 'testando',
            icon = icon('percent'),
            color= 'black'
        )
    )
    
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
        
        plot.ts(selectedData()[length(selectedData()):1],
                pch = 20, cex = 3, type = 'l',
                xlab='Tempo', ylab=input$tp);grid()
        #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    output$acf <- renderPlot({
        par(mfrow=c(1,2))
        acf(selectedData(),main='autocorrelação',
            xlab='defasagem', ylab='autocorrelacoes');grid()
        pacf(selectedData(),main='autocorrelação parcial',
             xlab='defasagem', ylab='autocorrelacoes');grid()
    })
    
    output$summary <- renderDataTable({
        selectedData2()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
