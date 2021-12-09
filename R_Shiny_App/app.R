library(shiny)
library(plotly)
library(fable)
library(ggpubr)
library(ggplot2)
library(fpp3)
library(bslib)
library(quantmod)
library(regclass)
library(readr)
library(dygraphs)
library(forecast)
library(tsibble)
library(lubridate)

# Start of Shiny App

library(shiny)
ui <- bootstrapPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    titlePanel("Time Series App"),
    
    sidebarLayout(
            sidebarPanel(
                width = 3,
                h5("CSV File"),
                fileInput("file","Upload a File"),
                uiOutput('choice1'),
                uiOutput('choice2'),
                dateRangeInput("daterange1", "Date range:",
                               start = "2001-01-01",
                               end   = "2004-12-01"),
                # dateInput("date", label = ("Start Date")),
                tags$br(),
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ','),
                radioButtons('resample', 'Resample Data',
                             c(Annually = 'Annually',
                               Quarterly = 'Quarterly',
                               Monthly = 'Monthly',
                               Weekly = 'Weekly',
                               Daily = 'Daily'),
                             'Daily'),
                # radioButtons('quote', 'Quote',
                #              c(None='',
                #                'Double Quote'='"',
                #                'Single Quote'="'"),
                #              '"')
                
                
                
                sliderInput("f",
                            "Forecast Length",
                            min = 1,
                            max = 50,
                            value = 12),
                
                
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Instructions", 
                                 h2("Welcome to Time Series App"),
                                 p(" This app allows users to upload a CSV file of their choice, and then plot it as a time series. "),
                                 p("To start, first select a csv file and then select the Key and Index variables. Your csv will be outputted into a data table under the Data tab. Click the Time Series Plot tab to then view the plot.  ")),
                        
                        
                        # tabPanel("File Upload", verbatimTextOutput("file"),
                        #          fileInput("file1", label = h3("Upload a File")), 
                        #          
                        #          selectInput("selectkey", label = h3("Choose the key for the time series"), 
                        #                      choices = names(df), selected = names(df)),
                        #          ),
                        
                        tabPanel("Data", tableOutput('table')),
                        
                        
                        tabPanel("Time Series Plot", plotlyOutput("plot")),
                        
                        
                        tabPanel("Time Series Residuals and Quick Summary", verbatimTextOutput("stats"),
                                 plotlyOutput('resid')),
                        
                        
                        tabPanel("Time Series Resample Plot", plotlyOutput("resample_plot")),
                        
                        
                        tabPanel("Time Series Decomposition", plotlyOutput("Decomposition"),
                                 p("The output of this will print to a popup window.")),
                        
                        
                        tabPanel("Simple Models",radioButtons('simple', "Simple Models",
                                                              c(Mean = 'mean',
                                                                Naive = 'naive',
                                                                SNaive = 'snaive',
                                                                Drift = 'drift'),
                                                              'mean'),
                                 
                                 plotlyOutput("simple_models_plot"),
                                 verbatimTextOutput("simple_models"),
                                 p('Note: This may take a while to run.')),
                        
                        
                        tabPanel("Exponential smoothing", radioButtons('exp_options', "Exponential Smoothing",
                                                                       c(Holt = 'Holt',
                                                                         Holt_Winters = 'Holt Winters'),
                                                                       'Holt'),
                                 plotlyOutput("exp_plot"),
                                 verbatimTextOutput("exp"),
                                 p('Note: This may take a while to run.')),
                        
                        
                        tabPanel("ARIMA", sliderInput("p",
                                                      "ARIMA Sliders P,D,Q",
                                                      min = 0,
                                                      max = 3,
                                                      value = 0),
                                 sliderInput("d",
                                             NULL,
                                             min = 0,
                                             max = 3,
                                             value = 0),
                                 sliderInput("q",
                                             NULL,
                                             min = 0,
                                             max = 3,
                                             value = 0),
                                 checkboxInput('auto', 'Auto ARIMA', FALSE),
                                 plotlyOutput("arima_plot"),
                                 verbatimTextOutput("arima"),
                                 p('Model is loading and may take 2 - 3 minutes. Model will need time to update when a different model / PDQ is selected'))
                        
                        
                        
            ))
        )
    
)



# SERVER
server <- function(input, output,session) {
    
    output$input_file1 <- renderTable({
        file_to_read <- input$file1
        read.table(file_to_read$datapath)
    })
    
    data <- reactive({ 
        req(input$file)
        inFile <- input$file 
        df <- read.csv(inFile$datapath, header = input$header)
        
        
        updateSelectInput(session, inputId = 'xcol', label = 'Key',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Index',
                          choices = names(df), selected = names(df)[2])
        
        # df[['date']] <- as.Date(df[['date']], format = "%m/%d/%Y")
        # 
        # df <- subset(df, date> input$daterange1[1] & date < input$daterange1[2])

        
        return(df)
    })
    
    output$choice1 = renderUI({
        file_to_read <- input$file1
        selectInput('choice1', "Index", names(data()))
    })
    
    output$choice2 = renderUI({
        file_to_read <- input$file1
        selectInput('choice2', 'Key', names(data()))
    })
    
    # Intermediate Data Table to convert to Tsibble and for resample/plotting
    data_int <- reactive({ 
    data_int <- data()
    data_int[['date']] <- as.Date(data_int[[input$choice1]], format = "%m/%d/%Y")
    data_int <-  subset(data_int, date> input$daterange1[1] & date < input$daterange1[2])
    return(data_int)
    })
    
    
    # Create Data Tsibble
    data_tsbl <- reactive({
        tmp <- data_int()
        d_tsbl <- as_tsibble(tmp, index = input$choice1)
       
        
        return(d_tsbl)
    })
    
    
    # Data for resample plot
    data_resample <- reactive({
        tmp <- data_int()
        resamp <- switch(input$resample,
                         Annually = 'year',
                         Quarterly = 'quarter',
                         Monthly = 'month',
                         Weekly = 'weekly',
                         Daily = 'day',
                       'day')
        tmp$resample <- floor_date(tmp$date, resamp)
        
        #find mean sales by month
        df <- tmp %>%
            group_by(resample) %>%
            summarize(mean = mean(value))
        
        return(df)
    })
    
    # Residuals
    rs <- reactive({
        tmp <- data_int()
        tmp$date2 <- decimal_date(tmp$date)
        M1 <- lm(value ~ date2, tmp) 
        tmp$res1 <- M1$residuals
        return(tmp)
    })
    
    
    # Data for simple models
    data_simple_fit <- reactive({
        tmp <- data_tsbl()
        
        train <- tmp %>% stretch_tsibble(.init = 3, .step = 1)
        simp <- input$simple
        
        if(simp == 'mean'){
        train_fit <- train %>% model(MEAN(value))
        }
        else if(simp =='naive'){
            train_fit <- train %>% model(NAIVE(value))
        }
        else if (simp =='snaive'){
            train_fit <- train %>% model(SNAIVE(value))
        }
        else{
            train_fit <- train %>% model(RW(value ~drift()))
        }
        
        return(train_fit)
    })
    
    # Data simple acc
    data_simple_acc <- reactive({
        tmp <- data_tsbl()
        
        train <- tmp %>% stretch_tsibble(.init = 3, .step = 1)
        tmp2 <- data_simple_fit() %>% predict(train)
        return(accuracy(tmp2, tmp ))
    })
    
    
    # Data simple forecast
    data_simple_forecast <- reactive({
        tmp <- data_simple_fit()
        fc <- tmp %>% forecast(h=input$f)
        return(fc)
    })
    
    
    
    # Create Data timeseries for decomposition
    data_time_series <- reactive({
        tmp <- data_int()
        start_date <-strsplit(as.character(input$daterange1[1]), '-', fixed=T)[[1]] 
        df.ts = ts(tmp[, -1], frequency = 12, start=c(as.integer(start_date[1]), 
                                                     as.integer(start_date[2]),
                                                     as.integer(start_date[3])))
        
        return(df.ts)
    })
    
    
    # Create Holt data
    data_holt <- reactive({
        tmp <- data_tsbl()
        
        if(input$exp_options == 'Holt'){
            fit <- tmp %>% model(ANN = ETS(value ~ error('A')+ trend("A") + season("N")
            ))
        }
        else{
            fit <- tmp %>% model(additive = ETS(value ~ error('A')+ trend("A") + season("A")),
                    multiplicative = ETS(value ~ error('M') +  trend("A") + season("M"))
            )
        }
        
        return(fit)
    })
        
    # Create Holt forecast
    holt_fc <- reactive({
        tmp <- data_holt()
        fc <- tmp %>% forecast(h=input$f)
        return(fc)
    })
    
    # Create Holt acc
    holt_acc <- reactive({
        tmp <- data_tsbl()
        
        tmp2 <- data_holt() %>% predict(tmp)
        return(accuracy(tmp2, tmp ))
    })
    
    # Create ARIMA
    arima_l <- reactive({
        tmp <- data_tsbl()
        if(input$auto == FALSE){
            fit <- tmp %>% model(ARIMA(value ~ pdq(
                input$p, input$d, input$q)))
        }
        else{
            fit <- tmp %>% auto.arima()
        }
        return(fit)
    })
    
    # Create Arima forecast
    arima_fc <- reactive({
        tmp <- arima_l()
        fc <- tmp %>% forecast(h=input$f)
        return(fc)
    })
    
    # Create Arima acc
    arima_acc <- reactive({
        tmp <- data_tsbl()
        
        tmp2 <- arima_l() %>% predict(tmp)
        return(accuracy(tmp2, tmp ))
    })
    
    
    reactive({ 
        req(input$file1)
        title <- names(data())
        return(title)
    })
    
    reactive({ 
        req(input$file1)
        names(code) <- names(data())
        return(code)
    })
    
    reactive({ 
        req(input$file1)
        title <- names(title())
        return(title)
    })
    
    df.subset2 <- reactive({if(1==1){
        a <- subset(df)
        return(a)
    }else{
        a <- subset(df)
        return(a)
    }
    })
    
    df.subset <- df.subset2
    
    # Variable Selection
    
    x <- reactive({
        data()[,input$choice1]
    })
    
    y <- reactive({
        data()[,input$choice2]
    })
    
    
    # Data output
    output$table = renderTable({
         data()
        
       # data_tsbl()
    })
    
    # Quick Summary output
    output$stats <- renderPrint({
        summary(data_int()[input$choice2])
    })
    
    output$plot <-
        renderPlotly(ggplot(data_int() , aes(x = data_int()[,input$choice1],
                        y = data_int()[,input$choice2]) )
            +geom_point() +geom_line()
            +geom_smooth(method = lm)
            + labs(x=input$choice1, y= input$choice2)
    )
    
    # Resample plot
    output$resample_plot <- renderPlotly(
        ggplot(data_resample() , aes(x = data_resample()$resample, 
                                     y = data_resample()$mean)) 
        + geom_point() + geom_line() + labs(x=input$choice1, y= input$choice2)
        + ggtitle(input$resample)
        
    )
    
    output$Decomposition <- 
        renderPlotly(plot(decompose(data_time_series())))
    
    # output$decomp <- 
    #     renderPlotly(gg_season())
    
    # Simple model accuracy
    output$simple_models <- renderPrint({
        data_simple_acc()
    })
    
    # simple model forecast
    output$simple_models_plot <- renderPlotly({
        ggplot() +geom_point(data = data_simple_forecast(), aes(x= date, y = data_simple_forecast()$.mean, color = 'red'))+ 
            geom_line(data = data_simple_forecast(), aes(x= date, y = data_simple_forecast()$.mean, color = 'red'))+
            geom_point(data = data_int() , aes(x = data_int()[,input$choice1],
                                                  y = data_int()[,input$choice2]))+
            labs(x=input$choice1, y= input$choice2)   +
            geom_line(data = data_int() , aes(x = data_int()[,input$choice1],
                                           y = data_int()[,input$choice2]))  +
            theme (legend.position = "none") + ggtitle(input$simple)
    })
    
    # Create holt outputs
    output$exp <- renderPrint({
        holt_acc()
    })
    
    # Create holt plot
    output$exp_plot <- renderPlotly({
        ggplot() +geom_point(data = holt_fc(), aes(x= date, y =holt_fc()$.mean, color = 'red'))+ 
            geom_line(data = holt_fc(), aes(x= date, y = holt_fc()$.mean, color = 'red'))+
            geom_point(data = data_int() , aes(x = data_int()[,input$choice1],
                                               y = data_int()[,input$choice2]))+
            labs(x=input$choice1, y= input$choice2)   +
            geom_line(data = data_int() , aes(x = data_int()[,input$choice1],
                                              y = data_int()[,input$choice2]))  +
            theme (legend.position = "none") + ggtitle(input$exp_options)
    })
    
    # Creat arima outputs
    output$arima <-  renderPrint({
        arima_acc()
    })
    
    # Create arima plot
    output$arima_plot <- renderPlotly({
        ggplot() +geom_point(data = arima_fc(), aes(x= date, y =arima_fc()$.mean, color = 'red'))+ 
            geom_line(data = arima_fc(), aes(x= date, y = arima_fc()$.mean, color = 'red'))+
            geom_point(data = data_int() , aes(x = data_int()[,input$choice1],
                                               y = data_int()[,input$choice2]))+
            labs(x=input$choice1, y= input$choice2)   +
            geom_line(data = data_int() , aes(x = data_int()[,input$choice1],
                                              y = data_int()[,input$choice2]))  +
            theme (legend.position = "none") + ggtitle(paste('Arima: (',
                                                             input$p, input$d, input$q, ')'))
    })
    
    # Resid output
    output$resid <- renderPlotly({
        ggplot(rs(), aes( x = rs()$date2, y = rs()$res1)) + geom_point() +
            labs(x=input$choice1, y= input$choice2) +
            ggtitle('Time Series Linear Fit Residuals')
    })
        
}

shinyApp(ui = ui, server = server)

