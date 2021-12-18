## exploreR Application ##
## Author: Sachit Mahajan ##
#Libraries# 
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(data.table)
library(DT)
library(plotly)
library(tsoutliers)
library(TSA)
library(lmtest)
library(astsa)
library(imputeTS)
library(adamethods)
library(autoencoder)
library(shinythemes)
library(ggcorrplot)
library(stringr)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(tidyr)
library(plyr)
library(Metrics)
library(xgboost)
library(randomForest)
library(neuralnet)
library(nnet)
library(caret)
library(xts)
library(lubridate)
library(zoo)
###
set.seed(143)
#User Interface
ui <- dashboardPage(
  dashboardHeader(title = "exploreR"),
  dashboardSidebar(
    #Creat sidebar Menus.
    sidebarMenu(
      menuItem("Data Input", tabName = "Data", icon = icon("dashboard")),
      menuItem("Anomalies and Outliers", tabName = "Outlier", icon = icon("broom")),
      menuItem("Missing Data Analysis", tabName = "nas", icon = icon("clipboard-check")),
      menuItem("Correlation Analysis", tabName = "corr", icon = icon("retweet", lib = "glyphicon")),
      menuItem("Box Plot", tabName = "box", icon = icon("folder-close", lib = "glyphicon")),
      menuItem("Histogram", tabName = "hist", icon = icon("sort-by-attributes-alt", lib = "glyphicon")),
      menuItem("Data Forecast", tabName = "forecast", icon = icon("cog", lib = "glyphicon")),
      menuItem("Data Aggregation", tabName = "aggr", icon = icon("calendar", lib = "glyphicon"))
      #h1(""),
      #tags$img(src='logo.png', height=70, width=180),
    )#SidebarMenu
    
  ),#DashboardSidebar 
  #DashBoard Core
  dashboardBody(
    # Boxes need to be put in a row (or column)
    shinyDashboardThemes(theme = "purple_gradient"),
    tabItems( # Total Items  Grouped items
      tabItem(tabName = "Data",  
              fluidRow(
                useShinyjs(),
                box(title = "File Input",
                    
                    checkboxInput("header", "Header", TRUE),
                    
                    selectInput("sep", "Separator:",
                                c("Comma" = ",",
                                  "Semicolon" = ";",
                                  "Tab" = "\t")
                    ),
                    
                    numericInput("skip", "Skip Rows:", 0, min = 0),
                    
                    numericInput("cn", "Fetch Columns names from row number:", 1, min = 1),
                    textInput("nanstring","NA character:",value = "?"),
                    
                    
                    fileInput("file1", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
                ),
                box(title = "Plot Options",
                    radioButtons("plt", "Plot type:",
                                 c("Scatter Plot" = "markers",
                                   "Line Plot"   = "line")
                    ),
                    uiOutput("Xaxis"),
                    uiOutput("Yaxis"),
                    actionButton("goButton", "Go!")
                )
              ),
              
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  
                  tabPanel("Raw Data", DT::dataTableOutput("contents")),
                  tabPanel("Data Summary",verbatimTextOutput("sum"))
                )
              ),
              
              fluidRow(
                box(title = "Plot",
                    width = NULL,
                    plotlyOutput("plot_intro"))
                
              )
              
      ),#TabItem
  #### -------------------------------------------- Part II Outliers
      tabItem(tabName = "Outlier",
              fluidRow(
                useShinyjs(),
                       box(title = "Outlier Parameters",
                           selectInput("algo_out", "Algorithms:",
                                       c("ARIMA Based" = "Arima",
                                         "KNN" = "knn",
                                         "Autoencoder" = "ANN")),
                           checkboxGroupInput("ArimaP", "Please select one of them:",
                                              choices = c("additive outlier" = "AO",
                                                          "intervention outlier"="IO"),
                                              selected = "AO"),
                           
                           sliderInput("outlierper", "Outlier % to select", 
                                       min = 85, max = 99, value = 90, step= 1),
                           uiOutput("mcolumns"),
                           
                           actionButton("ABotton", "Apply")
                           ),
                box(title = "Plot Options",
                    radioButtons("plt_out", "Plot type:",
                                 c("Scatter Plot" = "markers",
                                   "Line Plot"   = "line")
                                 ),
                    uiOutput("Xaxis_out"),
                    uiOutput("Yaxis_out"),
                    actionButton("goButton2", "Go!")
                    )
                ),
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2",
                  
                  tabPanel("Data", DT::dataTableOutput("Cleaned"))
                ),fluidRow(
                  box(title = "Plot",
                      width = NULL,
                      plotlyOutput("plot_out"))
                  
                )
                
              )
              
      ),#tabName
  ### -----------------------------------------   Part III Fill The NA's
      tabItem(tabName = "nas",
              fluidRow(
                useShinyjs(),
                box(title = "Fill the NA's value",
                    selectInput("algo_na", "Algorithms:",
                                c("Interpolation",
                                  "kalman" = "na.kalman")),
                    
                    uiOutput("mcolumns_nas"),
                    
                    
                    actionButton("ABotton2", "Apply"),
                    downloadButton("downloadData", "Download")
                )
                #box(title = "Save Data into a File",
                #    uiOutput("mcolumns_csv"),
                #    downloadButton("downloadData", "Download")
                #    textInput("name","File's name"),
                #    actionButton("ABotton3", "Save")
                #)
                
              ),#fluidRow
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset3",
                  
                  tabPanel("Data", DT::dataTableOutput("filled"))
                ))
      ),#tabName
 # -------------------------------------------
  tabItem(tabName = "corr",
          fluidRow(
            useShinyjs(),
            box(title = "Select Variables for Correlation Heatmap",
                uiOutput("corrVars"),
                actionButton("goButton3", "Go!")
             )
           ),
          fluidRow(
            box(title = "Correlation Plot",
                width = 6,
                plotlyOutput("plot_corr")
                ))
         ),#tabName
 # ---------------------------------------
  tabItem(tabName = "box",
          fluidRow(
            useShinyjs(),
            box(title = "Select Variables for Box Plot",
                uiOutput("boxVars"),
                width = 3,
                actionButton("goButton4", "Go!")
                ),
            box(title = "Plot Box Plots",
                width = 9,
                height = "100%",
                status = "primary",
                plotlyOutput("plot_box", height = 640)
            ))
  ), #tabName
 
 tabItem(tabName = "hist",
         fluidRow(
           useShinyjs(),
           box(title = "Select Variables for Box Plot",
               uiOutput("histVars"),
               width = 3,
               actionButton("goButton5", "Go!")
           ),
           box(title = "Plot Histograms Plots",
               width = 9,
               height = "100%",
               status = "primary",
               plotlyOutput("plot_hist", height = 640)
           ))
 ), #tabName
 ##-----------------------------------------------------------
 tabItem(tabName = "forecast",
         fluidRow(
           useShinyjs(),
           box(title = "Parameters Selection for Forecasting",
               radioButtons("model","Select Forecasting Model",
                           choices = c("Linear Regression","Random Forest Model","XGboost",
                                       "Neural Network"), 
                           selected = "Linear Regression"),
               sliderInput("trainSize","Select Train Size in %", min = 0, max = 100, value = 80, post  = " %"),
               uiOutput("iVars"),
               uiOutput("dVar"),
               width = 3,
               actionButton("goButton6", "Go!")
           ),
           
           box(title = "RMSE Error",
               width = 3,
               height = "100%",
               status = "primary",
               textOutput("error"),
               #textOutput("select")
             
           ),
           box(title = "Plot Forecasted Plots",
               width = 6,
               height = "100%",
               status = "primary",
               plotlyOutput("plot_predict"),
               #textOutput('predict'),
               #textOutput('actual')
               downloadButton("downloadresult", "Download CSV")
           )
           )
 ),#tabName
  # -------------------------------------
 tabItem(tabName = "aggr",
         fluidRow(
           useShinyjs(),
           box(title = "Parameters For Aggregation",
               radioButtons("aggregation","Select the Aggregation",
                           choices = c("sum","mean"),
                           selected = "sum"),
               uiOutput("aggreg"),
               uiOutput("tVar"),
               uiOutput("sumVar"),
               #selectInput("format","Please insert date format",c("ymd","mdy","dmy"),"ymd"),
               width = 3,
               actionButton("goButton7", "Go!")
           ),
           
           box(title = "Aggregated Data",
               width = 6,
               height = "100%",
               status = "primary",
               DT::dataTableOutput("apply_aggr"),
               downloadButton("downloadagg", "Download CSV")
               
               
           )
         )
 ) #tabName
 
 
)
)
)#DashBoardBody



# R server 
options(shiny.maxRequestSize=1024*1024^2) # to increase file size to read.

server <- function(input, output) {
  # creat observation of an event in order to enable or disable an numeric input
  observeEvent(input$header, {
    if(input$header == F){
      disable("cn")
    } else {
      enable("cn")
    }
  })
  
  # creat reactibe datatable
  d <- reactive({
    inFile <- input$file1
    
    #check existence of the file path
    if (is.null(inFile))
      return(NULL)
    # read the file
    g <- fread(inFile$datapath,
               skip = input$skip,
               header = input$header,
               sep =input$sep,
               stringsAsFactors = F ,
               na.strings = input$nanstring)
    # assign header in case of box header checked
    if(input$header){
      colnames(g) <- as.character(fread(inFile$datapath,stringsAsFactors = F,header = FALSE )[input$cn,])
    }
    g
  })
  #render the dataframe
  output$contents <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    d(),
    options = list(scrollX = TRUE)
  )
  
  
  # creat summary
  output$sum <- renderPrint({
    summary(d())
    })
  
  #creat a render UI to select X and Y axis from dataframe columns
  output$Xaxis <- renderUI({
    selectInput("Xaxis", "Select your X axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  output$Yaxis <- renderUI({
    selectInput("Yaxis", "Select your Y axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  # creat X and Y axis labels.
  xx <- reactive({
    list(
      title = input$Xaxis,
      showticklabels = TRUE
    )})
  
  yx <-  reactive({
    list(
      title = input$Yaxis
    )})
  
  
  # render the plot
  output$plot_intro <- renderPlotly({
    ploting()
    
  })
  output$plot_corr <- renderPlotly({
    ploting_corr()
    })
  
  output$plot_box <- renderPlotly({
    ploting_box()
    })
  
  output$plot_hist <- renderPlotly({
    ploting_hist()
  })
  
  output$plot_predict <- renderPlotly({
    ploting_predict()
  })
  
  
  # create the plot from all input previously
  ploting <- eventReactive(input$goButton, {
    data <- d()
    plot_ly(data = data, x = ~get(input$Xaxis), y = ~get(input$Yaxis), type = "scatter", color = 'rgb(205, 12, 24)', mode = input$plt) %>%
      layout(xaxis = xx(), yaxis = yx())
  })
  
  # create the plot from all input previously
  ploting_corr <- eventReactive(input$goButton3, {
    
    df <- subcorr()
    corr <- round(cor(df), 2)
    p.mat <- cor_pmat(df)
    corr.plot <- ggcorrplot(corr, hc.order = TRUE, lab = TRUE, outline.color = "white")
    ggplotly(corr.plot)
  })
  
  ploting_box <- eventReactive(input$goButton4, {
    df <- subbox()
    p <- ggplot(stack(df), aes(x = ind, y = values,color = ind)) +
      geom_boxplot() + facet_wrap(ind~.,scales = "free_y",ncol = 2)
  })
  
  ploting_hist <- eventReactive(input$goButton5, {
    df <- subhist()
    ggplot(gather(df), aes(value,color = key)) + 
      geom_histogram(bins = 10) + 
      facet_wrap(~key, scales = 'free_x')
  })
  
  ploting_predict <- eventReactive(input$goButton6, {
    actual <- forecast_model()$actual
    predicted <- forecast_model()$predict
    df <- data.frame("actual"=actual,
                     "predicted" = predicted)
    ggplot(df, aes(actual,predicted)) + geom_point() +
      ggtitle(input$model) + geom_smooth(method="lm")
  })
  
###### --------------------------------------- Part II
  # observe event to disable or enable arima parameters input
  observeEvent(input$algo_out, {
    if(input$algo_out == "arima"){
      enable("ArimaP")
    } else {
      disable("ArimaP")
    }
  })

  #enable and disable ANN
  observeEvent(input$algo_out, {
    if(input$algo_out %in% c("ANN","knn")  ){
      enable("outlierper")
      
    } else {
      disable("outlierper")
    
    }
  })
  

  #creat names list that has only numerics
  numeric_list <- reactive({
    names(unlist(lapply(d(), is.numeric)))
    })
  
 
  #selection from the list above
  output$mcolumns <- renderUI({
    selectInput("mcolumns", "Select Columns to clean Outliers",
                choices =numeric_list() ,multiple = TRUE)})
  #creat D_partII list with d as NULL
  D_partII <- reactiveValues(d=NULL)
  
  #observe event to apply algorithm once selected
  observeEvent(input$ABotton,{
    
    A <- as.data.frame(d())
    if(input$algo_out=="arima" ){
      for(i in input$mcolumns){
        s <- ts(A[,i]) # creat Time Series Object
        S_tso <- tso(s, 
                     discard.method="bottom-up", # to handle NA's
                     types = input$ArimaP,
                     maxit.iloop = 10, #Choice TA OA
                     maxit.oloop = 10
                     )
        ind <- S_tso$outliers$ind
        k  <- S_tso$yadj
        A[ind,i] <- k[ind]
      }
    }else if(input$algo_out=="knn"){
      for(i in input$mcolumns){
        x <- c(1:length(A[,i]))
        G <- data.frame(x,A[,i])
        colnames(G) <- c("x","y")
        k = as.integer(sqrt(length(x)))
        top_n = nrow(G) - nrow(G)*input$outlierper/100
        ind <- do_knno(G, k=k,top_n = top_n)
        A[ind,i] <- NA}
      }else{
        for( i in input$mcolumns ){
          
        nl=3
        unit.type = "tanh"
        Nx.patch=10
        Ny.patch=10
        N.input = Nx.patch*Ny.patch 
        N.hidden = 5*5
        lambda = 0.0002
        beta=6
        rho = 0.01
        epsilon <- 0.001
        max.iterations = 2000
        
        x <- c(1:length(A[,i]))
        G <- data.frame(x,A[,i])
        colnames(G) <- c("x","y")
        outliercount <- nrow(G) - nrow(G)*input$outlierper/100
        traind = as.matrix(G)
        
        autoencoder.object <- autoencode(X.train=traind,
                                         nl=nl,
                                         N.hidden=N.hidden,
                                         unit.type=unit.type,
                                         lambda=lambda,
                                         beta=beta,
                                         rho=rho,
                                         epsilon=epsilon,
                                         optim.method="BFGS",
                                         max.iterations=max.iterations,
                                         rescale.flag=TRUE,
                                         rescaling.offset=0.001)
        
        scores2 <- predict(autoencoder.object,X.input = traind)
        rajmse<-function(x_hat,x) rowMeans((x_hat-x)^2)
        score3 <- rajmse(G, scores2$X.output)
        d <- as.data.frame(score3)
        distance <- d[,1]
        temp <- cbind(G,distance)
        temp$cluster <- 1
        outlier <- order(temp$distance, decreasing=T)[1:outliercount]
        temp$outlier <- FALSE
        temp$outlier[outlier[1:outliercount]] <- TRUE 
        A[temp$outlier,i] <- NA
        }
        }
      
    
    D_partII$d <- A
  })
  
  #Render the last data from Outlier Detection algorithms
  output$Cleaned <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    D_partII$d,
    options = list(scrollX = TRUE)
  )
  
  # render Axis
  output$Xaxis_out <- renderUI({
    selectInput("Xaxis_out", "Select your X axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  output$Yaxis_out <- renderUI({
    selectInput("Yaxis_out", "Select your Y axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  
  #Axis Labels
  xx_out <- reactive({
    list(
      title = input$Xaxis_out,
      showticklabels = FALSE
    )})
  
  yx_out <-  reactive({
    list(
      title = input$Yaxis_out
    )})
  
  
  
  # creat plot with event reactive
  ploting_out <- eventReactive(input$goButton2, {
    data <- d()
    data2 <- D_partII$d
    
    plotlout <- plot_ly(data = data, x = ~get(input$Xaxis_out), y = ~get(input$Yaxis_out), type = "scatter",mode = input$plt_out) %>%
      layout(xaxis = xx_out(), yaxis = yx_out())
    if(input$plt_out=="line"){
      plotlout <- add_lines(plotlout, y = data2[,input$Yaxis_out] ,type = "scatter",mode = input$plt_out,name="Correction")
    }else{
      plotlout<- add_markers(plotlout, y = data2[,input$Yaxis_out] ,type = "scatter",mode = input$plt_out,name="Correction")
    }
    
    plotlout
  })
  #Render plot
  output$plot_out <- renderPlotly({
    ploting_out()
    
  })
  
  ###### --------------------------------------- Part III
  #Columns Selections
  output$mcolumns_nas <- renderUI({
    selectInput("mcolumns_nas", "Select Columns to fill the NA's",
                choices =numeric_list() ,multiple = TRUE)})
  
  #columns selected to save in csv Files
  output$mcolumns_csv <- renderUI({
    selectInput("mcolumns_csv", "Select Columns:",
                choices =numeric_list() ,multiple = TRUE)})
  
  
  
  D_partIII <- reactiveValues(d=NULL,f=NULL)
  
   observeEvent(input$ABotton2,{
    if(is.null(D_partII$d)){
      A <- d()
    }else{
      A <- D_partII$d
    }
    
    
   
    for(i in input$mcolumns_nas ){
      if( input$algo_na =="Interpolation"){
        A[,i] <- na.interpolation(A[,i])
      }else{
        A[,i] <- na.kalman(A[,i])
      }
      
    }
     D_partIII$d <- A
  })
  
  output$filled <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    D_partIII$d,
    options = list(scrollX = TRUE)
  )
  #Write the files base on the inputs above
  
  observeEvent(input$ABotton2,{
    if(is.null(D_partIII$d)){
      A <- D_partII$d
    }else{
      A <- D_partIII$d
    }
    
    D_partIII$f <- A
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(D_partIII$f, file,row.names = FALSE)
    }
  )
  # Select Variables for Correlation
  
  output$corrVars <- renderUI({
    selectInput("corrVars", "Select Columns for Correlation",
                choices = numeric_list(), multiple = TRUE,selected = numeric_list())})
  
  subcorr <- reactive({
    df <- subset(d(), select = input$corrVars)
    df
  })
  
  output$boxVars <- renderUI({
    selectInput("boxVars", "Select Columns for Box Plot",
                choices = numeric_list(), multiple = TRUE,selected = numeric_list())})
  
  subbox <- reactive({
    df <- subset(d(), select = input$boxVars)
    df
  })
  
  output$histVars <- renderUI({
    selectInput("histVars", "Select Columns for Histograms",
                choices = numeric_list(), multiple = TRUE,selected = numeric_list())})
  
  subhist <- reactive({
    df <- subset(d(), select = input$histVars)
    df
  })
  
  output$iVars <- renderUI({
    selectInput("iVars", "Select Independent Variables in the model",
                choices = numeric_list(), multiple = TRUE,selected = numeric_list()[3])})
  
  output$dVar <- renderUI({
    selectInput("dVar", "Select Dependent Variable",
                choices = numeric_list(),selected = numeric_list()[2])})
  
  forecast_model <- reactive({
    df = d()
    smp_siz <- floor(input$trainSize/100*nrow(df))
    trainSample <- sample(seq_len(nrow(df)),size = smp_siz)
    train <- df[trainSample,]
    test <- df[-trainSample,]
    Outcome <- NULL
    Outcome$select <- input$model
    if (input$model == "Linear Regression"){
      train_linear = subset(train, select = c(input$dVar,input$iVars))
      test_linear = subset(test, select = c(input$dVar,input$iVars))
      Outcome$train = train_linear
      f <- paste(names(train_linear)[1], "~", paste(names(train_linear)[-1], collapse=" + "))
      model_linear <- lm(str2lang(f), data = train_linear)
      Outcome$model = model_linear
      predict_linear = predict(model_linear,test_linear)
      predict_linear = as.numeric(predict_linear)
      Outcome$predict = predict_linear
      Outcome$error = rmse(as.numeric(test_linear[[1]]),predict_linear)
      Outcome$actual = as.numeric(test_linear[[1]])
      results <- data.frame(Actual = Outcome$actual, Prediction = predict_linear)
      output$downloadresult <- downloadHandler(
        filename = function() {
          paste("result_lr", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file,row.names = FALSE)
        }
      )
    }
    else if(input$model == "XGboost"){
      train_xgb = subset(train, select = c(input$dVar,input$iVars))
      test_xgb = subset(test, select = c(input$dVar,input$iVars))
      train_xgb <- as.matrix(train_xgb)
      test_xgb <- as.matrix(test_xgb)
      Outcome$train = train_xgb
      model_xgb <- xgboost(
        data = train_xgb[, 2:length(colnames(train_xgb))],
        label = train_xgb[, 1],
        nrounds = 1000,
        objective = "reg:squarederror",
        early_stopping_rounds = 3,
        max_depth = 6,
        eta = .25
      )
      Outcome$model = model_xgb
      predict_xgb = predict(model_xgb,test_xgb[, 2:length(colnames(test_xgb))])
      predict_xgb = as.numeric(predict_xgb)
      Outcome$predict = predict_xgb
      Outcome$error = rmse(as.numeric(test_xgb[[1]]),predict_xgb)
      Outcome$actual = test_xgb[,1]
      results <- data.frame(Actual = Outcome$actual, Prediction = predict_xgb)
      output$downloadresult <- downloadHandler(
        filename = function() {
          paste("result_xgb", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file,row.names = FALSE)
        }
      )
    }
    else if(input$model == "Neural Network"){
      train_nn = subset(train, select = c(input$dVar,input$iVars))
      test_nn = subset(test, select = c(input$dVar,input$iVars))
      Outcome$train = train_nn
      f <- paste(names(train_nn)[1], "~", paste(names(train_nn)[-1], collapse=" + "))
      model_nn <- train(as.formula(f), method="nnet", data=train_nn, trace=TRUE, maxit=1000, linout = 1)
      Outcome$model = model_nn
      predict_nn = predict(model_nn,test_nn)
      predict_nn = as.numeric(predict_nn)
      Outcome$predict = predict_nn
      Outcome$error = rmse(as.numeric(test_nn[[1]]),predict_nn)
      Outcome$actual = as.numeric(test_nn[[1]])
      results <- data.frame(Actual = Outcome$actual, Prediction = predict_nn)
      output$downloadresult <- downloadHandler(
        filename = function() {
          paste("result_nn", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file,row.names = FALSE)
        }
      )
    }
    else if(input$model == "Random Forest Model"){
      train_rfm = subset(train, select = c(input$dVar,input$iVars))
      test_rfm = subset(test, select = c(input$dVar,input$iVars))
      Outcome$train = train_rfm
      f <- paste(names(train_rfm)[1], "~", paste(names(train_rfm)[-1], collapse=" + "))
      model_rfm <- randomForest(as.formula(f), data = train_rfm,ntree   = 500)
      Outcome$model = model_rfm
      predict_rfm = predict(model_rfm,test_rfm)
      predict_rfm = as.numeric(predict_rfm)
      Outcome$predict = predict_rfm
      Outcome$error = rmse(as.numeric(test_rfm[[1]]),predict_rfm)
      Outcome$actual = as.numeric(test_rfm[[1]])
      results <- data.frame(Actual = Outcome$actual, Prediction = predict_rfm)
      output$downloadresult <- downloadHandler(
        filename = function() {
          paste("result_rfm", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results, file,row.names = FALSE)
        }
      )
    }
    Outcome
  })
  output$model <-renderText({
    forecast_model()$model
    })
  
  output$error <-renderText({
    forecast_model()$error
  })
  
  output$predict <-renderText({
    length(forecast_model()$predict)
  })
  
  output$actual <-renderText({
    length(forecast_model()$actual)
  })
  
  output$select <-renderText({
    forecast_model()$select
  })

  
  aggregations <- reactive({
    c("Yearly","Quaterly","Monthly","Weekly","Daily")
    })
    output$aggreg <- renderUI({
      
      selectInput("aggreg", "Select Date/Time Aggregations",
                  choices = aggregations(),selected = aggregations()[1])})
    
      apply_agg <- reactive({
      df <- d()
      df.T <- subset(df, select = c(input$tVar,input$sumVar))
      df.T <- df.T %>% mutate(DATE = parse_datetime(DATE, format = "%d/%m/%Y %H:%M"))
      str(df.T)
      
      if(input$aggreg == "Yearly"){
        df.yearly <- apply.yearly(xts(df.T[,2],order.by = df.T$DATE),input$aggregation)
        output$downloadagg <- downloadHandler(
          filename = function() {
            paste("result_yearly", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df.yearly, file,row.names = FALSE)
          }
        )
        df.yearly}
      else if(input$aggreg == "Quaterly"){
        df.quarterly <- apply.quarterly(xts(df.T[,2],order.by = df.T$DATE),input$aggregation)
        output$downloadagg <- downloadHandler(
          filename = function() {
            paste("result_quarterly", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df.quarterly, file,row.names = FALSE)
          }
        )
        df.quarterly}
      else if(input$aggreg == "Monthly"){
        df.monthly <- apply.monthly(xts(df.T[,2],order.by = df.T$DATE),input$aggregation)
        output$downloadagg <- downloadHandler(
          filename = function() {
            paste("result_monthly", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df.monthly, file,row.names = FALSE)
          }
        )
        df.monthly}
      else if(input$aggreg == "Weekly"){
        df.weekly <- apply.weekly(xts(df.T[,2],order.by = df.T$DATE),input$aggregation)
        output$downloadagg <- downloadHandler(
          filename = function() {
            paste("result_weekly", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df.weekly, file,row.names = FALSE)
          }
        )
        df.weekly}
      else if(input$aggreg == "Daily"){
        df.daily <- apply.daily(xts(df.T[,2],order.by = df.T$DATE),input$aggregation)
        output$downloadagg <- downloadHandler(
          filename = function() {
            paste("result_daily", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(df.daily, file,row.names = FALSE)
          }
        )
        df.daily}
    })
    output$apply_aggr <-   DT::renderDataTable({
      apply_agg()
    })
    output$sumVar <- renderUI({
      selectInput("sumVar", "Select Summarizable Variable",
                  choices = numeric_list(),selected = numeric_list()[2])}) 
    output$tVar <- renderUI({
      selectInput("tVar", "Select Date Variable",
                  choices = numeric_list(),selected = numeric_list()[1])})   
    
  
}
#shinyApp(ui, server)  #Uncomment this to the run the application locally
