library(shiny)
library(ggplot2)
library(cluster)
library(fpc)
library(rpart)
library(shinydashboard)

bank <- read.csv("F:/RStudio/Projects/Project/bank.csv")
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Predictive Marketing", dropdownMenuOutput("msgOutput"),
                  # dropdownMenu(type = "message",
                  #              messageItem(from = "Marketing Update", message = "We need to analyze more campaigns"),
                  #              messageItem(from = "Finance Result", message = "Check results in finance", icon = icon("bar-chart"))),
                  dropdownMenu(type = "tasks", 
                               taskItem(
                                 value = 45,
                                 color = "aqua",
                                 "Analytics of finance"
                               ),
                               taskItem(
                                 value = 75,
                                 color = "yellow",
                                 "Analytics of marketing campaigns"
                               ),
                               taskItem(
                                 value = 20,
                                 color = "red",
                                 "Working on new datasets"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "New tabs are added to your dashboard",
                                 icon = icon("dashboard"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server overloading increased to 95%",
                                 icon = icon("warning"),
                                 status = "warning"
                               )
                  )
  ),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Know your dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Recursive Partitioning", tabName = "regression", icon = icon("charts")),
      menuItem("Analysis", tabName = "analysis", icon = icon("charts"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBox(15*200, "Budget for next 20 days", icon = icon("hourglass-3"), color = "aqua"),
                valueBox(25*200, "Target for sales", icon = icon("warning"), color = "red"),
                valueBoxOutput("itemRequested")
              ),
              fluidRow(
                infoBox("Sales of Marketing Campaigns", 1000, icon = icon("thumbs-up")),
                infoBox("Conversion %", paste0('20%'), icon = icon("warning")),
                infoBoxOutput("approvedSales")
              )
      ),
      tabItem(tabName = "dataset",
              fluidRow(
                       box(solidHeader = T, status = "info", title = "File Upload", width = 100,
                           fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                           helpText("Default max. file size is 5MB"),
                           tags$hr(),
                           h5(helpText("Select the read.table parameters below")),
                           checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                           checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                           br(),
                           radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                       )
              ),
              fluidRow(
                       box(solidHeader = T, status = "success", title = "About your dataset", width = 100,
                           uiOutput("tb")
                           
                           # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
                           #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
                           #                   tabPanel("Data", tableOutput("table")))    
                       )
              )
      ),
      tabItem(tabName = "regression",
              fluidRow(
                box(solidHeader = T, status = "info", title = "Add your dataset",
                  fileInput('file1', 'Upload data',                              
                    accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                    conditionalPanel(condition="input.conditionedPanels==1",h4("After loading your data(.csv), please select the columns you'd like to use for prediction analysis"))
                ),
                box(solidHeader = T, status = "success", title = "Dataset loaded",
                    conditionalPanel(condition="input.conditionedPanels==1",
                                     tabPanel("Columns",uiOutput("choose_columns")),
                                     tabPanel("target",uiOutput("choose_target")))
                )
              ),
              fluidRow(
                box(solidHeader = T, status = "success", title = "Dataset Analysis", width = 100,
                    tabsetPanel(
                      tabPanel("Data",
                               h3("The dataset to be used for prediction is displayed below:"),
                               p("(A maximum of 50 rows and 10 columns can be displayed here due to window size, but all of the data uploaded will be used for prediction and cluster analysis.)"),
                               tableOutput("view"),
                               value=1
                      ),
                      tabPanel("Cluster Analysis",    
                               numericInput('clusters', 'Number of Clusters', 3, min = 1, max = 9),
                               plotOutput('kmeans_plot'),
                               h3("Cluster Means"),
                               tableOutput('agg_table'),
                               value=1),
                      tabPanel("Prediction Analysis",tableOutput("prediction"),value=1),
                      tabPanel("Plotting", plotOutput(outputId = "plot_tree")),
                      id = "conditionedPanels"
                    )
                )
              )
        
      ),
      tabItem(tabName = "analysis",
            fluidRow(
              box(solidHeader = T, status = "info", title = "Select the attributes",
                selectInput(
                  inputId = "y",
                  label = "Y-axis",
                  choices = c(
                      "Age" = "age",
                      "Job" = "job",
                      "Marital" = "marital",
                      "Education" = "education",
                      "Balance" = "balance",
                      "Housing" = "housing",
                      "Loan" = "loan",
                      "Contact" = "contact",
                      "Day" = "day",
                      "Month" = "month",
                      "Duration" = "duration",
                      "Campaign" = "campaign",
                      "Poutcome" = "poutcome",
                      "Subscribe" = "y"
                    ),
                  selected = "y"
                ),
                selectInput(
                  inputId = "x",
                  label = "X-axis",
                  choices = c(
                    "Age" = "age",
                    "Job" = "job",
                    "Marital" = "marital",
                    "Education" = "education",
                    "Balance" = "balance",
                    "Housing" = "housing",
                    "Loan" = "loan",
                    "Contact" = "contact",
                    "Day" = "day",
                    "Month" = "month",
                    "Duration" = "duration",
                    "Campaign" = "campaign",
                    "Poutcome" = "poutcome",
                    "Subscribe" = "y"
                  ),
                  selected = "campaign"
                )
              ),
              box(solidHeader = T, status = "success", title = "Scatterplot",
                plotOutput(outputId = "scatterplot")
              )
            )
        
      )
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$msgOutput <- renderMenu({
     msgs <- apply(read.csv("message.csv"), 1,function(row){
       messageItem(from = row[["from"]], message = row[["message"]])
     })
     
     dropdownMenu(type = "messages", .list = msgs)
   })
   
   output$itemRequested <- renderValueBox({
     valueBox(10*150, "Item Requested by Employees", icon = icon("fire"), color = "yellow")
     
   })
   
   output$approvedSales <- renderInfoBox({
     infoBox("Approved Sales", "100,000", icon = icon("bar-chart-o"))
   })
   
   data <- reactive({
     file1 <- input$file
     if(is.null(file1)){return()} 
     read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
     
   })
   
   # this reactive output contains the summary of the dataset and display the summary in table format
   output$filedf <- renderTable({
     if(is.null(data())){return ()}
     input$file
   })
   
   # this reactive output contains the summary of the dataset and display the summary in table format
   output$sum <- renderTable({
     if(is.null(data())){return ()}
     summary(data())
     
   })
   
   # This reactive output contains the dataset and display the dataset in table format
   output$table <- renderTable({
     if(is.null(data())){return ()}
     data()
   })
   
   # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
   output$tb <- renderUI({
     tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
   })
   
   dInput = reactive({
     in.file = input$file1
     
     if (is.null(in.file))
       return(NULL)
     
     data <- read.csv(in.file$datapath, na.strings=c(".", "NA", "", "?"))
     
   })
   
   # Function that render the data file and passes it to ui.R
   output$view = renderTable({
     d.input = dInput()
     if (is.null(d.input)) return(NULL)
     if (ncol(d.input>10)) d.input = d.input[] #$,1:10]
     head(dInput(), n=50)  
   })
   
   # Check boxes
   output$choose_columns <- renderUI({
     # If missing input, return to avoid error later in function
     d.input = dInput()
     if(is.null(d.input))
       return()
     
     # Get the data set with the appropriate name
     #dat <- get(input$file1)
     colnames <- names(d.input)
     
     # Create the checkboxes and select them all by default
     checkboxGroupInput("columns", "Choose columns", 
                        choices  = colnames,
                        selected = colnames)
   })
   
   # Target variable selection
   output$choose_target <- renderUI({
     # If missing input, return to avoid error later in function
     d.input = dInput()
     if(is.null(d.input))
       return()
     
     # Get the data set with the appropriate name
     
     colnames <- names(d.input)
     
     # Create the checkboxes and select them all by default
     selectInput("target", "Choose Target Attribute", 
                 choices  = colnames)#,
     # selected = colnames)
   })
   #k-means cluster
   
   selectedData <- reactive({
     d.input = dInput()
     dat_cluster <- d.input
     dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
     for (x in 1:length(dat_cluster))      # Transform the variables
     { 
       if(!is.numeric(dat_cluster[,x])) {
         column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
         temp_data <- as.numeric(dat_cluster[,x])
         temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
         colnames(temp_mat) <- column
         data_clust_trans <- cbind(dat_cluster,temp_mat)
       }
       
     }
     nums <- sapply(dat_cluster, is.numeric)  
     dat_cluster_numeric <- dat_cluster[,nums]  #Considered only numeric values for K-Means
     dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE
     
     dat_cluster_numeric
     
   })	
   clusters <- reactive({
     
     kmeans(selectedData(), input$clusters)
   })
   
   output$kmeans_plot <- renderPlot({
     
     clusplot(selectedData(), clusters()$cluster, color=clusters()$cluster, labels=clusters()$cluster, cex=1.0, shade=TRUE,lines=0)
   })
   
   output$clust_table <- renderTable({
     clust_table <- cbind(selectedData(), by=list(cluster=clusters()$cluster))
     clust_table
   })
   
   output$agg_table <- renderTable({
     agg_table <-  aggregate(selectedData(), by=list(cluster=clusters()$cluster),mean)
     agg_table
   })
   
   # Decision Tree #Function that calculates the output sent to the main panel in ui.R
   output$prediction = renderTable({
     
     d.input = dInput()
     # If missing input, return to avoid error later in function
     if(is.null(d.input))
       return()
     
     # Get the data set
     dat <- d.input
     
     target <- input$target
     # print("target", target)
     dat <- dat[, input$columns, drop = FALSE]
     apply.pred_amt(dat,target)
     
   })
   
   
   apply.pred_amt <- function(df,v_target) {
     
     data_set <- df
     attach(data_set)
     
     target <- v_target 
     nobs <- nrow(data_set)
     form <- formula(paste(target, "~ ."))
     
     #Dataset Division
     form_column <- eval(parse(text=paste("data_set",target,sep="$")))
     new_data_set <- subset(data_set, is.na(form_column))
     nobs <- nrow(data_set) # 20000 observations 
     train <- sample(nobs, 0.75*nobs) # 15000 observations
     trainset <- data_set[train, ]
     validate <- sample(setdiff(seq_len(nobs), train), 0.25*nobs) # 5000 observations
     validateset <- data_set[validate, ]
     test <- setdiff(setdiff(seq_len(nobs), train), validate) # 5000 observations
     testset <- data_set[test,]
     
     motorVars <- setdiff(colnames(data_set),list(target))#,ignore))
     dsFormula <- as.formula(paste(target,paste(motorVars,collapse=' + '),sep=" ~ "))
     model <- rpart(dsFormula, data=trainset)
     model
     output$plot_tree <- renderPlot({
       plot(model, uniform = TRUE)
       text(model, use.n = TRUE, all = TRUE, cex = 0.8)
     })
     model$variable.importance
     summary(model)
     cp <- printcp(model)  #Complexity Parameter
     predicted <- predict(model, newdata=testset)
     # Extract the relevant variables from the dataset.
     pred_loc <- ncol(data_set)
     sdata <- subset(data_set[test,]) 
     
     variance <-  round((predicted - sdata[,pred_loc])/sdata[,pred_loc] * 100,2)
     variance_value <- (predicted - sdata[,pred_loc])
     res <- cbind(sdata, predicted,variance_value,variance)
     # New data prediction
     predicted_new <- predict(model, newdata=new_data_set)
     res_new <- cbind(predicted_new,new_data_set)
     names(res_new)[1]<-paste(target, "_predicted")
     res_new
   }
   
   observe({
     if (is.null(input$dataset))
       return()
     obj<-switch(input$dataset,
                 "iris" = iris,
                 "mtcars" = mtcars,
                 "bank" = bank)	 
     var.opts<-names(colnames(obj))
     updateSelectInput(session, "variable", choices = var.opts)
     updateSelectInput(session, "group", choices = var.opts)
   })
   
   output$caption<-renderText({
     switch(input$plot.type,
            "boxplot" 	= 	"Boxplot",
            "histogram" =	"Histogram",
            "density" 	=	"Density plot",
            "bar" 		=	"Bar graph")
   })
   
   
   output$plot <- renderUI({
     plotOutput("p")
   })
   
   #plotting function using ggplot2
   output$p <- renderPlot({
     
     variable <- get(input$dataset)[[input$variable]]
     group <- get(input$dataset)[[input$group]]
     if (is.null(variable) || is.null(group))
       return(NULL)
     
     plot.obj<<-list() # not sure why input$X can not be used directly?
     plot.obj$data<<-get(input$dataset) 
     plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
     plot.obj$group<<-with(plot.obj$data,get(input$group)) 
     
     #dynamic plotting options
     plot.type<-switch(input$plot.type,
                       "boxplot" 	= 	geom_boxplot(),
                       "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                       "density" 	=	geom_density(alpha=.75),
                       "bar" 		=	geom_bar(position="dodge")
     )
     
     require(ggplot2)
     #plotting theme
     .theme<- theme(
       axis.line = element_line(colour = 'gray', size = .75), 
       panel.background = element_blank(),  
       plot.background = element_blank()
     )	 
     if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
       p<-ggplot(plot.obj$data, 
                 aes(
                   x 		= plot.obj$group, 
                   y 		= plot.obj$variable,
                   fill 	= as.factor(plot.obj$group)
                 )
       ) + plot.type
       
       if(input$show.points==TRUE)
       { 
         p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
       }
       
     } else {
       
       p<-ggplot(plot.obj$data, 
                 aes(
                   x 		= plot.obj$variable,
                   fill 	= as.factor(plot.obj$group),
                   group 	= as.factor(plot.obj$group),
                   #color 	= as.factor(plot.obj$group)
                 )
       ) + plot.type
     }
     
     p<-p+labs(
       fill 	= input$group,
       x 		= "",
       y 		= input$variable
     )  +
       .theme
     print(p)
   })
   
   output$scatterplot <- renderPlot({
     ggplot(data = bank, aes_string(x = input$x, y = input$y)) + geom_point()
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

