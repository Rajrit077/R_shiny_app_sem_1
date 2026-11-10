library(shiny)
library(ggplot2)
library(rvest)
library(bslib)

#https://en.wikipedia.org/wiki/Demographics_of_the_world

ui <- fluidPage(
  theme = bs_theme(bootswatch = "solar"),
  fluidRow(
    textInput("url","Enter Website Url",value = ""),
    numericInput("index","Which table to display",value = 1),
    actionButton("scrape","Scrape Data"),
    tableOutput("web"),
    verbatimTextOutput("c1"),
    verbatimTextOutput("c2"),
    verbatimTextOutput("c3"),
    verbatimTextOutput("c4"),
    verbatimTextOutput("c5")
    
    
  ),
  
  fluidRow(
    
    column(2,
           textInput("cname1","Enter Column Name","Score1"),
           selectInput("t1","Type of Data",choices = c("Numeric","Categorical")),
           textInput("n1","Enter the data separated by comma or space")
    ),
    column(2,
           textInput("cname2","Enter Column Name","Score2"),
           selectInput("t2","Type of Data",choices = c("Numeric","Categorical")),
           textInput("n2","Enter the data separated by comma or space")
    ),
    column(2,
           textInput("cname3","Enter Column Name","Score3"),
           selectInput("t3","Type of Data",choices = c("Numeric","Categorical")),
           textInput("n3","Enter the data separated by comma orspace"),
           actionButton("simulate","Simulate")
    ),
    column(2,
           textInput("cname4","Enter Column Name","Score4"),
           selectInput("t4","Type of Data",choices = c("Numeric","Categorical")),
           textInput("n4","Enter the data separated by comma or space")
    ),
    column(2,
           textInput("cname5","Enter Column Name","Score5"),
           selectInput("t5","Type of Data",choices = c("Numeric","Categorical")),
           textInput("n5","Enter the data separated by comma or space")
    ),
    
  ),
  fluidRow(
    tableOutput("tableO")
  ),
  fluidRow(
    titlePanel("Histogram"),
    column(4,
           selectInput("hist","Enter the Column Name",choices = NULL),
           actionButton("s_h","Simulate")
    ),
    column(8,
           plotOutput("plot0")
    ),
    
    
  ),
  fluidRow(
    titlePanel("Scatterplot and Fitting"),
    column(4,
           selectInput("x","Enter Column Name for X axis",choices = NULL),
           selectInput("y","Enter Column Name for Y axis",choices = NULL),
           numericInput("n","Enter Degree of Polynomial fitting",value = 1),
           actionButton("f","Simulate")
           
    ),
    column(8,
           plotOutput("plot1"),
    )
    
    
  ),
  fluidRow(
    titlePanel("Logistic Regression"),
    column(4),
    column(4,
           selectInput("x1","Enter Column Name for the Data",choices = NULL),
           selectInput("y1","Enter Column Name for binary Data",choices = NULL),
           actionButton("log","Generate Summary"),
           numericInput("l1","Enter the Value",value = NULL),
           actionButton("predict","Predict")
    ),
    column(4)
  ),
  fluidRow(
    verbatimTextOutput("text0"),
    verbatimTextOutput("text1")
  )
  
)



server <- function(input, output, session) {
  
  bs_themer()
  
  scraped_df<-eventReactive(input$scrape,{
    req(input$url,input$index)
    webpage<-read_html(input$url)
    table_nodes<-html_nodes(webpage,"table")
    req(length(table_nodes)>=input$index)
    html_table(table_nodes[[input$index]],fill=TRUE)
  })
  
  output$web<-renderTable({
    scraped_df()
  })
  
  output$c1<-renderPrint({
    cnames<-colnames(scraped_df())
    scraped_df()[[cnames[1]]]
  })
  
  output$c2<-renderPrint({
    cnames<-colnames(scraped_df())
    scraped_df()[[cnames[2]]]
  })
  
  output$c3<-renderPrint({
    cnames<-colnames(scraped_df())
    scraped_df()[[cnames[3]]]
  })
  
  output$c4<-renderPrint({
    cnames<-colnames(scraped_df())
    scraped_df()[[cnames[4]]]
  })
  
  output$c5<-renderPrint({
    cnames<-colnames(scraped_df())
    scraped_df()[[cnames[5]]]
  })
  
  
  
  df<-eventReactive(input$simulate,{
    #req(input$n1, input$n2, input$n3, input$n4, input$n5,input$cname1, input$cname2, input$cname3, input$cname4, input$cname5)
    validate(
      need(input$n1 != "", "Please enter data in at least the first column.")
    )
    # 1. First read the inputs into helper variables to check them easily
    n1_val <- input$n1
    n2_val <- input$n2
    n3_val <- input$n3
    n4_val <- input$n4
    n5_val <- input$n5
    
    if(input$t1=="Categorical"){
      data1<-unlist(strsplit(n1_val, "[, ]+"))
    }
    else{
      data1 <- as.numeric(unlist(strsplit(n1_val, "[, ]+")))
      
    }
    
    if(input$t2=="Categorical"){
      data2<-unlist(strsplit(n2_val, "[, ]+"))
    }
    else{
      data2 <- as.numeric(unlist(strsplit(n2_val, "[, ]+")))
      
    }
    
    if(input$t3=="Categorical"){
      data3<-unlist(strsplit(n3_val, "[, ]+"))
    }
    else{
      data3 <- as.numeric(unlist(strsplit(n3_val, "[, ]+")))
      
    }
    
    if(input$t4=="Categorical"){
      data4<-unlist(strsplit(n4_val, "[, ]+"))
    }
    else{
      data4 <- as.numeric(unlist(strsplit(n4_val, "[, ]+")))
      
    }
    
    if(input$t5=="Categorical"){
      data5<-unlist(strsplit(n5_val, "[, ]+"))
    }
    else{
      data5 <- as.numeric(unlist(strsplit(n5_val, "[, ]+")))
      
    }
    
    
    
    # 2. Convert text strings into numeric vectors
    
    if (n2_val == "" & n3_val == "" & n4_val == "" & n5_val == "") {
      # Only column 1 has data
      raw_df <- data.frame(data1)
      colnames(raw_df) <- c(input$cname1)
      
    } else if (n3_val == "" & n4_val == "" & n5_val == "") {
      # Columns 1 and 2 have data
      raw_df <- data.frame(data1, data2)
      colnames(raw_df) <- c(input$cname1, input$cname2)
      
    } else if (n4_val == "" & n5_val == "") {
      # Columns 1, 2, and 3 have data
      raw_df <- data.frame(data1, data2, data3)
      colnames(raw_df) <- c(input$cname1, input$cname2, input$cname3)
      
    } else if (n5_val == "") {
      # Columns 1, 2, 3, and 4 have data
      raw_df <- data.frame(data1, data2, data3, data4)
      colnames(raw_df) <- c(input$cname1, input$cname2, input$cname3, input$cname4)
      
    } else {
      # All 5 columns have data
      raw_df <- data.frame(data1, data2, data3, data4, data5)
      colnames(raw_df) <- c(input$cname1, input$cname2, input$cname3, input$cname4, input$cname5)
    }
    
    
    return(raw_df)
  })
  
  
  output$tableO<-renderTable({
    df()
  })
  
  # Plotting Histogram
  
  observeEvent(input$simulate, {
    req(df())
    updateSelectInput(session, "hist", choices = colnames(df()))
  })
  
  plot_histogram<-eventReactive(input$s_h,{
    req(df(),input$hist)
    
    ggplot(df(),aes(.data[[input$hist]]))+geom_histogram(fill = "salmon",colour = "black")+theme_minimal()
  })
  
  output$plot0<-renderPlot({
    plot_histogram()
    
    
  })
  
  # Scatter Plot
  observeEvent(input$simulate,{
    req(df())
    updateSelectInput(session,"x",choices = colnames(df()))
    updateSelectInput(session,"y",choices = colnames(df()))
  })
  
  scatterplot<-eventReactive(input$f,{
    req(df(), isolate(input$x), isolate(input$y), isolate(input$n))
    
    # Fetch the static values at the moment of the button click
    current_x <- isolate(input$x)
    current_y <- isolate(input$y)
    current_n <- isolate(input$n)
    model <- lm(
      df()[[current_y]] ~ poly(df()[[current_x]],
                               current_n, raw = TRUE)
    )
    
    # Extract coefficients
    coefs <- coef(model)
    
    # Create the equation
    equation <- paste0(
      "y = ", round(coefs[1], 3),
      paste0(
        ifelse(coefs[-1] >= 0, " + ", " - "),
        round(abs(coefs[-1]), 3),
        "x^", seq_along(coefs[-1])
      ) |> paste(collapse = "")
    )
    
    ggplot(df(),aes(.data[[current_x]],.data[[current_y]]))+geom_point(size=3, color="blue")+geom_smooth(method=lm, formula = y~poly(x,current_n),colour = "red")+annotate("label",x = Inf, y = -Inf, label = equation,  hjust = 1.1, vjust = -0.5, size = 4)+theme_minimal()
  })
  
  
  output$plot1<-renderPlot({
    scatterplot()
  })
  
  observeEvent(input$simulate,{
    req(df())
    updateSelectInput(session,"x1",choices = colnames(df()))
    updateSelectInput(session,"y1",choices = colnames(df()))
  })
  
  gist<-eventReactive(input$log,{
    req(input$x1, input$y1, df())
    
    log_model <- glm(
      reformulate(input$x1, response = input$y1),
      data = df(),
      family = binomial()
    )
    
    #summary(log_model)
    list(model = log_model, summary = summary(log_model))
    
  })
  
  probability <- eventReactive(input$predict, {
    req(gist(), input$l1)
  
    coefs <- coef(gist()$model)
    beta_0 <- coefs[1]          # Intercept
    beta_1 <- coefs[2]          # Coefficient for Score1
    
    
    x_val <- input$l1
    log_odds <- beta_0 + (beta_1 * x_val)
    p <- 1 / (1 + exp(-log_odds))
    
    return(p)
  })
  
  output$text0 <- renderPrint({
      gist()
    
  })
  
  output$text1<-renderPrint({
    # req(gist())
    req(probability())
    probability()
  })
  
  
}

shinyApp(ui, server)












