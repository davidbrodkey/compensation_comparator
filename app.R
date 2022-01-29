library(shiny)
library(ggplot2)
library(scales)


ui <- shinyUI(fluidPage(
  
  titlePanel("Compensation Comparator"),
  
  fluidRow(
    column(2,
           h2("Job 1")
           ,
           numericInput("job_1_base"
                        ,"Base Pay"
                        ,value=0
                        )
           ,
           textInput("job_1_ticker"
                     ,"Ticker")
           ,
           fileInput("job_1_file"
                     ,"Bonus/Stock File"
                     ,accept=".csv")
    )
    ,
    column(2,
           h2("Job 2")
           ,
           numericInput("job_2_base"
                        ,"Base Pay"
                        ,value=0
                        )
           ,
           textInput("job_2_ticker"
                     ,"Ticker")
           ,
           fileInput("job_2_file"
                     ,"Bonus/Stock File"
                     ,accept=".csv")
           )
    ,
    column(8,
           plotOutput("daily_plot"))

  )
))

server <- function(input, output) {
  #Capture File inputs
  file_job_1<-reactive({
    if(is.null(input$job_1_file)){
      return(NULL)
    }
    read.csv(input$job_1_file$datapath)
  })
  
  file_job_2<-reactive({
    if(is.null(input$job_2_file)){
      return(NULL)
    }
    read.csv(input$job_2_file$datapath)
  })
  
  
  salary_table<-reactive({
    date_list<-Sys.Date()+1:(365*5)
    df<-data.frame(date_list)
    colnames(df)<-"date"
    
    df$job_1_pay<-input$job_1_base/365
    df$job_2_pay<-input$job_2_base/365
    
    df
  })
  
  output$daily_plot <- renderPlot({
    df<-salary_table()
    
    out_plot<-ggplot(data=df,aes(x=date))+
      geom_line(aes(y=job_1_pay),color="blue")+
      geom_line(aes(y=job_2_pay),color="red")+
      xlab("Date")+
      ylab("Daily Pay")+
      ggtitle("Daily Pay by Job")+
      scale_y_continuous(labels=scales::dollar_format())
    
    out_plot
  })
}

shinyApp(ui = ui, server = server)