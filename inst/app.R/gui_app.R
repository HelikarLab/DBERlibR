# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# #------------------------------------------------------
# # https://chat.openai.com/chat  how to build graphical user interface to take inputs from the user in an r package
# # https://deanattali.com/2015/04/21/r-package-shiny-app/
# #------------------------------------------------------

# # Take demographic variable name and create a subset of the dataframe with average score and demographic data
# message("############")
# nd <- ncol(demographic_data)
# for (i in 2:nd) {
#   message(paste0(colnames(demographic_data[i]),"=",i-1))
# }
#

library(DBERlibR)
library(shiny)
library(shinyFiles)

ui <- fluidPage(
  navlistPanel(
    tabPanel("Item Analysis",
             fluidRow(
               column(12, br(),
                      fileInput("file1", "Choose Data File"),
                      sliderInput("scale1", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit1", "Run Item Analysis", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,
                      # tableOutput("summary_table")
               )
             )
    ),
    tabPanel("Paired Samples Data Analysis",
             fluidRow(
               column(12, br(),
                      fileInput("file2", "Choose Pre-test Data File"),
                      fileInput("file3", "Choose Post-test Data File"),
                      sliderInput("scale2", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit2", "Run Paired Samples Data Analysis", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,

               )
             )
    ),
    tabPanel("Independent Samples Data Analysis",
             fluidRow(
               column(12, br(),
                      fileInput("file4", "Choose Treatment-group Data File"),
                      fileInput("file5", "Choose Control-group Data File"),
                      sliderInput("scale3", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit3", "Run Independent Samples Data Analysis", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,

               )
             )
    ),
    tabPanel("One-way ANCOVA",
             fluidRow(
               column(12, br(),
                      fileInput("file6", "Choose Treatment-group Pre-test Data File"),
                      fileInput("file7", "Choose Treatment-group Post-test Data File"),
                      fileInput("file8", "Choose Control-group Pre-test Data File"),
                      fileInput("file9", "Choose Control-group Post-test Data File"),
                      sliderInput("scale4", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit4", "Run One-way ANCOVA", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,

               )
             )
    ),
    tabPanel("One-way Repeated Measures ANOVA",
             fluidRow(
               column(12, br(),
                      fileInput("file10", "Choose Pre-test Data File"),
                      fileInput("file11", "Choose Post-test Data File"),
                      fileInput("file12", "Choose Post2-test Data File"),
                      sliderInput("scale5", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit5", "Run One-way Repeated Measures ANOVA", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,

               )
             )
    ),
    tabPanel("Analysis of Demographic Group Differences",
             fluidRow(
               column(12, br(),
                      fileInput("file13", "Choose Assessment Data File"),
                      fileInput("file14", "Choose Demographic Data File"),
                      sliderInput("scale6", "Cutoff Value", min = 0, max = 1, value = 0.15),
                      selectInput("column", "Group Variable Name", choices = NULL),
                      # textInput("text7", "Group Variable Name"),
                      h3("___________________________", style = "color: blue;"),
                      actionButton("submit6", "Run Analysis of Demographic Group Differences", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(12,

               )
             )
    )
  )
)

server <- function(input, output, session) {
  data1 <- reactive({
    input$file1$datapath
  })
  # data11 <- reactive({
  #   inFile <- input$file14
  #   if (is.null(inFile)) return(NULL)
  #   read.csv(inFile$datapath)
  # })
  data2 <- reactive({
    input$file2$datapath
  })
  data3 <- reactive({
    input$file3$datapath
  })
  data4 <- reactive({
    input$file4$datapath
  })
  data5 <- reactive({
    input$file5$datapath
  })
  data6 <- reactive({
    input$file6$datapath
  })
  data7 <- reactive({
    input$file7$datapath
  })
  data8 <- reactive({
    input$file8$datapath
  })
  data9 <- reactive({
    input$file9$datapath
  })
  data10 <- reactive({
    input$file10$datapath
  })
  data11 <- reactive({
    input$file11$datapath
  })
  data12 <- reactive({
    input$file12$datapath
  })
  data13 <- reactive({
    input$file13$datapath
  })
  data14 <- reactive({
    input$file14$datapath
  })
  data <- reactive({
    inFile <- input$file14
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })


  scale1 <- reactive({
    input$scale1
  })
  scale2 <- reactive({
    input$scale2
  })
  scale3 <- reactive({
    input$scale3
  })
  scale4 <- reactive({
    input$scale4
  })
  scale5 <- reactive({
    input$scale5
  })
  scale6 <- reactive({
    input$scale6
  })
  # text7 <- reactive({
  #   input$text7
  # })

  ## Item Analysis

  # output$summary_table <- renderTable({
  #   summary(data11())
  # })


  observeEvent(input$submit1, {
    if (is.null(data1())) {
      return(NULL)
    }
    # data1 <- file1()
    item_analysis(score_csv_data = data1(),
                  m_cutoff = scale1()
                  )
  })

  ## Paired Samples Data Analysis

  observeEvent(input$submit2, {

    if (is.null(data2())) {
      return(NULL)
    }
    if (is.null(data3())) {
      return(NULL)
    }

    paired_samples(pre_csv_data = data2(),
                   post_csv_data = data3(),
                   m_cutoff = scale2()
                   )
  })

  ## Independent Samples Data Analysis

  observeEvent(input$submit3, {

    if (is.null(data4())) {
      return(NULL)
    }
    if (is.null(data5())) {
      return(NULL)
    }

    independent_samples(treat_csv_data = data4(),
                        ctrl_csv_data = data5(),
                        m_cutoff = scale3()
                        )
  })

  ## One-way ANCOVA

  observeEvent(input$submit4, {

    if (is.null(data6())) {
      return(NULL)
    }
    if (is.null(data7())) {
      return(NULL)
    }
    if (is.null(data8())) {
      return(NULL)
    }
    if (is.null(data9())) {
      return(NULL)
    }

    one_way_ancova(treat_pre_csv_data = data6(),
                   treat_post_csv_data = data7(),
                   ctrl_pre_csv_data = data8(),
                   ctrl_post_csv_data = data9(),
                   m_cutoff = scale4()
                   )
  })

  ## One-way Repeated Measures ANOVA

  observeEvent(input$submit5, {

    if (is.null(data10())) {
      return(NULL)
    }
    if (is.null(data11())) {
      return(NULL)
    }
    if (is.null(data12())) {
      return(NULL)
    }

    one_way_repeated_anova(treat_pre_csv_data = data10(),
                           treat_post_csv_data = data11(),
                           treat_post2_csv_data = data12(),
                           m_cutoff = scale5()
                           )
  })

  ## Demographic Group Differences

  observe({
    column_names <- names(data())[-1]
    updateSelectInput(session, "column", choices = column_names)
  })

  observeEvent(input$submit6, {

    if (is.null(data13())) {
      return(NULL)
    }
    if (is.null(data14())) {
      return(NULL)
    }

    demo_group_diff(score_csv_data = data13(),
                    group_csv_data = data14(),
                    m_cutoff = scale6(),
                    group_name = input$column
                    )
  })

}

shinyApp(ui, server)
