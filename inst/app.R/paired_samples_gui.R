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

library(DBERlibR)
library(shiny)
library(shinyFiles)

ui <- fluidPage(
  navlistPanel(
    tabPanel("Item Analysis",
             fluidRow(
               column(6, fileInput("file1", "Choose Data File"),
                      actionButton("submit1", "Run Item Analysis")
               ),
               column(3, textInput("text1", "Cutoff Value"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    ),
    tabPanel("Paired Samples Data Analysis",
             fluidRow(
               column(6, fileInput("file2", "Choose Pre-test Data File"),
                      fileInput("file3", "Choose Post-test Data File"),
                      actionButton("submit2", "Run Paired Samples Data Analysis"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(3, textInput("text2", "Cutoff Value"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    ),
    tabPanel("Independent Samples Data Analysis",
             fluidRow(
               column(6, fileInput("file4", "Choose Treatment Group Data File"),
                      fileInput("file5", "Choose Control Group Data File"),
                      actionButton("submit3", "Run Independent Samples Data Analysis"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(3, textInput("text3", "Cutoff Value"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    ),
    tabPanel("One-way ANCOVA",
             fluidRow(
               column(6, fileInput("file6", "Choose Treatment-Group Pre-test Data File"),
                      fileInput("file7", "Choose Treatment Group Post-test Data File"),
                      fileInput("file8", "Choose Control-Group Pre-test Data File"),
                      fileInput("file9", "Choose Control-Group Post-test Data File"),
                      actionButton("submit4", "Run One-way ANCOVA"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(3, textInput("text4", "Cutoff Value"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    ),
    tabPanel("One-way Repeated Measures ANOVA",
             fluidRow(
               column(6, fileInput("file10", "Choose Pre-test Data File"),
                      fileInput("file11", "Choose Post-test Data File"),
                      fileInput("file12", "Choose Post2-test Data File"),
                      actionButton("submit5", "Run One-way Repeated Measures ANOVA"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(3, textInput("text5", "Cutoff Value"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    ),
    tabPanel("Analysis of Demographic Group Differences",
             fluidRow(
               column(6, fileInput("file13", "Choose Assessment Data File"),
                      fileInput("file14", "Choose Demographic Data File"),
                      actionButton("submit6", "Run Analysis of Demographic Group Differences"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               ),
               column(3, textInput("text6", "Cutoff Value"),
                      textInput("text7", "Group Variable Name"),
                      height = c(-100,0,0,0),
                      spacing = c(-100,0,0,0)
               )
             )
    )
  )
)

server <- function(input, output) {
  file1 <- reactive({
    input$file1
  })
  file2 <- reactive({
    input$file2
  })
  file3 <- reactive({
    input$file3
  })
  file4 <- reactive({
    input$file4
  })
  file5 <- reactive({
    input$file5
  })
  file6 <- reactive({
    input$file6
  })
  file7 <- reactive({
    input$file7
  })
  file8 <- reactive({
    input$file8
  })
  file9 <- reactive({
    input$file9
  })
  file10 <- reactive({
    input$file10
  })
  file11 <- reactive({
    input$file11
  })
  file12 <- reactive({
    input$file12
  })
  file13 <- reactive({
    input$file13
  })
  file14 <- reactive({
    input$file14
  })

  text1 <- reactive({
    input$text1
  })
  text2 <- reactive({
    input$text2
  })
  text3 <- reactive({
    input$text3
  })
  text4 <- reactive({
    input$text4
  })
  text5 <- reactive({
    input$text5
  })
  text6 <- reactive({
    input$text6
  })
  text7 <- reactive({
    input$text7
  })

  ## Item Analysis

  observeEvent(input$submit1, {
    if (is.null(file1())) {
      return(NULL)
    }
    data1 <- file1()
    item_analysis(score_csv_data = data1$datapath,
                  m_cutoff = text1()
                  )
  })

  ## Paired Samples Data Analysis

  observeEvent(input$submit2, {

    if (is.null(file2())) {
      return(NULL)
    }
    if (is.null(file3())) {
      return(NULL)
    }

    data2 <- file2()
    data3 <- file3()
    paired_samples(pre_csv_data = data2$datapath,
                   post_csv_data = data3$datapath,
                   m_cutoff = text2()
                   )
  })

  ## Independent Samples Data Analysis

  observeEvent(input$submit3, {

    if (is.null(file4())) {
      return(NULL)
    }
    if (is.null(file5())) {
      return(NULL)
    }

    data4 <- file4()
    data5 <- file5()
    independent_samples(treat_csv_data = data4$datapath,
                        ctrl_csv_data = data5$datapath,
                        m_cutoff = text3()
                        )
  })

  ## One-way ANCOVA

  observeEvent(input$submit4, {

    if (is.null(file6())) {
      return(NULL)
    }
    if (is.null(file7())) {
      return(NULL)
    }
    if (is.null(file8())) {
      return(NULL)
    }
    if (is.null(file9())) {
      return(NULL)
    }

    data6 <- file6()
    data7 <- file7()
    data8 <- file8()
    data9 <- file9()
    one_way_ancova(treat_pre_csv_data = data6$datapath,
                   treat_post_csv_data = data7$datapath,
                   ctrl_pre_csv_data = data8$datapath,
                   ctrl_post_csv_data = data9$datapath,
                   m_cutoff = text4()
                   )
  })

  ## One-way Repeated Measures ANOVA

  observeEvent(input$submit5, {

    if (is.null(file10())) {
      return(NULL)
    }
    if (is.null(file11())) {
      return(NULL)
    }
    if (is.null(file12())) {
      return(NULL)
    }

    data10 <- file10()
    data11 <- file11()
    data12 <- file12()
    one_way_repeated_anova(treat_pre_csv_data = data10$datapath,
                           treat_post_csv_data = data11$datapath,
                           treat_post2_csv_data = data12$datapath,
                           m_cutoff = text5()
                           )
  })

  ## Demographic Group Differences

  observeEvent(input$submit6, {

    if (is.null(file13())) {
      return(NULL)
    }
    if (is.null(file14())) {
      return(NULL)
    }

    data13 <- file13()
    data14 <- file14()
    demo_group_diff(score_csv_data = data13$datapath,
                    group_csv_data = data14$datapath,
                    m_cutoff = text6(),
                    group_name = text7()
                    )
  })

}

shinyApp(ui, server)
