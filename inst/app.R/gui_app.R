################################################
## DBERlibR - Automated Assessment Data Analysis
################################################

library(shiny)
library(shinyFiles)
library(rvest)
library(officer)
library(flextable)
library(magrittr)

library(DBERlibR)
library(car)
library(dplyr)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(psych)
library(readr)
library(reshape)
library(rstatix)
library(tibble)

ui <- fluidPage(

  # tags$head(
  #   tags$style(HTML("
  #     .hidden-title-link {
  #       text-indent: -9999px;
  #       display: inline-block;
  #     }
  #   "))
  # ),

  titlePanel(
    div("DBERlibR - Automated Assessnment Data Analysis",
        style = "color = blue; font-size: 24px;")
  ),

  a("Instructions", href = "https://cran.r-project.org/web/packages/DBERlibR/vignettes/dberlibr-vignette.html"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_function",
        "Choose a function:",
        choices = c("Item Analysis",
                    "Paired Samples Data Analysis",
                    "Independent Samples Data Analysis",
                    "One-way ANCOVA",
                    "One-way Repeated Measures ANOVA",
                    "Demographic Group Differences Analysis"
                    )
      ),

      # Item Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Item Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file1", "Choose Data File"),
        sliderInput("scale1", "Cutoff Value", min = 0, max = 1, value = 0.15),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit1", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
        # h5("-----------------", style = "color: gray;"),
        # downloadButton("download_word1", "Download To Word Document")
      ),

      # Paired Samples Data Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Paired Samples Data Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file2", "Choose Pre-test Data File"),
        fileInput("file3", "Choose Post-test Data File"),
        sliderInput("scale2", "Cutoff Value", min = 0, max = 1, value = 0.15),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit2", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # Independent Samples Data Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Independent Samples Data Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file4", "Choose Treatment-group Data File"),
        fileInput("file5", "Choose Control-group Data File"),
        sliderInput("scale3", "Cutoff Value", min = 0, max = 1, value = 0.15),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit3", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # One-way ANCOVA Inputs
      conditionalPanel(
        condition = "input.selected_function == 'One-way ANCOVA'",
        h5("_________________", style = "color: black;"),
        fileInput("file6", "Choose Treatment-group Pre-test Data File"),
        fileInput("file7", "Choose Treatment-group Post-test Data File"),
        fileInput("file8", "Choose Control-group Pre-test Data File"),
        fileInput("file9", "Choose Control-group Post-test Data File"),
        sliderInput("scale4", "Cutoff Value", min = 0, max = 1, value = 0.15),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit4", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # One-way Repeated Measures ANOVA Inputs
      conditionalPanel(
        condition = "input.selected_function == 'One-way Repeated Measures ANOVA'",
        h5("_________________", style = "color: black;"),
        fileInput("file10", "Choose Pre-test Data File"),
        fileInput("file11", "Choose Post-test Data File"),
        fileInput("file12", "Choose Post2-test Data File"),
        sliderInput("scale5", "Cutoff Value", min = 0, max = 1, value = 0.15),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit5", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # Demographic Group Differences Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Demographic Group Differences Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file13", "Choose Assessment Data File"),
        fileInput("file14", "Choose Demographic Data File"),
        sliderInput("scale6", "Cutoff Value", min = 0, max = 1, value = 0.15),
        selectInput("column", "Group Variable Name", choices = NULL),
        # h5("_________________", style = "color: blue;"),
        actionButton("submit6", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      h5("_________________", style = "color: red;"),
      actionButton("reload_button", "Reset", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: red;"),
      # Add JavaScript to trigger page reload
      tags$script(HTML("
        $(document).on('click', '#reload_button', function() {
          window.location.reload();
        });
      ")),
      actionButton("close_app", "Close App", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: Black;"),
    ),

    mainPanel(
      uiOutput("dynamic_panel")
      # htmlOutput("n_students_deleted"),
      # verbatimTextOutput("difficulty_index"),
      # plotOutput("difficulty_index_plot"),
      # htmlOutput("too_difficulty_items"),
      # verbatimTextOutput("discrimination_index"),
      # plotOutput("discrimination_index_plot"),
      # htmlOutput("non_discrimination_items")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$close_app, {
    stopApp()
  })

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


####################################################################

  output$dynamic_panel <- renderUI({
    if (input$selected_function == "Item Analysis") {
      # UI for Item Analysis
      tagList(
        textOutput("error1"),
        h5("", style = "color: green;"),
        htmlOutput("n_students_deleted"),
        verbatimTextOutput("difficulty_index"),
        plotOutput("difficulty_index_plot"),
        htmlOutput("too_difficulty_items"),
        verbatimTextOutput("discrimination_index"),
        plotOutput("discrimination_index_plot"),
        htmlOutput("non_discrimination_items")
      )
    } else if (input$function_choice == "Paired Samples Data Analysis") {
      # UI for Paired Samples Data Analysis
      tagList(
        h3("Paired Samples Data Analysis Output"),
        verbatimTextOutput("function2_output")
      )
    }
  })


####################################################################

  # observeEvent(input$submit1, {
  #   if (is.null(data1())) {
  #     return(NULL)
  #   }
  #   # data1 <- file1()
  #   item_analysis(score_csv_data = data1(),
  #                 m_cutoff = scale1()
  #   )
  # })

  # observeEvent(input$submit1, {
  #   if (is.null(data1())) {
  #     return(NULL)
  #   }
    # data1 <- file1()
    #################################################################
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #

  ##############################
  error1 <- reactive({
    input$submit1
    validate(
      need(input$file1, "Please upload a data file to analyze!")
    )
    paste("The file name uploaded for item analysis: ", input$file1$name)
  })
  output$error1 <- renderText({
    error1()
  })
  ##############################

  # error1 <- reactive({
  #   input$submit1
  #   validate(
  #     need(isolate(input$submit1) && input$file1, "Please upload a data file to analyze!")
  #   )
  #   paste("The file name uploaded for item analysis: ", input$file1$name)
  # })
  # output$error1 <- renderText({
  #   error1()
  # })


  observeEvent(input$submit1, {
    #   if (is.null(data1())) {
    #     return(NULL)
    #   }

    # req(input$submit1)

    validate(
      need(input$file1, "Please upload a data file to analyze!")
    )

    output$error1 <- renderText({
      paste("The file name uploaded for item analysis: ", input$file1$name)
    })

    # binding for global variable
    m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
      geom_point() +
      geom_vline(xintercept = 0.2, color ="red") +
      xlab("Relationship Coefficient") +
      ylab("Question Item Number") +
      ggtitle("Discrimination Plot") +
      theme_minimal() +
      geom_text_repel(aes(label = round(avg_score,2)))

    # Create data subset: discrimination index lower than 0.2
    qnumber <- colnames(discrimination_data)
    discrimination_index <- round(discrimination_data$avg_score,2)
    discrimination_index <- data.frame(qnumber,discrimination_index)
    nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
    nondiscriminantitems_nrow <- nrow(nondiscriminantitems)

    ##### present the results in the console for the convenience of users
    message("==============================")
    message("The number of students deleted: ",n_students_deleted, " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
    message("", sep="\n")
    message("==================================")
    message("Item Analysis Results - Difficulty")
    print(difficulty_index)
    plot(difficulty_index_plot)
    message("Refer to 'Difficulty Plot' in the 'Plots' panel.", sep="\n")
    too_difficulty_items <- if (toodifficultitems_nrow > 0) {
      message("As seen in the difficulty plot, the following question items present a difficulty plot lower than:")
      print(toodifficultitems$Q_id)
    } else {
      message("As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", sep="\n")
    }
    message("======================================")
    message("Item Analysis Results - Discrimination")
    print(discrimination_index)
    plot(discrimination_index_plot, sep="\n")
    message("Refer to 'Discrimination Plot' in the 'Plots' panel", sep="\n")
    non_discrimination_items <- if (nondiscriminantitems_nrow > 0) {
      message("As seen in the discrimination plot, the following question items present a discrimination index lower than 0.2:")
      print(nondiscriminantitems$qnumber)
    } else {
      message("As seen in the discrimination plot, None of the discrimination indixes was found to be lower than 0.2", sep="\n")
    }
    #################################################################
  })

  item_analysis <- eventReactive(input$submit1, {
    # if (is.null(data1())) {
    #   return(NULL)
    # }
    # data1 <- file1()
    #################################################################
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
      geom_point() +
      geom_vline(xintercept = 0.2, color ="red") +
      xlab("Relationship Coefficient") +
      ylab("Question Item Number") +
      ggtitle("Discrimination Plot") +
      theme_minimal() +
      geom_text_repel(aes(label = round(avg_score,2)))

    # Create data subset: discrimination index lower than 0.2
    qnumber <- colnames(discrimination_data)
    discrimination_index <- round(discrimination_data$avg_score,2)
    discrimination_index <- data.frame(qnumber,discrimination_index)
    nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
    nondiscriminantitems_nrow <- nrow(nondiscriminantitems)

  })

  n_students_deleted <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    nrow_all-nrow_subset
  })

  difficulty_index <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    # suppressWarnings({
    difficulty_index <- data_original[,c(-1)] %>%
      colMeans() %>%
      t() %>%
      as.data.frame() %>%
      t()
    difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
      dplyr::select(-X2)
    colnames(difficulty_index) = c("q.number","difficulty_index")
    return(difficulty_index)
  })

  difficulty_index_plot <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })
    return(difficulty_index_plot)
  })

  toodifficultitems <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    subset(difficulty_index, difficulty_index < 0.2)
  })

  toodifficultitems_nrow <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    nrow(toodifficultitems)
  })

  discrimination_index <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    qnumber <- colnames(discrimination_data)
    discrimination_index <- round(discrimination_data$avg_score,2)
    discrimination_index <- data.frame(qnumber,discrimination_index)
    return(discrimination_index)
  })

  discrimination_index_plot <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
      geom_point() +
      geom_vline(xintercept = 0.2, color ="red") +
      xlab("Relationship Coefficient") +
      ylab("Question Item Number") +
      ggtitle("Discrimination Plot") +
      theme_minimal() +
      geom_text_repel(aes(label = round(avg_score,2)))
    return(discrimination_index_plot)
  })

  nondiscriminantitems <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
      geom_point() +
      geom_vline(xintercept = 0.2, color ="red") +
      xlab("Relationship Coefficient") +
      ylab("Question Item Number") +
      ggtitle("Discrimination Plot") +
      theme_minimal() +
      geom_text_repel(aes(label = round(avg_score,2)))

    # Create data subset: discrimination index lower than 0.2
    qnumber <- colnames(discrimination_data)
    discrimination_index <- round(discrimination_data$avg_score,2)
    discrimination_index <- data.frame(qnumber,discrimination_index)
    subset(discrimination_index, discrimination_index < 0.2)
  })

  nondiscriminantitems_nrow <- reactive({
    # item_analysis <- function(score_csv_data, scale1() = 0.15) {
    #
    # binding for global variable
    score_csv_data <- m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Deleting students with too many skipped answers-----------------
    nrow_all <- nrow(data_original)
    n <- as.numeric(length(data_original[,-1]))
    data_original <- data_original %>%
      mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

    data_original <- subset(data_original, m_rate < scale1())
    nrow_subset <- nrow(data_original)
    n_students_deleted <- nrow_all-nrow_subset

    # Delete the column of "m_rate" to exclude from the analysis down the road.
    data_original$m_rate=NULL

    # Clean data
    n_col <- ncol(data_original)
    for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
    data_original[is.na(data_original)]= 0
    na.sum <- sum(is.na(data_original))
    # Calculate average scores
    data_original <- data_original %>%
      mutate(avg_score = rowMeans(data_original[,-1]))

    # ggplots: difficulty index
    suppressWarnings({
      difficulty_index <- data_original[,c(-1)] %>%
        colMeans() %>%
        t() %>%
        as.data.frame() %>%
        t()
      difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
        dplyr::select(-X2)
      colnames(difficulty_index) = c("q.number","difficulty_index")
      difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
        geom_point(alpha=0.9) +
        geom_vline(xintercept = 0.2, color ="red")+
        geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
        ggtitle("Difficulty Plot") +
        xlab("Proportion") +
        ylab("Question Item Number") +
        theme_minimal() +
        geom_text_repel(aes(label = round(difficulty_index,2)))
    })

    # Create data subset: difficulty index lower than 0.2
    toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
    toodifficultitems_nrow <- nrow(toodifficultitems)

    # ggplots: discrimination index
    discrimination_data <- cor(data_original[,-1]) %>%
      as.data.frame()
    discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
      geom_point() +
      geom_vline(xintercept = 0.2, color ="red") +
      xlab("Relationship Coefficient") +
      ylab("Question Item Number") +
      ggtitle("Discrimination Plot") +
      theme_minimal() +
      geom_text_repel(aes(label = round(avg_score,2)))

    # Create data subset: discrimination index lower than 0.2
    qnumber <- colnames(discrimination_data)
    discrimination_index <- round(discrimination_data$avg_score,2)
    discrimination_index <- data.frame(qnumber,discrimination_index)
    nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
    nrow(nondiscriminantitems)
  })

  ################################################################################

  output$n_students_deleted <- renderUI({
    item_analysis()
    n_students_deleted <-c("======================================",
                           paste("The number of students deleted: ", as.numeric(n_students_deleted()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""),
                           "======================================",
                           paste("Item Analysis Results - Difficulty", sep=""))
    HTML(paste(n_students_deleted, collapse = "<br/>"))
  })

  output$difficulty_index <- renderPrint({
    item_analysis()
    suppressWarnings({
      difficulty_index()
    })
  })

  output$difficulty_index_plot <- renderPlot({
    item_analysis()
    difficulty_index_plot()
  })

  output$too_difficulty_items <- renderUI({
    item_analysis()
    if (toodifficultitems_nrow() > 0) {
      too_difficulty_items <- c(paste("As seen in the difficulty plot, the following question items present a difficulty plot lower than: ", toodifficultitems()$Q_id, sep=""),
                                "======================================",
                                paste("Item Analysis Results - Discrimination", sep=""))
    } else {
      too_difficulty_items <- c(paste("As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", sep=""),
                                "======================================",
                                paste("Item Analysis Results - Discrimination", sep=""))
    }
    HTML(paste(too_difficulty_items, collapse = "<br/>"))
  })

  output$discrimination_index <- renderPrint({
    item_analysis()
    # suppressWarnings({
    discrimination_index()
    # })
  })

  output$discrimination_index_plot <- renderPlot({
    item_analysis()
    discrimination_index_plot()
  })

  output$non_discrimination_items <- renderUI({
    item_analysis()
    if (nondiscriminantitems_nrow() > 0) {
      non_discrimination_items <- c(paste("As seen in the discrimination plot, the following question(s) items present(s) a discrimination index lower than 0.2: "),
                                    nondiscriminantitems()$qnumber)
    } else {
      non_discrimination_items <- c(paste("As seen in the discrimination plot, None of the discrimination indixes was found to be lower than 0.2", sep="\n"))
    }
    HTML(paste(non_discrimination_items, collapse = ","))
  })
#
#   ######################################################
#   output$download_word1 <- downloadHandler(
#     filename = function() {
#       "item_analysis_output.docx"
#     },
#     content = function(file) {
#       # Create a Word document
#       doc <- read_docx()
#
#
#       #____________________________________________________________
#       # binding for global variable
#       m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL
#
#       # Reading
#       data_original <- read_csv(data1(), col_types = cols())
#
#       # Deleting students with too many skipped answers-----------------
#       nrow_all <- nrow(data_original)
#       n <- as.numeric(length(data_original[,-1]))
#       data_original <- data_original %>%
#         mutate(m_rate = round(rowSums(is.na(data_original))/n,3))
#
#       data_original <- subset(data_original, m_rate < scale1())
#       nrow_subset <- nrow(data_original)
#       n_students_deleted <- nrow_all-nrow_subset
#
#       # Delete the column of "m_rate" to exclude from the analysis down the road.
#       data_original$m_rate=NULL
#
#       # Clean data
#       n_col <- ncol(data_original)
#       for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
#       data_original[is.na(data_original)]= 0
#       na.sum <- sum(is.na(data_original))
#       # Calculate average scores
#       data_original <- data_original %>%
#         mutate(avg_score = rowMeans(data_original[,-1]))
#
#       # ggplots: difficulty index
#       suppressWarnings({
#         difficulty_index <- data_original[,c(-1)] %>%
#           colMeans() %>%
#           t() %>%
#           as.data.frame() %>%
#           t()
#         difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
#           dplyr::select(-X2)
#         colnames(difficulty_index) = c("q.number","difficulty_index")
#         difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
#           geom_point(alpha=0.9) +
#           geom_vline(xintercept = 0.2, color ="red")+
#           geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
#           ggtitle("Difficulty Plot") +
#           xlab("Proportion") +
#           ylab("Question Item Number") +
#           theme_minimal() +
#           geom_text_repel(aes(label = round(difficulty_index,2)))
#       })
#
#       # Create data subset: difficulty index lower than 0.2
#       toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
#       toodifficultitems_nrow <- nrow(toodifficultitems)
#
#       # ggplots: discrimination index
#       discrimination_data <- cor(data_original[,-1]) %>%
#         as.data.frame()
#       discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
#         geom_point() +
#         geom_vline(xintercept = 0.2, color ="red") +
#         xlab("Relationship Coefficient") +
#         ylab("Question Item Number") +
#         ggtitle("Discrimination Plot") +
#         theme_minimal() +
#         geom_text_repel(aes(label = round(avg_score,2)))
#
#       # Create data subset: discrimination index lower than 0.2
#       qnumber <- colnames(discrimination_data)
#       discrimination_index <- round(discrimination_data$avg_score,2)
#       discrimination_index <- data.frame(qnumber,discrimination_index)
#       nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
#       nondiscriminantitems_nrow <- nrow(nondiscriminantitems)
#       #_______________________________________________________________
#
#
#       # Add text
#       doc <- body_add_par(doc, difficulty_index, style = "Normal")
#
#       # Add table
#       # sample_table <- data.frame(A = 1:5, B = 6:10)
#       # ft <- flextable(sample_table)
#       # doc <- body_add_flextable(doc, ft)
#
#       # Add plot
#       # sample_plot <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_point()
#       temp_file <- tempfile(fileext = ".png")
#       ggsave(temp_file, difficulty_index_plot, width = 6, height = 4)
#       doc <- body_add_img(doc, src = temp_file, width = 6, height = 4)
#
#       # Save the Word document
#       print(doc, target = file)
#     }
#   )
# ############################################################################

  # observeEvent(input$reset_button1, {
  #   output$n_students_deleted <- renderUI({
  #     NULL
  #   })
  #   output$difficulty_index <- renderPrint({
  #     NULL
  #   })
  #   output$difficulty_index_plot <- renderPlot({
  #     NULL
  #   })
  #   output$too_difficulty_items <- renderUI({
  #     NULL
  #   })
  #   output$discrimination_index <- renderPrint({
  #     NULL
  #   })
  #   output$discrimination_index_plot <- renderPlot({
  #     NULL
  #   })
  #   output$non_discrimination_items <- renderUI({
  #     NULL
  #   })
  # })



#
#   ## Paired Samples Data Analysis
#
#   observeEvent(input$submit2, {
#
#     if (is.null(data2())) {
#       return(NULL)
#     }
#     if (is.null(data3())) {
#       return(NULL)
#     }
#
#     paired_samples(pre_csv_data = data2(),
#                    post_csv_data = data3(),
#                    m_cutoff = scale2()
#                    )
#   })
#

  paired_samples_return <- eventReactive(input$submit2, {
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


    observeEvent(input$submit2, {
      if (is.null(data2())) {
        return(NULL)
      }
      if (is.null(data3())) {
        return(NULL)
      }

  # paired_samples <- function(pre_csv_data, post_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale2())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale2())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

  #----------------------------------------------------------------

  n <- as.numeric(length(data_treat_pre[,-1]))
  n=n-1

  # Clean data (e.g., Replace skipped answers with "0")
  n_col <- ncol(data_treat_pre)
  for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
  data_treat_pre[is.na(data_treat_pre)]= 0
  na.sum_pre <- sum(is.na(data_treat_pre))
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0

  # Change column names
  colnames(data_treat_pre) <- paste(colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste(colnames(data_treat_post), "post", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>%
    mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))

  # Merge pre/post data
  treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
  treat_data_merged <- treat_data_merged %>%
    mutate(avg_diff=avg_score_post-avg_score_pre)
  Mean_Differences <- treat_data_merged$avg_diff

  # Convert Data Frame to a Long Format & Define the Variable
  avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post)
  df_Long <- melt(avg_score_df)
  names(df_Long) <- c("id", "Time", "Score")
  # Name Time(group) -> 1=Pre, 2=Post
  df_Long$Time <- factor(df_Long$Time,levels=c(1,2),labels=c("Pre", "Post"))
  # Get descriptive statistics
  descriptive_statistics <- df_Long %>%
    group_by(Time) %>%
    get_summary_stats(Score, type="common")
  boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Paired Samples - Boxplots")

  #Check Assumptions
  #No Outliers
  Outliers <- treat_data_merged %>%
    identify_outliers(avg_diff)
  # Normality
  shapiro_wilk_test <- treat_data_merged %>%
    shapiro_test(avg_diff)
  normal_qq_plot <- ggqqplot(treat_data_merged, "avg_diff", title="Normal Q-Q Plot")

  # Run paired samples t-test (two-sided)
  paired_samples_t_test <- t.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, mu=0, alt="two.sided", paired=T, conf.level=0.95)
  ttest_p.value <- paired_samples_t_test$p.value

  # Run paired samples t-test (two-sided)
  suppressWarnings({
    wilcoxon_signed_rank_test <- wilcox.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, paired=T, conf.level=0.95)
  })

  ####### Print the results in the console for the conveniece of the user

  message("======================")
  message("--> ",n_students_deleted_pre, " student(s) from the pre-test data and ", n_students_deleted_post, " student(s) from the post-test data have been deleted since they have more than ",as.numeric(scale2())*100,"% of skipped answers.")
  message("", sep="\n")
  message("=================================================")
  message("Pre-test/Post-test Scores' Descriptive Statistics")
  print(descriptive_statistics)
  message("Refer to the boxplot in the 'Plots' panel to visually examine the descriptive statistics.", sep="\n")
  print(boxplots)
  message("===================================")
  message("Testing the Assumption of Normality")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p > 0.05) {
    message("## Interpretation: the assumption of normality by group has been met (p>0.05)")
  } else {
    message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05)")
  }
  message("Refer to the histogram and normal q-q plot to check the normality visually", sep="\n")
  hist(Mean_Differences)
  print(normal_qq_plot)
  message("=====================")
  message("Paired T-Test Results")
  print(paired_samples_t_test)
  message("## Sample Interpretation of the outputs above:")
  if (paired_samples_t_test$p.value < 0.05) {
    message("--> The average pre-test score was ",round(descriptive_statistics$mean[1],2),
            " and the average post-test score was ",round(descriptive_statistics$mean[2],2),". ",
            "The Paired Samples T-Test showed that the pre-post difference was statistically significant (p=",
            round(paired_samples_t_test$p.value,3),").")
  } else {
    message("--> The average pre-test score was ",round(descriptive_statistics$mean[1],2),
            " and the average post-test score was ",round(descriptive_statistics$mean[2],2),". ",
            "The Paired Samples T-Test showed that the pre-post difference was NOT statistically significant (p=",
            round(paired_samples_t_test$p.value,3),").")
  }
  # Check the condition of data to determine the need to run a non-parametric analysis
  message("", sep="\n")
  message("_______________________________________")
  sample_size <- nrow(treat_data_merged)
  shapiro_wilk_test <- shapiro.test(treat_data_merged$avg_diff)
  if (sample_size < 15 & shapiro_wilk_test$p.value < 0.05) {
    message("## The sample size of ", sample_size, " is small, and the Shapiro-Wilk test result shows a violation of normality assumption (p=",shapiro_wilk_test$p.value,"). Although the t-test is robust to a small sample size and a violation of the normality assumption, you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size < 15 & shapiro_wilk_test$p.value > 0.05) {
    message("## The sample size of ", sample_size, " may be too small to safely interpret the parametric t-test results above. Although the t-test is known to be robust to a small sample size you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size >= 15 & shapiro_wilk_test$p.value<0.05) {
    message("## The Shapiro-Wilk test result shows a violation of normality assumption (p=",round(shapiro_wilk_test$p.value,3),"). Although the t-test is known to be robust to a violation of the normality assumption, you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size < 15 | shapiro_wilk_test$p.value < 0.05) {
    message("=====================================")
    message("Wilcoxon Signed Rank Sum Test Results")
    print(wilcoxon_signed_rank_test)
    message("## A sample interpretation of the Wilcoxon signed rank test results above:")
    if (wilcoxon_signed_rank_test$p.value < 0.05) {
      message("--> The Wilcoxon signed rank test results above show that the pre-post difference was statistically significant (p=",
              round(wilcoxon_signed_rank_test$p.value,3),").")
    } else {
      message("--> The Wilcoxon signed rank test results above show that the pre-post difference was NOT statistically significant (p=",
              round(wilcoxon_signed_rank_test$p.value,3),").")
    }
  }
  message("", sep="\n")

  # Rename variables for return

  # Return values for testing the function
  out <- tibble::lst(
    n_students_deleted,
    shapiro_wilk_test,
    normal_qq_plot,
    descriptive_statistics,
    boxplots,
    paired_samples_t_test,
    wilcoxon_signed_rank_test,
  )
  return(invisible(out))

  })


#   ## Independent Samples Data Analysis
#
#   observeEvent(input$submit3, {
#
#     if (is.null(data4())) {
#       return(NULL)
#     }
#     if (is.null(data5())) {
#       return(NULL)
#     }
#
#     independent_samples(treat_csv_data = data4(),
#                         ctrl_csv_data = data5(),
#                         m_cutoff = scale3()
#                         )
#   })
#
#   ## One-way ANCOVA
#
#   observeEvent(input$submit4, {
#
#     if (is.null(data6())) {
#       return(NULL)
#     }
#     if (is.null(data7())) {
#       return(NULL)
#     }
#     if (is.null(data8())) {
#       return(NULL)
#     }
#     if (is.null(data9())) {
#       return(NULL)
#     }
#
#     one_way_ancova(treat_pre_csv_data = data6(),
#                    treat_post_csv_data = data7(),
#                    ctrl_pre_csv_data = data8(),
#                    ctrl_post_csv_data = data9(),
#                    m_cutoff = scale4()
#                    )
#   })
#
#   ## One-way Repeated Measures ANOVA
#
#   observeEvent(input$submit5, {
#
#     if (is.null(data10())) {
#       return(NULL)
#     }
#     if (is.null(data11())) {
#       return(NULL)
#     }
#     if (is.null(data12())) {
#       return(NULL)
#     }
#
#     one_way_repeated_anova(treat_pre_csv_data = data10(),
#                            treat_post_csv_data = data11(),
#                            treat_post2_csv_data = data12(),
#                            m_cutoff = scale5()
#                            )
#   })
#
#   ## Demographic Group Differences
#
#   observe({
#     column_names <- names(data())[-1]
#     updateSelectInput(session, "column", choices = column_names)
#   })
#
#   observeEvent(input$submit6, {
#
#     if (is.null(data13())) {
#       return(NULL)
#     }
#     if (is.null(data14())) {
#       return(NULL)
#     }
#
#     demo_group_diff(score_csv_data = data13(),
#                     group_csv_data = data14(),
#                     m_cutoff = scale6(),
#                     group_name = input$column
#                     )
#   })

}

shinyApp(ui, server)
