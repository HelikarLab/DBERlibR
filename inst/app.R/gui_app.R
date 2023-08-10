################################################
## DBERlibR - Automated Assessment Data Analysis
################################################

library(shiny)
library(webshot)
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
      tags$head(
        tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
        tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")
      ),

      tags$label(title = "Select a function for the analysis you want to perform", "Choose an Action:"),
      selectizeInput(
        "selected_function",
        "",
        choices = c("Item Analysis",
                    "Paired Samples Data Analysis",
                    "Independent Samples Data Analysis",
                    "One-way ANCOVA",
                    "One-way Repeated Measures ANOVA",
                    "Demographic Group Differences Analysis"
        ),
        options = list(render = I("{
              item: function(item, escape) {
                var tooltips = {
                  'Item Analysis': 'This function provides item difficulty and item discrimination scores, which helps improve the question items for better assessment.',
                  'Paired Samples Data Analysis': 'This function helps compare pre-test and post-test scores in the same group.',
                  'Independent Samples Data Analysis': 'This function analyzes the difference between two different groups (e.g., treatment and control groups)).',
                  'One-way ANCOVA': 'This function analyzes the changes over time in the treatment group with a control group.',
                  'One-way Repeated Measures ANOVA': 'This function helps examine the changes over time (e.g., the difference among pre-, post-, and post2-test scores.',
                  'Demographic Group Differences Analysis': 'This function helps investigate the difference among demographic sub-groups (e.g., gender, grade)'
                };
                var tooltipText = tooltips[item.label];
                return '<div title=\"' + tooltipText + '\">' + escape(item.label) + '</div>';
              },
              option: function(item, escape) {
                var tooltips = {
                  'Item Analysis': 'This analysis provides item difficulty and item discrimination scores, which helps improve the question items for better assessment.',
                  'Paired Samples Data Analysis': 'This is the description for paired samples data analysis',
                  'Independent Samples Data Analysis': 'This is the description for independent samples data analysis',
                  'One-way ANCOVA': 'This is the description for one-way ANCOVA',
                  'One-way Repeated Measures ANOVA': 'This is the description for one-way repeated measures ANOVA',
                  'Demographic Group Differences Analysis': 'This is the description for demographic group difference analysis'
                };
                var tooltipText = tooltips[item.label];
                return '<div title=\"' + tooltipText + '\">' + escape(item.label) + '</div>';
              }
            }"))
          ),
      tags$script(HTML("
        $(document).ready(function() {
          $('#my_dropdown').on('mouseover', 'div', function() {
            $(this).tooltip({placement: 'bottom', trigger: 'hover'});
            $(this).tooltip('show');
          });
        });
      ")),
      # Item Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Item Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file1", "Choose Data File"),
        tags$label(title = "This is to argue for excluding students with too may skipped answers.", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "To convert multiple-choice answers to a binary format for this analysis. You have to select 'Yes' and provide an answer key file if you input data with multiple-choice answer.", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice1", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
          conditionalPanel(condition = "input.m_choice1 == 1",
          fileInput("file_key1", "Choose Answer Key File")
          ),
        h5(".", style = "color: white;"),
        actionButton("submit1", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
        # h5("_________________", style = "color: black;"),
        # h5(".", style = "color: white;"),
        # downloadButton("download_word1", "Download To Word Document")
      ),

      # Paired Samples Data Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Paired Samples Data Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file2", "Choose Pre-test Data File"),
        fileInput("file3", "Choose Post-test Data File"),
        tags$label(title = "Select a cutoff value", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "If you use a data with multiple-choice answers~~~", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice2", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
        conditionalPanel(condition = "input.m_choice2 == 1",
                         fileInput("file_key2", "Choose Answer Key File")
        ),
        h5(".", style = "color: white;"),
        actionButton("submit2", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # Independent Samples Data Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Independent Samples Data Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file4", "Choose Treatment-group Data File"),
        fileInput("file5", "Choose Control-group Data File"),
        tags$label(title = "Select a cutoff value", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "If you use a data with multiple-choice answers~~~", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice3", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
        conditionalPanel(condition = "input.m_choice3 == 1",
                         fileInput("file_key3", "Choose Answer Key File")
        ),
        h5(".", style = "color: white;"),
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
        tags$label(title = "Select a cutoff value", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "If you use a data with multiple-choice answers~~~", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice4", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
        conditionalPanel(condition = "input.m_choice4 == 1",
                         fileInput("file_key4", "Choose Answer Key File")
        ),
        h5(".", style = "color: white;"),
        actionButton("submit4", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # One-way Repeated Measures ANOVA Inputs
      conditionalPanel(
        condition = "input.selected_function == 'One-way Repeated Measures ANOVA'",
        h5("_________________", style = "color: black;"),
        fileInput("file10", "Choose Pre-test Data File"),
        fileInput("file11", "Choose Post-test Data File"),
        fileInput("file12", "Choose Post2-test Data File"),
        tags$label(title = "Select a cutoff value", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "If you use a data with multiple-choice answers~~~", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice5", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
        conditionalPanel(condition = "input.m_choice5 == 1",
                         fileInput("file_key5", "Choose Answer Key File")
        ),
        h5(".", style = "color: white;"),
        actionButton("submit5", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      # Demographic Group Differences Analysis Inputs
      conditionalPanel(
        condition = "input.selected_function == 'Demographic Group Differences Analysis'",
        h5("_________________", style = "color: black;"),
        fileInput("file13", "Choose Assessment Data File"),
        fileInput("file14", "Choose Demographic Data File"),
        selectInput("group_var", "Group Variable Name", choices = NULL),
        tags$label(title = "Select a cutoff value", "Cutoff Value:"),
        sliderInput("scale1", "", min = 0, max = 1, value = 0.15),
        tags$label(title = "If you use a data with multiple-choice answers~~~", "Data with Multiple-choice Answers?"),
        radioButtons("m_choice6", label = "", choices = list("No" = 0, "Yes" = 1), selected = 0),
        conditionalPanel(condition = "input.m_choice6 == 1",
                         fileInput("file_key6", "Choose Answer Key File")
        ),
        h5(".", style = "color: white;"),
        actionButton("submit6", "R u n", style = "font-size: 14px;", style = "font-weight: bold;", style = "color: white;", style = "background-color: blue;"),
      ),

      h5(".", style = "color: white;"),
      h5("_________________", style = "color: red;"),
      h5(".", style = "color: white;"),
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
      uiOutput("item_analysis_output"),
      uiOutput("paired_samples_output"),
      uiOutput("independent_samples_output"),
      uiOutput("one_way_ancova_output"),
      uiOutput("one_way_repeated_anova_output"),
      uiOutput("demo_group_diff_output"),
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$close_app, {
    stopApp()
  })

  answer_key1 <- reactive({
    input$file_key1$datapath
  })
  answer_key2 <- reactive({
    input$file_key2$datapath
  })
  answer_key3 <- reactive({
    input$file_key3$datapath
  })
  answer_key4 <- reactive({
    input$file_key4$datapath
  })
  answer_key5 <- reactive({
    input$file_key5$datapath
  })
  answer_key6 <- reactive({
    input$file_key6$datapath
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

  output$item_analysis_output <- renderUI({
    if (input$selected_function == "Item Analysis") {
      # UI for Item Analysis
      tagList(
        textOutput("error1"),
        htmlOutput("n_students_deleted_1"),
        htmlOutput("difficulty_results_title"),
        verbatimTextOutput("difficulty_index"),
        plotOutput("difficulty_index_plot"),
        htmlOutput("too_difficulty_items_explain"),
        htmlOutput("too_difficulty_items"),
        htmlOutput("discrimination_results_title"),
        verbatimTextOutput("discrimination_index"),
        plotOutput("discrimination_index_plot"),
        htmlOutput("non_discrimination_items_explain"),
        htmlOutput("non_discrimination_items"),
        htmlOutput("line_break_1"),
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

    # binding for global variable
    m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

    # Reading
    data_original <- read_csv(data1(), col_types = cols())

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

    # Converting multiple-choice answers to binary
    # req(input$m_choice1)
    if (input$m_choice1 == "1") {
      if (is.null(answer_key1())) {
        stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key1(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
          data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

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

  ## Item Analysis Rendering ##############################################################################

  # Line Break --------------------------------
  output$line_break_1 <- renderUI({
    item_analysis()
    line_break_1 <-c("","")
    HTML(paste(line_break_1, collapse = "<br/>"))
  })
  # output$line_break_2 <- renderUI({
  #   line_break_2 <-c("","")
  #   HTML(paste(line_break_2, collapse = "<br/>"))
  # })
  # output$line_break_3 <- renderUI({
  #   line_break_3 <-c("","")
  #   HTML(paste(line_break_3, collapse = "<br/>"))
  # })
  # output$line_break_4 <- renderUI({
  #   line_break_4 <-c("","")
  #   HTML(paste(line_break_4, collapse = "<br/>"))
  # })
  # output$line_break_5 <- renderUI({
  #   line_break_5 <-c("","")
  #   HTML(paste(line_break_5, collapse = "<br/>"))
  # })
  # output$line_break_6 <- renderUI({
  #   line_break_6 <-c("","")
  #   HTML(paste(line_break_6, collapse = "<br/>"))
  # })
  # output$line_break_7 <- renderUI({
  #   line_break_7 <-c("","")
  #   HTML(paste(line_break_7, collapse = "<br/>"))
  # })
  # output$line_break_8 <- renderUI({
  #   line_break_8 <-c("","")
  #   HTML(paste(line_break_8, collapse = "<br/>"))
  # })
  # output$line_break_9 <- renderUI({
  #   line_break_9 <-c("","")
  #   HTML(paste(line_break_9, collapse = "<br/>"))
  # })
  # output$line_break_10 <- renderUI({
  #   line_break_10 <-c("","")
  #   HTML(paste(line_break_10, collapse = "<br/>"))
  # })
  # Line Break --------------------------------

  # output$n_students_deleted <- renderUI({
  #   item_analysis()
  #   n_students_deleted <-c("",
  #                          "======================================",
  #                          paste("The number of students deleted: ", as.numeric(n_students_deleted()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""),
  #                          "",
  #                          "======================================",
  #                          paste("Item Analysis Results - Difficulty", sep=""))
  #   HTML(paste(n_students_deleted, collapse = "<br/>"))
  # })

  output$n_students_deleted_1 <- renderUI({
    item_analysis()
    n_students_deleted_1 <-c("",
                           "======================================",
                           paste("The number of students deleted: ", as.numeric(n_students_deleted()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
    HTML(paste(n_students_deleted_1, collapse = "<br/>"))
  })

  # output$n_students_deleted_1 <- renderPrint({
  #   item_analysis()
  #   paste0("", "\n",
  #          "======================================", "\n",
  #          "The number of students deleted: ", as.numeric(n_students_deleted()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep="")
  # })

  output$difficulty_results_title <- renderUI({
    item_analysis()
    difficulty_results_title <-c("",
                                 "======================================",
                           paste("Item Analysis Results - Difficulty", sep=""))
    HTML(paste(difficulty_results_title, collapse = "<br/>"))
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

  output$too_difficulty_items_explain <- renderUI({
    item_analysis()
    too_difficulty_items_explain <-c("Item difficulty refers to the proportion of students that correctly answered the item, scaled 0 to 1. The plot displays a vertical red-colored line that represents the threshold of 0.2 so that users can instantly find the question items that warrant users’ attention for improvement.")
    HTML(paste(too_difficulty_items_explain, collapse = "<br/>"))
  })

  output$too_difficulty_items <- renderUI({
    item_analysis()
    if (toodifficultitems_nrow() > 0) {
      too_difficulty_items <- c(paste("As seen in the difficulty plot, the following question items present a difficulty plot lower than: ", toodifficultitems()$Q_id, sep=""))
    } else {
      too_difficulty_items <- c(paste("As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", sep=""))
    }
    HTML(paste(too_difficulty_items, collapse = "<br/>"))
  })

  output$discrimination_results_title <- renderUI({
    item_analysis()
    discrimination_results_title <- c("",
                                "======================================",
                                paste("Item Analysis Results - Discrimination", sep=""))
    HTML(paste(discrimination_results_title, collapse = "<br/>"))
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

  output$non_discrimination_items_explain <- renderUI({
    item_analysis()
    non_discrimination_items_explain <-c("The item discrimination represents the relationship between how well students did on the item and their total test performance, ranging from 0 to 1. The plot displays a vertical red-colored line that represents the threshold of 0.2 so that users can instantly find the question items that warrant users’ attention for improvement.")
    HTML(paste(non_discrimination_items_explain, collapse = "<br/>"))
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

  # ######################################################
  # output$download_word1 <- downloadHandler(
  #   filename = function() {
  #     "item_analysis_output.docx"
  #   },
  #   content = function(file) {
  #     # Create a Word document
  #     doc <- read_docx()
  #
  #
  #     #____________________________________________________________
  #     # binding for global variable
  #     m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL
  #
  #     # Reading
  #     data_original <- read_csv(data1(), col_types = cols())
  #
  #     # Deleting students with too many skipped answers-----------------
  #     nrow_all <- nrow(data_original)
  #     n <- as.numeric(length(data_original[,-1]))
  #     data_original <- data_original %>%
  #       mutate(m_rate = round(rowSums(is.na(data_original))/n,3))
  #
  #     data_original <- subset(data_original, m_rate < scale1())
  #     nrow_subset <- nrow(data_original)
  #     n_students_deleted <- nrow_all-nrow_subset
  #
  #     # Delete the column of "m_rate" to exclude from the analysis down the road.
  #     data_original$m_rate=NULL
  #
  #     # Clean data
  #     n_col <- ncol(data_original)
  #     for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
  #     data_original[is.na(data_original)]= 0
  #     na.sum <- sum(is.na(data_original))
  #     # Calculate average scores
  #     data_original <- data_original %>%
  #       mutate(avg_score = rowMeans(data_original[,-1]))
  #
  #     # ggplots: difficulty index
  #     suppressWarnings({
  #       difficulty_index <- data_original[,c(-1)] %>%
  #         colMeans() %>%
  #         t() %>%
  #         as.data.frame() %>%
  #         t()
  #       difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
  #         dplyr::select(-X2)
  #       colnames(difficulty_index) = c("q.number","difficulty_index")
  #       difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
  #         geom_point(alpha=0.9) +
  #         geom_vline(xintercept = 0.2, color ="red")+
  #         geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
  #         ggtitle("Difficulty Plot") +
  #         xlab("Proportion") +
  #         ylab("Question Item Number") +
  #         theme_minimal() +
  #         geom_text_repel(aes(label = round(difficulty_index,2)))
  #     })
  #
  #     # Create data subset: difficulty index lower than 0.2
  #     toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
  #     toodifficultitems_nrow <- nrow(toodifficultitems)
  #
  #     # ggplots: discrimination index
  #     discrimination_data <- cor(data_original[,-1]) %>%
  #       as.data.frame()
  #     discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
  #       geom_point() +
  #       geom_vline(xintercept = 0.2, color ="red") +
  #       xlab("Relationship Coefficient") +
  #       ylab("Question Item Number") +
  #       ggtitle("Discrimination Plot") +
  #       theme_minimal() +
  #       geom_text_repel(aes(label = round(avg_score,2)))
  #
  #     # Create data subset: discrimination index lower than 0.2
  #     qnumber <- colnames(discrimination_data)
  #     discrimination_index <- round(discrimination_data$avg_score,2)
  #     discrimination_index <- data.frame(qnumber,discrimination_index)
  #     nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
  #     nondiscriminantitems_nrow <- nrow(nondiscriminantitems)
  #     #_______________________________________________________________
  #
  #     # Add outputs to the Word doc
  #     doc <- body_add_par(doc, "======================================", style = "Normal")
  #     doc <- body_add_par(doc, "The number of students deleted: 0 student(s) has(have) been deleted from the data since they have more than 15% of skipped answers.", style = "Normal")
  #     doc <- body_add_par(doc, "", style = "Normal")
  #     doc <- body_add_par(doc, "======================================", style = "Normal")
  #     doc <- body_add_par(doc, "Item Analysis Results - Difficulty", style = "Normal")
  #
  #     ft_1 <- flextable(difficulty_index)
  #     doc <- body_add_flextable(doc, ft_1)
  #
  #     temp_file_1 <- tempfile(fileext = ".png")
  #     ggsave(temp_file_1, difficulty_index_plot, width = 6, height = 4)
  #     doc <- body_add_img(doc, src = temp_file_1, width = 6, height = 4)
  #
  #     doc <- body_add_par(doc, "Item difficulty refers to the proportion of students that correctly answered the item, scaled 0 to 1. The plot displays a vertical red-colored line that represents the threshold of 0.2 so that users can instantly find the question items that warrant users’ attention for improvement.", style = "Normal")
  #     doc <- body_add_par(doc, "As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", style = "Normal")
  #
  #     doc <- body_add_par(doc, "", style = "Normal")
  #     doc <- body_add_par(doc, "======================================", style = "Normal")
  #     doc <- body_add_par(doc, "Item Analysis Results - Discrimination", style = "Normal")
  #
  #     ft_2 <- flextable(discrimination_index)
  #     doc <- body_add_flextable(doc, ft_2)
  #
  #     temp_file_2 <- tempfile(fileext = ".png")
  #     ggsave(temp_file_2, discrimination_index_plot, width = 6, height = 4)
  #     doc <- body_add_img(doc, src = temp_file_2, width = 6, height = 4)
  #
  #     doc <- body_add_par(doc, "The item discrimination represents the relationship between how well students did on the item and their total test performance, ranging from 0 to 1. The plot displays a vertical red-colored line that represents the threshold of 0.2 so that users can instantly find the question items that warrant users’ attention for improvement.", style = "Normal")
  #     doc <- body_add_par(doc, "As seen in the discrimination plot, the following question(s) items present(s) a discrimination index lower than 0.2: ,Q01,Q02,Q03,Q08,Q10,Q12,Q14,Q16,Q17,Q22", style = "Normal")
  #
  #     # Save the Word document
  #     print(doc, target = file)
  #   }
  # )
############################################################################
## Paired Samples Data Analysis
######################################################################

  output$paired_samples_output <- renderUI({
    if (input$selected_function == "Paired Samples Data Analysis") {
      # UI for Paired Samples Data Analysis
      tagList(
        textOutput("error2_1"),
        textOutput("error2_2"),
        htmlOutput("n_students_deleted_2"),
        htmlOutput("descriptive_statistics_title_2"),
        tableOutput("descriptive_statistics_2"),
        htmlOutput("boxplot_reference_2"),
        plotOutput("boxplots_2"),
        htmlOutput("assumption_testing_title_2"),
        tableOutput("shapiro_wilk_test_2"),
        verbatimTextOutput("shapiro_wilk_test_interpret_2"),
        plotOutput("normal_qq_plot_2"),
        htmlOutput("paired_samples_t_test_title_2"),
        tableOutput("paired_samples_t_test_2"),
        verbatimTextOutput("paired_samples_t_test_interpret_2"),
        htmlOutput("wilcoxon_signed_rank_test_title_2"),
        tableOutput("wilcoxon_signed_rank_test_2"),
        verbatimTextOutput("wilcoxon_signed_rank_test_interpret_2"),
      )
    }
  })


  ##############################
  error2_1 <- reactive({
    input$submit2
    validate(
      need(input$file2, "Please upload a pre-test data file to analyze!"),
    )
    paste("The pre-test data file name uploaded for paired samples data analysis: ", input$file2$name)
  })
  output$error2_1 <- renderText({
    error2_1()
  })

  error2_2 <- reactive({
    input$submit2
    validate(
      need(input$file3, "Please upload a post-test data file to analyze!")
    )
    paste("The post-test data file name uploaded for paired samples data analysis: ", input$file3$name)
  })
  output$error2_2 <- renderText({
    error2_2()
  })
  ##############################


  observeEvent(input$submit2, {
    # paired_samples_return <- eventReactive(input$submit2, {
    # if (is.null(data2())) {
    #   return(NULL)
    # }
    # if (is.null(data3())) {
    #   return(NULL)
    # }
    # paired_samples(pre_csv_data = data2(),
    #                post_csv_data = data3(),
    #                m_cutoff = scale2()
    #                )
    # })
    #
    #
    # observeEvent(input$submit2, {
    #   if (is.null(data2())) {
    #     return(NULL)
    #   }
    #   if (is.null(data3())) {
    #     return(NULL)
    #   }

  # paired_samples <- function(pre_csv_data, post_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is Yes, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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
  message("--> ",n_students_deleted_pre, " student(s) from the pre-test data and ", n_students_deleted_post, " student(s) from the post-test data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
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
#
#   # Rename variables for return
#
#   # Return values for testing the function
#   out <- tibble::lst(
#     n_students_deleted,
#     shapiro_wilk_test,
#     normal_qq_plot,
#     descriptive_statistics,
#     boxplots,
#     paired_samples_t_test,
#     wilcoxon_signed_rank_test,
#   )
#   return(invisible(out))
#
  })
#######################################################

paired_samples <- eventReactive(input$submit2, {
    # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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
})

#-------------------------------------------------------------------------------

n_students_deleted_pre <- reactive({

  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  nrow_all-nrow_subset
})

  #-------------------------------------------------------------------------------

  n_students_deleted_post <- reactive({

    # binding for global variable
    m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

    # Read pre-post datasets (of the treatment group)
    data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
    data_treat_post <- read_csv(data3(), show_col_types = FALSE)

    # Converting multiple-choice answers to binary
    if (input$m_choice2 == "1") {
      if (is.null(answer_key2())) {
        stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
      } else {
        answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
        nrow <- nrow(answer_keys)
        for (i in 1:nrow) {
          data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
          data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
          data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
          data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        }
      }
    }

    # Deleting students with too many skipped answers: data_treat_post.csv-----------------
    nrow_all <- nrow(data_treat_post)
    n <- as.numeric(length(data_treat_post[,-1]))
    data_treat_post <- data_treat_post %>%
      mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

    data_treat_post <- subset(data_treat_post, m_rate < scale1())
    nrow_subset <- nrow(data_treat_post)
    nrow_all-nrow_subset
  })

#------------------------------------------------------------------------------

shapiro_wilk_test_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(shapiro_wilk_test)
})

#-------------------------------------------------------------------------------

normal_qq_plot_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(normal_qq_plot)
})

#------------------------------------------------------------------------------

descriptive_statistics_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(descriptive_statistics)
})

#------------------------------------------------------------------------------

boxplots_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(boxplots)
})

#------------------------------------------------------------------------------

paired_samples_t_test_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(paired_samples_t_test)
})

#------------------------------------------------------------------------------

wilcoxon_signed_rank_test_2 <- reactive({
  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(data2(), show_col_types = FALSE)
  data_treat_post <- read_csv(data3(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice2 == "1") {
    if (is.null(answer_key2())) {
      stop("If m_choice is TRUE, then you need to put answer_key2() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key2(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
        data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

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

  return(wilcoxon_signed_rank_test)
})

##############-----------------------------------------------------------------
## Output rendering

  output$n_students_deleted_2 <- renderUI({
    paired_samples()
    n_students_deleted_2 <-c("",
                             "======================================",
                             paste("The number of students deleted: ", n_students_deleted_pre(), " student(s) from the pre-test data and ", n_students_deleted_post(), "  student(s) from the post-test data has(have) been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
    HTML(paste(n_students_deleted_2, collapse = "<br/>"))
  })

  output$descriptive_statistics_title_2 <- renderUI({
    paired_samples()
    descriptive_statistics_title_2 <-c("",
                                 "======================================",
                                 paste("Pre-test/Post-test Scores' Descriptive Statistics", sep=""))
    HTML(paste(descriptive_statistics_title_2, collapse = "<br/>"))
  })

  output$descriptive_statistics_2 <- renderTable({
    paired_samples()
    descriptive_statistics_2()
  })

  output$boxplot_reference_2 <- renderUI({
    paired_samples()
    boxplot_reference_2 <-c("--> Refer to the boxplot below for a visual presentation of the descriptive statistics above:")
    HTML(paste(boxplot_reference_2, collapse = "<br/>"))
  })

  output$boxplots_2 <- renderPlot({
    paired_samples()
    boxplots_2()
  })

  output$assumption_testing_title_2 <- renderUI({
    paired_samples()
    assumption_testing_title_2 <-c("",
                                 "======================================",
                                 paste("Testing the Assumption of Normality", sep=""))
    HTML(paste(assumption_testing_title_2, collapse = "<br/>"))
  })

  output$shapiro_wilk_test_2 <- renderTable({
    paired_samples()
    shapiro_wilk_test_2()
  })

  output$shapiro_wilk_test_interpret_2 <- renderPrint({
    paired_samples()
    if (shapiro_wilk_test_2()$p > 0.05) {
      cat("--> Interpretation: The assumption of normality by group has been met (p>0.05). Refer to the normal Q-Q plot below to check the normality visually. Refer to the paired-samples t-test results below.")
    } else {
      cat("--> Interpretation: The assumption of normality by group has NOT been met (p<0.05). Refer to the normal Q-Q plot below to check the normality visually. Refer to the Wilcoxon signed rank sum test results below.")
    }
  })

  output$normal_qq_plot_2 <- renderPlot({
    paired_samples()
    normal_qq_plot_2()
  })

  output$paired_samples_t_test_title_2 <- renderUI({
    paired_samples()
    paired_samples_t_test_title_2 <-c("",
                                    "======================================",
                                    paste("Paired T-Test Results", sep=""))
    HTML(paste(paired_samples_t_test_title_2, collapse = "<br/>"))
  })

  output$paired_samples_t_test_2 <- renderTable({
    paired_samples()
    tidy(paired_samples_t_test_2())
  })

  output$paired_samples_t_test_interpret_2 <- renderPrint({
    paired_samples()
    if (paired_samples_t_test_2()$p.value < 0.05) {
      cat("--> Interpretation: The average pre-test score was ", round(descriptive_statistics_2()$mean[1],2),
             " and the average post-test score was ",round(descriptive_statistics_2()$mean[2],2),". ",
             "The Paired Samples T-Test showed that the pre-post difference was statistically significant (p=",
             round(paired_samples_t_test_2()$p.value,3),").")
    } else {
      cat("--> Interpretation: The average pre-test score was ",round(descriptive_statistics_2()$mean[1],2),
             " and the average post-test score was ",round(descriptive_statistics_2()$mean[2],2),". ",
             "The Paired Samples T-Test showed that the pre-post difference was NOT statistically significant (p=",
             round(paired_samples_t_test_2()$p.value,3),").")
    }
  })

  output$wilcoxon_signed_rank_test_title_2 <- renderUI({
    paired_samples()
    wilcoxon_signed_rank_test_title_2 <-c("",
                                        "======================================",
                                        paste("Wilcoxon Signed Rank Sum Test Results", sep=""))
    HTML(paste(wilcoxon_signed_rank_test_title_2, collapse = "<br/>"))
  })

  output$wilcoxon_signed_rank_test_2 <- renderTable({
    paired_samples()
    tidy(wilcoxon_signed_rank_test_2())
  })

  output$wilcoxon_signed_rank_test_interpret_2 <- renderPrint({
    paired_samples()
    if (wilcoxon_signed_rank_test_2()$p.value < 0.05) {
      cat("--> Interpretation: The Wilcoxon signed rank test results above show that the pre-post difference was statistically significant (p=",
             round(wilcoxon_signed_rank_test_2()$p.value,3),").")
    } else {
      cat("--> Interpretation: The Wilcoxon signed rank test results above show that the pre-post difference was NOT statistically significant (p=",
             round(wilcoxon_signed_rank_test_2()$p.value,3),").")
    }
  })

###############################################################################
###############################################################################
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

  output$independent_samples_output <- renderUI({
    if (input$selected_function == "Independent Samples Data Analysis") {
      # UI for Item Analysis
        tagList(
          textOutput("error3_1"),
          textOutput("error3_2"),
          htmlOutput("n_students_deleted_treat"),
          htmlOutput("n_students_deleted_ctrl"),
          htmlOutput("descriptive_statistics_title_3"),
          tableOutput("descriptive_statistics_3"),
          htmlOutput("boxplot_reference_3"),
          plotOutput("boxplots_3"),
          htmlOutput("assumption_testing_title_3"),
          htmlOutput("shapiro_wilk_test_title_3"),
          tableOutput("shapiro_wilk_test_3"),
          verbatimTextOutput("shapiro_wilk_test_interpret_3"),
          plotOutput("normal_qq_plot_3"),
          htmlOutput("equalvariance_title_3"),
          tableOutput("equalvariance_3"),
          verbatimTextOutput("equalvariance_interpret_3"),
          htmlOutput("independent_samples_t_test_title_3"),
          verbatimTextOutput("independent_samples_t_test_3"),
          htmlOutput("mann_whitney_u_test_title_3"),
          verbatimTextOutput("mann_whitney_u_test_3"),
          verbatimTextOutput("mann_whitney_u_test_interpret_3"),
        )
    }
  })
####################################################################
      ##############################
      error3_1 <- reactive({
        input$submit3
        validate(
          need(input$file4, "Please upload a treatment group test data file to analyze!"),
        )
        paste("The treatment group test data file name uploaded for independent samples data analysis: ", input$file4$name)
      })
      output$error3_1 <- renderText({
        error3_1()
      })

      error3_2 <- reactive({
        input$submit3
        validate(
          need(input$file5, "Please upload a control group test data file to analyze!")
        )
        paste("The control group test data file name uploaded for independent samples data analysis: ", input$file5$name)
      })
      output$error3_2 <- renderText({
        error3_2()
      })
      ##############################



observeEvent(input$submit3, {

  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  ####### Present the resutls in the R console for the convenience of the user

  message("==============================")
  message("The number of students deleted")
  print(n_students_deleted)
  message("--> ",n_students_deleted_treat, " student(s) from the treatment group data and ", n_students_deleted_ctrl, " student(s) from the control group data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
  message("", sep="\n")
  message("======================")
  message("Descriptive Statistics")
  print(descriptive_statistics)
  message("Refer to the boxplots in the 'Plots' panel for visual inspection of the descriptive statistics.", sep="\n")
  print(boxplots)
  message("====================")
  message("Checking Assumptions", sep="\n")
  message("The Equality of Variances")
  print(equalvariance)
  if (equalvariance$`Pr(>F)`[1] > 0.05) {
    message("## Interpretation: the assumption of equality of variances has been met (p>0.05)", sep="\n")
  } else {
    message("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05)", sep="\n")
  }
  message("The Nomality by Groups")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p[2] >0.05 & shapiro_wilk_test$p[1] >0.05) {
    message("## Interpretation: the assumption of normality has been met (p>0.05 for each group).", sep="\n")
  } else {
    message("## Interpretation: the assumption of normality has NOT been met (p<0.05 for at least one of the groups).", sep="\n")
  }
  message("Refer to the Q-Q Plots in the 'Plots' panel for visual inspection of the normality.", sep="\n")
  print(normal_qq_plot)

  message("===============================================")
  message("Independent Samples T-Test Results (Parametric)")
  if (equalvariance$`Pr(>F)`[1] > 0.05) {
    print(independent_samples_t_test_equal)
    message("## A sample interpretation of the t-test results above:")
    if (independent_samples_t_test_equal$p.value < 0.05) {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
              " and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
              "The independent samples t-test results show that the pre-post difference is statistically significant
                 (p=", round(independent_samples_t_test_equal$p.value,3),").")
    } else {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
              ", and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
              "The independent samples t-test results show that the group difference is NOT statistically significant (p=",
              round(independent_samples_t_test_equal$p.value,3),").")
    }
  } else {
    print(independent_samples_t_test_unequal)
    message("## A sample interpretation of the t-test results above:")
    if (independent_samples_t_test_unequal$p.value < 0.05) {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
              " and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
              "The independent samples t-test results show that the pre-post difference is statistically significant
                 (p=", round(independent_samples_t_test_unequal$p.value,3),").")
    } else {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
              ", and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
              "The independent samples t-test results show that the group difference is NOT statistically significant (p=",
              round(independent_samples_t_test_unequal$p.value,3),").")
    }
  }
  sample_size_t <- nrow(data_treat_post)
  sample_size_c <- nrow(data_ctrl_post)
  if (sample_size_t < 15 | sample_size_c < 15 | shapiro_wilk_test$p[2] < 0.05 | shapiro_wilk_test$p[1] < 0.05) {
    message("## Either a sample size is too small or the data violates the assumption of eval variances (refer to the descriptive statistics and the Shapiro-Wilk test resutls). Although the t-test is known to be robust to a small sample size or violation of the normality assumption, you may want to refer to the Mann-Whitney U (Wilcoxon Rank Sum) test results below to be safe.")
    message("===============================================================")
    message("Mann-Whitney U (Wilcoxon Rank Sum) Test Results (Nonparametric)", sep="\n")
    print(mann_whitney_u_test)
    message("## A sample interpretation of the Mann-Whitney U (Wilcoxon Rank Sum) test results above:")
    if (mann_whitney_u_test$p.value < 0.05) {
      message("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was statistically significant (p=",
              round(mann_whitney_u_test$p.value,3),").")
    } else {
      message("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was NOT statistically significant (p=",
              round(mann_whitney_u_test$p.value,3),").")
    }
  }
})

#------------------------------------------------------------------------------

independent_samples <- eventReactive(input$submit3, {

  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

})

#------------------------------------------------------------------------------

n_students_deleted_treat <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  nrow_all-nrow_subset
})

#------------------------------------------------------------------------------

n_students_deleted_ctrl <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  nrow_all-nrow_subset
})

#------------------------------------------------------------------------------

descriptive_statistics_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  return(descriptive_statistics)
})

#------------------------------------------------------------------------------

boxplots_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  return(boxplots)
})

#------------------------------------------------------------------------------

shapiro_wilk_test_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)

  return(shapiro_wilk_test)
})

#------------------------------------------------------------------------------

normal_qq_plot_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  return(normal_qq_plot)
})

#------------------------------------------------------------------------------

independent_samples_t_test_equal_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  return(independent_samples_t_test_equal)
})

#------------------------------------------------------------------------------

independent_samples_t_test_unequal_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  return(independent_samples_t_test_unequal)
})

#------------------------------------------------------------------------------

mann_whitney_u_test_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  return(mann_whitney_u_test)
})

#------------------------------------------------------------------------------

equalvariance_3 <- reactive({
  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(data4(), show_col_types = FALSE)
  data_ctrl_post<- read_csv(data5(), show_col_types = FALSE)

  # Converting multiple-choice answers to binary
  if (input$m_choice3 == "1") {
    if (is.null(answer_key3())) {
      stop("If m_choice is TRUE, then you need to put answer_key3() (e.g., 'answer_keys.csv').")
    } else {
      answer_keys <- read_csv(answer_key3(), show_col_types = FALSE)
      nrow <- nrow(answer_keys)
      for (i in 1:nrow) {
        data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
        data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
        data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
      }
    }
  }

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < scale1())
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
    group_by(datagroup) %>%
    get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
    group_by(datagroup) %>%
    identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
    group_by(datagroup) %>%
    shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- leveneTest(avg_score_post ~ datagroup, group_data_binded)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  return(equalvariance)
})

#############################--------------------------------------------------
## Output rendering

      output$n_students_deleted_treat <- renderUI({
        independent_samples()
        n_students_deleted_treat <-c("",
                                     "======================================",
                                     paste("The number of students deleted: ", as.numeric(n_students_deleted_treat()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
        HTML(paste(n_students_deleted_treat, collapse = "<br/>"))
      })

      output$n_students_deleted_ctrl <- renderUI({
        independent_samples()
        n_students_deleted_ctrl <-c("",
                                    "======================================",
                                    paste("The number of students deleted: ", as.numeric(n_students_deleted_ctrl()), " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
        HTML(paste(n_students_deleted_ctrl, collapse = "<br/>"))
      })

      output$descriptive_statistics_title_3 <- renderUI({
        independent_samples()
        descriptive_statistics_title_3 <-c("",
                                         "======================================",
                                         paste("Treatment/Control Group Scores' Descriptive Statistics", sep=""))
        HTML(paste(descriptive_statistics_title_3, collapse = "<br/>"))
      })

      output$descriptive_statistics_3 <- renderTable({
        independent_samples()
        descriptive_statistics_3()
      })

      output$boxplot_reference_3 <- renderUI({
        independent_samples()
        boxplot_reference_3 <-c("--> Refer to the boxplot below for a visual presentation of the descriptive statistics above:")
        HTML(paste(boxplot_reference_3, collapse = "<br/>"))
      })

      output$boxplots_3 <- renderPlot({
        independent_samples()
        boxplots_3()
      })

      output$assumption_testing_title_3 <- renderUI({
        independent_samples()
        assumption_testing_title_3 <- c("",
                                     "======================================",
                                     paste("Testing the Assumptions", sep=""))
        HTML(paste(assumption_testing_title_3, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_title_3 <- renderUI({
        independent_samples()
        shapiro_wilk_test_title_3 <-c("",
                                    "-------------------------------------",
                                    paste("Normality by Group - Shapiro-Wilk Test Result", sep=""))
        HTML(paste(shapiro_wilk_test_title_3, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_3 <- renderTable({
        independent_samples()
        shapiro_wilk_test_3()
      })

      output$shapiro_wilk_test_interpret_3 <- renderPrint({
        independent_samples()
        if (shapiro_wilk_test_3()$p[2] >0.05 & shapiro_wilk_test_3()$p[1] >0.05) {
          paste0("--> Interpretation: the assumption of normality has been met (p>0.05 for each group). Refer to the Q-Q plots below for visual inspection of the normality.", sep="\n")
        } else {
          paste0("--> Interpretation: the assumption of normality has NOT been met (p<0.05 for at least one of the groups). Refer to the Q-Q plots below for visual inspection of the normality.", sep="\n")
        }
      })

      output$normal_qq_plot_3 <- renderPlot({
        independent_samples()
        normal_qq_plot_3()
      })

      output$equalvariance_title_3 <- renderUI({
        independent_samples()
        equalvariance_title_3 <-c("",
                                "-------------------------------------",
                                paste("Equality of Variances - Levene Test Result", sep=""))
        HTML(paste(equalvariance_title_3, collapse = "<br/>"))
      })

      output$equalvariance_3 <- renderTable({
        independent_samples()
        equalvariance_3()
      })

      output$equalvariance_interpret_3 <- renderPrint({
        independent_samples()
        if (equalvariance_3()$`Pr(>F)`[1] > 0.05) {
          paste0("--> Interpretation: the assumption of equality of variances has been met (p>0.05)", sep="\n")
        } else {
          paste0("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05)", sep="\n")
        }
      })

      output$independent_samples_t_test_title_3 <- renderUI({
        independent_samples()
        independent_samples_t_test_title_3 <-c("",
                                             "======================================",
                                             paste("Independent Samples T-Test Results", sep=""))
        HTML(paste(independent_samples_t_test_title_3, collapse = "<br/>"))
      })

      output$independent_samples_t_test_3 <- renderPrint({
        independent_samples()
        if (equalvariance_3()$`Pr(>F)`[1] > 0.05) {
          independent_samples_t_test_equal_3()
          if (independent_samples_t_test_equal_3()$p.value < 0.05) {
            paste0("--> The independent samples t-test results show that the group difference is statistically significant
                 (p=", round(independent_samples_t_test_equal_3()$p.value,3),").")
          } else {
            paste0("--> The independent samples t-test results show that the group difference is NOT statistically significant (p=",
                   round(independent_samples_t_test_equal_3()$p.value,3),").")
          }
        } else {
          independent_samples_t_test_unequal_3()
          if (independent_samples_t_test_unequal_3()$p.value < 0.05) {
            paste0("--> The independent samples t-test results show that the group difference is statistically significant
                 (p=", round(independent_samples_t_test_unequal_3()$p.value,3),").")
          } else {
            paste0("--> The independent samples t-test results show that the group difference is NOT statistically significant (p=",
                   round(independent_samples_t_test_unequal_3()$p.value,3),").")
          }
        }
      })

      output$mann_whitney_u_test_title_3 <- renderUI({
        independent_samples()
        mann_whitney_u_test_title_3 <-c("",
                                      "======================================",
                                      paste("Mann-Whitney U Test Results", sep=""))
        HTML(paste(mann_whitney_u_test_title_3, collapse = "<br/>"))
      })

      output$mann_whitney_u_test_3 <- renderPrint({
        independent_samples()
        mann_whitney_u_test_3()
      })

      output$mann_whitney_u_test_interpret_3 <- renderPrint({
        independent_samples()
        if (mann_whitney_u_test_3()$p.value < 0.05) {
          paste0("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was statistically significant (p=",
                 round(mann_whitney_u_test_3()$p.value,3),").")
        } else {
          paste0("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was NOT statistically significant (p=",
                 round(mann_whitney_u_test_3()$p.value,3),").")
        }
      })

      ###############################################################################
      ###############################################################################
      ## One-way ANCOVA
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
      #
      output$one_way_ancova_output <- renderUI({
        if (input$selected_function == "One-way ANCOVA") {
          tagList(
            textOutput("error4_1"),
            textOutput("error4_2"),
            textOutput("error4_3"),
            textOutput("error4_4"),
            htmlOutput("n_students_deleted"),
            htmlOutput("pre_descriptive_statistics_title"),
            tableOutput("pre_descriptive_statistics"),
            htmlOutput("post_descriptive_statistics_title"),
            tableOutput("post_descriptive_statistics"),
            htmlOutput("boxplot_reference"),
            plotOutput("boxplots"),
            htmlOutput("assumption_testing_title"),
            htmlOutput("scatter_plot_title"),
            plotOutput("scatter_plot"),
            htmlOutput("shapiro_wilk_test_title"),
            verbatimTextOutput("shapiro_wilk_test"),
            verbatimTextOutput("shapiro_wilk_test_interpret"),
            plotOutput("normal_qq_plot"),
            htmlOutput("levene_test_title"),
            tableOutput("levene_test"),
            verbatimTextOutput("levene_test_interpret"),
            htmlOutput("line_slopes_title"),
            tableOutput("line_slopes"),
            verbatimTextOutput("line_slopes_interpret"),
            htmlOutput("res.aov_title"),
            tableOutput("res.aov"),
            htmlOutput("estimated_marginal_means_title"),
            tableOutput("estimated_marginal_means"),
            verbatimTextOutput("estimated_marginal_means_interpret"),
          )
        }
      })
      ####################################################################

      error4_1 <- reactive({
        input$submit4
        validate(
          need(input$file6, "Please upload a treatment-group pre-test test data file to analyze!"),
        )
        paste("The treatment-group pre-test data file name uploaded for one-way ANCOVA: ", input$file6$name)
      })
      output$error4_1 <- renderText({
        error4_1()
      })

      error4_2 <- reactive({
        input$submit4
        validate(
          need(input$file7, "Please upload a treatment-group post-test data file to analyze!"),
        )
        paste("The treatment-group post-test data file name uploaded for one-way ANCOVA: ", input$file7$name)
      })
      output$error4_2 <- renderText({
        error4_2()
      })

      error4_3 <- reactive({
        input$submit4
        validate(
          need(input$file8, "Please upload a control-group pre-test data file to analyze!"),
        )
        paste("The control-group pre-test data file name uploaded for one-way ANCOVA: ", input$file8$name)
      })
      output$error4_3 <- renderText({
        error4_3()
      })

      error4_4 <- reactive({
        input$submit4
        validate(
          need(input$file9, "Please upload a control-group post-test data file to analyze!"),
        )
        paste("The control-group post-test data file name uploaded for one-way ANCOVA: ", input$file9$name)
      })
      output$error4_4 <- renderText({
        error4_4()
      })

      ##############################

      observeEvent(input$submit4, {
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        # Check outlier
        outliers <- model_metrics %>%
          dplyr::filter(abs(.std.resid) > 3) %>%
          as.data.frame()

        # Run one-way ANCOVA
        one_way_ancova <- res.aov <- full_data_binded %>%
          rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre)

        # Display the adjusted means (a.k.a., estimated marginal means) for each group
        emms <- emmeans_test(full_data_binded,
                             avg_score_post ~ datagroup,
                             covariate = avg_score_pre,
                             p.adjust.method = "bonferroni",
                             conf.level=0.95,
                             detailed=TRUE
        )
        estimated_marginal_means <- get_emmeans(emms) %>%
          as.data.frame()

        ####### Present the results in the console panel for the convenience of users
        message("==============================")
        message("The number of students deleted")
        print(n_students_deleted)
        message("--> ",n_students_deleted_treat_pre, " student(s) from the treatment group pre-test data, ", n_students_deleted_treat_post, " student(s) from the treatment group post-test data, ",n_students_deleted_ctrl_pre, " student(s) from the control group pre-test data, and ", n_students_deleted_ctrl_post, " student(s) from the control group post-test data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
        message("", sep="\n")
        message("======================")
        message("Descriptive Statistics")
        message("Pre-test scores by group")
        print(pre_descriptive_statistics)
        message("-------------------------")
        message("Post-test scores by group")
        print(post_descriptive_statistics)
        message("Refer to the boxplots in the 'Plots' panel to visually inspect the descriptive statistics")
        print(boxplots)
        message("", sep="\n")
        message("===============================")
        message("Results of Checking Assumptions", sep="\n")
        message("# Linearity:")
        message("Refer to the scatter plot to check linearity in the 'Plots' panel. If two lines look almost paralleled, they can be interpreted as meeting the assumption of linearity.")
        plot(scatter_plot)
        message("## Interpretation: if you are seeing a liner relationship between the covariate (i.e., pre-test scores for this analysis) and dependent variable (i.e., post-test scores for this analysis) for both treatment and control group in the plot, then you can say this assumption has been met or the data has not violated this assumption of linearity. If your relationships are not linear, you have violated this assumption, and an ANCOVA is not a suitable analysis. However, you might be able to coax your data to have a linear relationship by transforming the covariate, and still be able to run an ANCOVA.")
        message("-------------------------")
        message("# Normality of Residuals:")
        print(shapiro_wilk_test)
        if (shapiro_wilk_test$p.value > 0.05) {
          message("## Interpretation: the assumption of normality by group has been met (p>0.05).")
        } else {
          message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANCOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue)")
        }
        message("Refer to the histogram and the normal Q-Q plot in the 'Plots' panel to visually inspect the normality of residuals.")
        hist(Residuals)
        print(normal_qq_plot)
        message("---------------------------")
        message("# Homogeneity of Variances:")
        print(levene_test)
        if (levene_test$`Pr(>F)`[1] > 0.05) {
          message("## Interpretation: the assumption of equality of error variances has been met (p>0.05).")
        } else {
          message("## Interpretation: the assumption of equality of error variances has NOT been met (p<0.05). You need to transform your dependent variable (post-test average scores) to see if you can remove the heterogeneity in your ANCOVA model.")
        }
        message("----------------------------------------")
        message("# Homogeneity of Regression Slopes:")
        print(line_slopes)
        if (line_slopes$p[3] > 0.05) {
          message("## Interpretation: there was homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was not statistically significant (p>0.05).")
        } else {
          message("## Interpretation: the data has violated the assumption of homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was statistically significant (p<0.05). You will not be able to undertake an ANCOVA analysis.")
        }
        if (nrow(outliers) != 0) {
          message("-----------")
          message("# Outliers:")
          print(outliers)
        } else {
          message("----------")
          message("# Outliers: No outlier has been found.")
        }
        message("", sep="\n")
        message("==================================")
        message("Results of the main One-way ANCOVA")
        print(res.aov)
        message("--------------------------")
        message("# Estimated Marginal Means")
        print(estimated_marginal_means)
        if (res.aov$p[1] < 0.05) {
          message("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be significant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means$emmean[2],2),", SE=,",round(estimated_marginal_means$se[2],2),") was significantly different from that of the control group (",round(estimated_marginal_means$emmean[1],2),", SE=,",round(estimated_marginal_means$se[1],2),").")
        } else {
          message("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be insignificant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means$emmean[2],2),", SE=,",round(estimated_marginal_means$se[2],2),") was not significantly different from that of the control group (",round(estimated_marginal_means$emmean[1],2),", SE=,",round(estimated_marginal_means$se[1],2),").")
        }
      })

      ################################################################

      one_way_ancova <- eventReactive(input$submit4, {
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        # Check outlier
        outliers <- model_metrics %>%
          dplyr::filter(abs(.std.resid) > 3) %>%
          as.data.frame()

        # Run one-way ANCOVA
        one_way_ancova <- res.aov <- full_data_binded %>%
          rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre)

        # Display the adjusted means (a.k.a., estimated marginal means) for each group
        emms <- emmeans_test(full_data_binded,
                             avg_score_post ~ datagroup,
                             covariate = avg_score_pre,
                             p.adjust.method = "bonferroni",
                             conf.level=0.95,
                             detailed=TRUE
        )
        estimated_marginal_means <- get_emmeans(emms) %>%
          as.data.frame()

      })

      #------------------------------------------------------------------------------

      n_students_deleted_treat_pre <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        nrow_all-nrow_subset
      })

      #------------------------------------------------------------------------------

      n_students_deleted_treat_post <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        nrow_all-nrow_subset
      })

      #------------------------------------------------------------------------------

      n_students_deleted_ctrl_pre <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        nrow_all-nrow_subset
      })

      #------------------------------------------------------------------------------

      n_students_deleted_ctrl_post <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        nrow_all-nrow_subset
      })

      #------------------------------------------------------------------------------

      pre_descriptive_statistics <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        return(pre_descriptive_statistics)
      })

      #------------------------------------------------------------------------------

      post_descriptive_statistics <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        return(post_descriptive_statistics)
      })

      #------------------------------------------------------------------------------

      boxplots <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        return(boxplots)
      })

      #------------------------------------------------------------------------------

      scatter_plot <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        return(scatter_plot)
      })

      #------------------------------------------------------------------------------

      line_slopes <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        return(line_slopes)
      })

      #------------------------------------------------------------------------------

      shapiro_wilk_test <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        return(shapiro_wilk_test)
      })

      #------------------------------------------------------------------------------

      normal_qq_plot <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        return(normal_qq_plot)
      })

      #------------------------------------------------------------------------------

      levene_test <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        return(levene_test)
      })

      #------------------------------------------------------------------------------

      outlier <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        # Check outlier
        outliers <- model_metrics %>%
          dplyr::filter(abs(.std.resid) > 3) %>%
          as.data.frame()

        return(outliers)
      })

      #------------------------------------------------------------------------------

      res.aov <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        # Check outlier
        outliers <- model_metrics %>%
          dplyr::filter(abs(.std.resid) > 3) %>%
          as.data.frame()

        # Run one-way ANCOVA
        res.aov <- full_data_binded %>%
          rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre)

        return(res.aov)
      })

      #------------------------------------------------------------------------------

      estimated_marginal_means <- reactive({
        # binding for global variable
        m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

        # Read all data sets
        data_treat_pre <- read_csv(data6(), show_col_types = FALSE)
        data_treat_post<- read_csv(data7(), show_col_types = FALSE)
        data_ctrl_pre <- read_csv(data8(), show_col_types = FALSE)
        data_ctrl_post<- read_csv(data9(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice4 == "1") {
          if (is.null(answer_key4())) {
            stop("If m_choice is TRUE, then you need to put answer_key4() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key4(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_pre[,i+1][data_ctrl_pre[,i+1]==answer_keys$key[i]]=1
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]!=answer_keys$key[i]]=0
              data_ctrl_post[,i+1][data_ctrl_post[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_treat_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_treat_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
        nrow_all <- nrow(data_ctrl_pre)
        n <- as.numeric(length(data_ctrl_pre[,-1]))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

        data_ctrl_pre <- subset(data_ctrl_pre, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_pre)
        n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
        nrow_all <- nrow(data_ctrl_post)
        n <- as.numeric(length(data_ctrl_post[,-1]))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

        data_ctrl_post <- subset(data_ctrl_post, m_rate < scale1())
        nrow_subset <- nrow(data_ctrl_post)
        n_students_deleted_ctrl_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_ctrl_post$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
        data_ctrl_pre[is.na(data_ctrl_pre)]= 0
        for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
        data_ctrl_post[is.na(data_ctrl_post)]= 0

        # Creat data sets for descriptive statistics
        data_c_pre <- data_ctrl_pre %>%
          mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_c_post <- data_ctrl_post %>%
          mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
        data_t_pre <- data_treat_pre %>%
          mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
        data_t_post <- data_treat_post %>%
          mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
        colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_ctrl_pre <- data_ctrl_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
        data_ctrl_post <- data_ctrl_post %>%
          mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

        # Merge pre/post data and generate group code (treat=1, control=0)
        treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
        treat_data_merged <- treat_data_merged %>%
          mutate(datagroup=1)

        ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
        names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
        ctrl_data_merged <- ctrl_data_merged %>%
          mutate(datagroup=0)

        # Bind treat/control data
        full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

        # Name datagroup -> 0=control, 1=treatment
        full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

        # Descriptive Statistics
        pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
        post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
          summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

        data_c_pre <- data_c_pre %>%
          mutate(datagroup=1)
        data_c_post <- data_c_post %>%
          mutate(datagroup=2)
        data_t_pre <- data_t_pre %>%
          mutate(datagroup=3)
        data_t_post <- data_t_post %>%
          mutate(datagroup=4)

        df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
        df_Long <- df_Long %>%
          select(avg_score, datagroup)
        names(df_Long) <- c("Average_Score", "Group")
        # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
        df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

        boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

        # Check assumptions
        # Check linearity (visual inspection)
        scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
          stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
          ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
          xlab("Pre-test Scores") + ylab("Post-test Scores")

        # Check homogeneity of regression slopes
        # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
        line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

        # Inspect the model diagnostic metrics
        model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
        model_metrics <- augment(model) %>%
          dplyr::select(-.hat, -.sigma, -.fitted)
        # print(head(model_metrics, 3))

        # Check normality of Residuals
        norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
        shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
        Residuals <- norm.all.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

        # Check homogeneity of variances
        # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
        levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

        # Check outlier
        outliers <- model_metrics %>%
          dplyr::filter(abs(.std.resid) > 3) %>%
          as.data.frame()

        # Run one-way ANCOVA
        one_way_ancova <- res.aov <- full_data_binded %>%
          rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre)

        # Display the adjusted means (a.k.a., estimated marginal means) for each group
        emms <- emmeans_test(full_data_binded,
                             avg_score_post ~ datagroup,
                             covariate = avg_score_pre,
                             p.adjust.method = "bonferroni",
                             conf.level=0.95,
                             detailed=TRUE
        )
        estimated_marginal_means <- get_emmeans(emms) %>%
          as.data.frame()

        return(estimated_marginal_means)
      })

      ###############################################################################
      ## Output rendering (or_ancova)

      output$n_students_deleted <- renderUI({
        one_way_ancova()
        n_students_deleted <-c("",
                               "======================================",
                               paste("The number of students deleted: ", n_students_deleted_treat_pre(), " student(s) from the treatment group pre-test data, ", n_students_deleted_treat_post(), " student(s) from the treatment group post-test data, ",n_students_deleted_ctrl_pre(), " student(s) from the control group pre-test data, and ", n_students_deleted_ctrl_post(), " student(s) from the control group post-test data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
        HTML(paste(n_students_deleted, collapse = "<br/>"))
      })

      output$pre_descriptive_statistics_title <- renderUI({
        one_way_ancova()
        pre_descriptive_statistics_title <-c("",
                                             "======================================",
                                             paste("Pre-test Scores' Descriptive Statistics by Group", sep=""))
        HTML(paste(pre_descriptive_statistics_title, collapse = "<br/>"))
      })

      output$pre_descriptive_statistics <- renderTable({
        one_way_ancova()
        pre_descriptive_statistics()
      })

      output$post_descriptive_statistics_title <- renderUI({
        one_way_ancova()
        post_descriptive_statistics_title <-c("",
                                              "======================================",
                                              paste("Post-test Scores' Descriptive Statistics by Group", sep=""))
        HTML(paste(post_descriptive_statistics_title, collapse = "<br/>"))
      })

      output$post_descriptive_statistics <- renderTable({
        one_way_ancova()
        post_descriptive_statistics()
      })

      output$boxplot_reference <- renderUI({
        one_way_ancova()
        boxplot_reference <-c("--> Refer to the boxplot below for a visual presentation of the descriptive statistics above:")
        HTML(paste(boxplot_reference, collapse = "<br/>"))
      })

      output$boxplots <- renderPlot({
        one_way_ancova()
        boxplots()
      })

      output$assumption_testing_title <- renderUI({
        one_way_ancova()
        assumption_testing_title <- c("",
                                      "======================================",
                                      paste("Results of Checking Assumptions", sep=""))
        HTML(paste(assumption_testing_title, collapse = "<br/>"))
      })

      output$scatter_plot_title <- renderUI({
        one_way_ancova()
        scatter_plot_title <-c("",
                               "-------------------------------------",
                               paste("Linearity- Scatter Plot: If two lines in the plot below look almost paralleled, they can be interpreted as meeting the assumption of linearity.", sep=""))
        HTML(paste(scatter_plot_title, collapse = "<br/>"))
      })

      output$scatter_plot <- renderPlot({
        one_way_ancova()
        scatter_plot()
      })

      output$shapiro_wilk_test_title <- renderUI({
        one_way_ancova()
        shapiro_wilk_test_title <-c("",
                                    "-------------------------------------",
                                    paste("Normality by Group - Shapiro-Wilk Test Result", sep=""))
        HTML(paste(shapiro_wilk_test_title, collapse = "<br/>"))
      })

      output$shapiro_wilk_test <- renderPrint({
        one_way_ancova()
        shapiro_wilk_test()
      })

      output$shapiro_wilk_test_interpret <- renderPrint({
        one_way_ancova()
        if (shapiro_wilk_test()$p.value > 0.05) {
          paste0("--> Interpretation: the assumption of normality by group has been met (p>0.05). Refer to the normal Q-Q plot below to visually inspect the normality of residuals.")
        } else {
          paste0("--> Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANCOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue). Refer to the normal Q-Q plot below to visually inspect the normality of residuals.")
        }
      })

      output$normal_qq_plot <- renderPlot({
        one_way_ancova()
        normal_qq_plot()
      })

      output$levene_test_title <- renderUI({
        one_way_ancova()
        levene_test_title <-c("",
                              "-------------------------------------",
                              paste("Homogeneity of Variances - Levene Test Result", sep=""))
        HTML(paste(levene_test_title, collapse = "<br/>"))
      })

      output$levene_test <- renderTable({
        one_way_ancova()
        levene_test()
      })

      output$levene_test_interpret <- renderPrint({
        one_way_ancova()
        if (levene_test()$`Pr(>F)`[1] > 0.05) {
          paste0("--> Interpretation: the assumption of equality of error variances has been met (p>0.05).")
        } else {
          paste0("--> Interpretation: the assumption of equality of error variances has NOT been met (p<0.05). You need to transform your dependent variable (post-test average scores) to see if you can remove the heterogeneity in your ANCOVA model.")
        }
      })

      output$line_slopes_title <- renderUI({
        one_way_ancova()
        line_slopes_title <-c("",
                              "-------------------------------------",
                              paste("Homogeneity of Regression Slopes", sep=""))
        HTML(paste(line_slopes_title, collapse = "<br/>"))
      })

      output$line_slopes <- renderTable({
        one_way_ancova()
        line_slopes()
      })

      output$line_slopes_interpret <- renderPrint({
        one_way_ancova()
        if (line_slopes()$p[3] > 0.05) {
          paste0("--> Interpretation: there was homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was not statistically significant (p>0.05).")
        } else {
          paste0("--> Interpretation: the data has violated the assumption of homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was statistically significant (p<0.05). You will not be able to undertake an ANCOVA analysis.")
        }
      })

      output$res.aov_title <- renderUI({
        one_way_ancova()
        res.aov_title <-c("",
                          "======================================",
                          paste("Results of the main One-way ANCOVA", sep=""))
        HTML(paste(res.aov_title, collapse = "<br/>"))
      })

      output$res.aov <- renderTable({
        one_way_ancova()
        res.aov()
      })

      output$estimated_marginal_means_title <- renderUI({
        one_way_ancova()
        estimated_marginal_means_title <-c("",
                                           "-------------------------------------",
                                           paste("Estimated Marginal Means", sep=""))
        HTML(paste(estimated_marginal_means_title, collapse = "<br/>"))
      })

      output$estimated_marginal_means <- renderTable({
        one_way_ancova()
        estimated_marginal_means()
      })

      output$estimated_marginal_means_interpret <- renderPrint({
        one_way_ancova()
        if (res.aov()$p[1] < 0.05) {
          paste0("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be significant with pre-test scores being controlled: F(1,",res.aov()$DFd[1],")=",res.aov()$F[1],", p=",res.aov()$p[1]," (effect size=",res.aov()$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means()$emmean[2],2),", SE=,",round(estimated_marginal_means()$se[2],2),") was significantly different from that of the control group (",round(estimated_marginal_means()$emmean[1],2),", SE=,",round(estimated_marginal_means()$se[1],2),").")
        } else {
          paste0("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be insignificant with pre-test scores being controlled: F(1,",res.aov()$DFd[1],")=",res.aov()$F[1],", p=",res.aov()$p[1]," (effect size=",res.aov()$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means()$emmean[2],2),", SE=,",round(estimated_marginal_means()$se[2],2),") was not significantly different from that of the control group (",round(estimated_marginal_means()$emmean[1],2),", SE=,",round(estimated_marginal_means()$se[1],2),").")
        }
      })







      ###############################################################################
      ###############################################################################
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

      output$one_way_repeated_anova_output <- renderUI({
        if (input$selected_function == "One-way Repeated Measures ANOVA") {
          tagList(
            textOutput("error5_1"),
            textOutput("error5_2"),
            textOutput("error5_3"),
            htmlOutput("n_students_deleted_5"),
            htmlOutput("descriptive_statistics_title_5"),
            tableOutput("descriptive_statistics_5"),
            htmlOutput("boxplot_reference_5"),
            plotOutput("boxplots_5"),
            htmlOutput("assumption_testing_title_5"),
            htmlOutput("shapiro_wilk_test_title_5"),
            verbatimTextOutput("shapiro_wilk_test_5"),
            verbatimTextOutput("shapiro_wilk_test_interpret_5"),
            plotOutput("normal_qq_plot_5"),
            htmlOutput("sphericity_title_5"),
            verbatimTextOutput("sphericity_5"),
            htmlOutput("one_way_repeated_anova_title_5"),
            tableOutput("res.aov_5"),
            tableOutput("one_way_repeated_anova_5"),
            verbatimTextOutput("one_way_repeated_anova_interpret_5"),
            htmlOutput("one_way_repeated_anova_pwc_title_5"),
            tableOutput("one_way_repeated_anova_pwc_5"),
            verbatimTextOutput("one_way_repeated_anova_pwc_interpret_5"),
            htmlOutput("friedman_test_title_5"),
            tableOutput("friedman_test_5"),
            verbatimTextOutput("friedman_test_interpret_5"),
            htmlOutput("friedman_pwc_title_5"),
            tableOutput("friedman_pwc_5"),
            verbatimTextOutput("friedman_pwc_interpret_5"),
          )
        }
      })
      ####################################################################

      error5_1 <- reactive({
        input$submit5
        validate(
          need(input$file10, "Please upload a pre-test data file to analyze!"),
        )
        paste("The pre-test data file name uploaded for one-way repeated measures ANOVA: ", input$file10$name)
      })
      output$error5_1 <- renderText({
        error5_1()
      })

      error5_2 <- reactive({
        input$submit5
        validate(
          need(input$file11, "Please upload a post-test test data file to analyze!"),
        )
        paste("The post-test data file name uploaded for one-way repeated measures ANOVA: ", input$file11$name)
      })
      output$error5_2 <- renderText({
        error5_2()
      })

      error5_3 <- reactive({
        input$submit5
        validate(
          need(input$file12, "Please upload a post2-test data file to analyze!"),
        )
        paste("The post2-test data file name uploaded for one-way repeated measures ANOVA: ", input$file12$name)
      })
      output$error5_3 <- renderText({
        error5_3()
      })

      ##############################

      observeEvent(input$submit5, {
      # binding for global variable
      m_rate <- Time <- Score <- id <- NULL

      # Read all datasets
      data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
      data_treat_post<- read_csv(data11(), show_col_types = FALSE)
      data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

      # Converting multiple-choice answers to binary
      if (input$m_choice5 == "1") {
        if (is.null(answer_key5())) {
          stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
        } else {
          answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
          nrow <- nrow(answer_keys)
          for (i in 1:nrow) {
            data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
            data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
            data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
            data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
            data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
            data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
          }
        }
      }

      # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
      nrow_all <- nrow(data_treat_pre)
      n <- as.numeric(length(data_treat_pre[,-1]))
      data_treat_pre <- data_treat_pre %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

      data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
      nrow_subset <- nrow(data_treat_pre)
      n_students_deleted_pre <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_pre$m_rate=NULL

      # Deleting students with too many skipped answers: data_treat_post.csv-----------------
      nrow_all <- nrow(data_treat_post)
      n <- as.numeric(length(data_treat_post[,-1]))
      data_treat_post <- data_treat_post %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

      data_treat_post <- subset(data_treat_post, m_rate < scale1())
      nrow_subset <- nrow(data_treat_post)
      n_students_deleted_post <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_post$m_rate=NULL

      # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
      nrow_all <- nrow(data_treat_post2)
      n <- as.numeric(length(data_treat_post2[,-1]))
      data_treat_post2 <- data_treat_post2 %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

      data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
      nrow_subset <- nrow(data_treat_post2)
      n_students_deleted_post2 <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_post2$m_rate=NULL

      n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
      colnames(n_students_deleted) <- c("n")
      rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

      #----------------------------------------------------------------

      # Clean data (e.g., Replace skipped answers with "0")
      n_col <- ncol(data_treat_pre)
      for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
      data_treat_pre[is.na(data_treat_pre)]= 0
      for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
      data_treat_post[is.na(data_treat_post)]= 0
      for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
      data_treat_post2[is.na(data_treat_post2)]= 0

      # Change column names with their origin'sir
      colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
      colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
      colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

      # Calculate average scores
      data_treat_pre <- data_treat_pre %>%
        mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
      data_treat_post <- data_treat_post %>%
        mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
      data_treat_post2 <- data_treat_post2 %>%
        mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

      # Merge pre/post/post2 data
      data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
      treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
      names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

      # Convert Data Frame to a Long Format & Define the Variable
      avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
      df_Long <- melt(avg_score_df)
      names(df_Long) <- c("id", "Time", "Score")
      # Name Time(group) -> 1=Pre, 2=post, 3=Post2
      df_Long$id <- factor(df_Long$id)
      df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

      # Descriptive Statistics
      descriptive_statistics <- df_Long %>%
        group_by(Time) %>%
        get_summary_stats(Score, type="common")
      boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

      # Check Outliers
      outliers <- df_Long %>%
        group_by(Time) %>%
        identify_outliers(Score)

      # Check normality of Residuals
      res.aov <- aov(Score~Time, data=df_Long)
      shapiro_wilk_test <- shapiro.test(resid(res.aov))
      Residuals <- res.aov$residuals
      normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

      # One-way Repeated Measures ANOVA
      res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
      one_way_repeated_anova <- result <- get_anova_table(res.aov)
      repeated_anova_p.value <- result$p

      # Pairwise Comparisons
      one_way_repeated_anova_pwc <- pwc <- df_Long %>%
        pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

      # Friedman Test (Non-parametric)
      friedman_test <- friedman.test(avg_score_df)

      # Pairwise Comparisons - Friedman
      friedman_pwc <- df_Long %>%
        wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

      ####### Present the results in the console for the convenience of users
      message("==============================")
      message("The number of students deleted")
      print(n_students_deleted)
      message("--> ",n_students_deleted_pre, " student(s) from the pre-test data, ", n_students_deleted_post, " student(s) from the post-test data, and ",n_students_deleted_post2, " student(s) from the post2-test data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
      message("", sep="\n")
      message("======================================")
      message("Descriptive Statistics: Average Scores")
      print(descriptive_statistics)
      message("Refer to the boxplots in the 'Plots' panel to visually inspect the descriptive statistics.")
      print(boxplots)
      message("", sep="\n")
      message("==============================")
      message("Results of Testing Assumptions", sep="\n")
      # message("-----------")
      # message("# Outliers:")
      # print(outliers)
      # if (outliers$is.extreme[1]==FALSE & outliers$is.extreme[2]==FALSE) {
      #   message("## Interpretation: No extreme outlier was identified in your data.", sep="\n")
      # } else {
      #   message("## Interpretation: At least one extreme outlier was identified in your data, probably due to data entry errors, mesurement errors, or unusual values. you need to check the outlier(s) to understand them.You can still include the outlier(s) in the analysis if you don't believe the result will be substantially affected. This can be evaluated by comparing the result of the ANOVA with and without the outlier(s).", sep="\n")
      # }

      message("---------------------------")
      message("# Normality:")
      print(shapiro_wilk_test)
      if (shapiro_wilk_test$p>0.05) {
        message("--> Interpretation: the average test score was normally distributed at each time point, as assessed by Shapiro-Wilk test (p>0.05).", sep="\n")
      } else {
        message("--> Interpretation: the assumption of normality has NOT been met; the average score was not normally distributed at least at one time point. Although the repeated measures ANOVA is known to be robust to a violation of the assumption of normality, you may want/need to refer to the result of the Friedman test, which is a non-parametric version of one-way repeated measures ANOVA in you report.", sep="\n")
      }
      message("If the sample size is greater than 50, it would be better refer to the normal Q-Q plot displayed in the 'Plots' panel to visually inspect the normality. This is because the Shapiro-Wilk test becomes very sensitive to a minor deviation from normality at a larger sample size (>50 in this case).", sep="\n")
      hist(Residuals)
      print(normal_qq_plot)
      message("--> Interpretation: if all the points fall in the plots above approximately along the reference line, you can assume normality.", sep="\n")

      message("-------------")
      message("# Sphericity:")
      message("--> The assumption of sphericity has been checked during the computation of the ANOVA test (the Mauchly's test has been internally run to assess the sphericity assumption). Then, the Greenhouse-Geisser sphericity correction has been automatically applied to factors violating the sphericity of assumption.", sep="\n")

      message("===============================================================")
      message("Result of the main One-way Repeated Measures ANOVA (Parametric)")
      print(one_way_repeated_anova)
      if (one_way_repeated_anova$p<0.001) {
        message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.001, eta2(g)=",result$ges,".")
      } else if (one_way_repeated_anova$p<0.01) {
        message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.01, eta2(g)=",result$ges,".")
      } else if (one_way_repeated_anova$p<0.05) {
        message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.05, eta2(g)=",result$ges,".")
      } else {
        message("--> Interpretation: The average test score at different time points of the intervention are not statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p>0.05, eta2(g)=",result$ges,".")
      }
      message("", sep="\n")
      message("--------------------")
      message("Pairwise Comparisons")
      print(pwc)
      # Between Pre and Post
      if (pwc$p.adj[2]<0.001) {
        if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.001).")
        } else {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
        }
      } else if (pwc$p.adj[2]<0.01) {
        if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.01).")
        } else {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
        }
      } else if (pwc$p.adj[2]<0.05) {
        if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.05).")
        } else {
          message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
        }
      } else {
        message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are not significantly different (p.adj>0.05).")
      }
      # Between Post and Post2
      if (pwc$p.adj[1]<0.001) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.001).")
        } else {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.001).")
        }
      } else if (pwc$p.adj[1]<0.01) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.01).")
        } else {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.01).")
        }
      } else if (pwc$p.adj[1]<0.05) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.05).")
        } else {
          message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.05).")
        }
      } else {
        message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are not significantly different (p.adj>0.05).")
      }
      # Between Pre and Post2
      if (pwc$p.adj[3]<0.001) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.001).")
        } else {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
        }
      } else if (pwc$p.adj[3]<0.01) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.01).")
        } else {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average Post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
        }
      } else if (pwc$p.adj[3]<0.05) {
        if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.05).")
        } else {
          message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
        }
      } else {
        message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are not significantly different (p.adj>0.05).")
      }

      # Display the result of the Friedman test only when the assumption of normality is violated
      if (shapiro_wilk_test$p<0.05) {
        message("---------------------------------------------")
        message("As the result of the Shapiro-Wilk normality test show above, the assumption of normality of residuals has been violated (i.e., p-value from the Shapiro-Wilk test is less than 0.05). Although the repeated measures ANOVA is fairly 'robust' to violations of normality, you may want/need to use the Friedman test results presented below:")
        message("", sep="\n")
        message("===============================================")
        message("Friedman Rank Sum Test - Result (Nonparametric)")
        print(friedman_test)
        if (friedman_test$p.value<0.001) {
          message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.001).")
        } else if (friedman_test$p.value<0.01) {
          message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.01).")
        } else if (friedman_test$p.value<0.05) {
          message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.05).")
        } else {
          message("--> Interpretation: the median test score is not significantly different at the different time points during the intervention (p>0.05).")
        }
        message("", sep="\n")
        message("---------------------------------------------")
        message("Friedman Rank Sum Test - Pairwise Comparisons")
        print(friedman_pwc)
        # Between Pre and post
        if (friedman_pwc$p.adj[2]<0.001) {
          if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.001).")
          } else {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
          }
        } else if (friedman_pwc$p.adj[2]<0.01) {
          if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.01).")
          } else {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
          }
        } else if (friedman_pwc$p.adj[2]<0.05) {
          if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.05).")
          } else {
            message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
          }
        } else {
          message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are not significantly different (p.adj>0.05).")
        }
        # Between post and Post2
        if (friedman_pwc$p.adj[1]<0.001) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.001).")
          } else {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.001).")
          }
        } else if (friedman_pwc$p.adj[1]<0.01) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.01).")
          } else {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.01).")
          }
        } else if (friedman_pwc$p.adj[1]<0.05) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.05).")
          } else {
            message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.05).")
          }
        } else {
          message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are not significantly different (p.adj>0.05).")
        }
        # Between Pre and Post2
        if (friedman_pwc$p.adj[3]<0.001) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.001).")
          } else {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
          }
        } else if (friedman_pwc$p.adj[3]<0.01) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.01).")
          } else {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
          }
        } else if (friedman_pwc$p.adj[3]<0.05) {
          if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.05).")
          } else {
            message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
          }
        } else {
          message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are not significantly different (p.adj>0.05).")
        }
      }
})

      ################################################################

      one_way_repeated_anova <- eventReactive(input$submit5, {
      # binding for global variable
      m_rate <- Time <- Score <- id <- NULL

      # Read all datasets
      data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
      data_treat_post<- read_csv(data11(), show_col_types = FALSE)
      data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

      # Converting multiple-choice answers to binary
      if (input$m_choice5 == "1") {
        if (is.null(answer_key5())) {
          stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
        } else {
          answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
          nrow <- nrow(answer_keys)
          for (i in 1:nrow) {
            data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
            data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
            data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
            data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
            data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
            data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
          }
        }
      }

      # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
      nrow_all <- nrow(data_treat_pre)
      n <- as.numeric(length(data_treat_pre[,-1]))
      data_treat_pre <- data_treat_pre %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

      data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
      nrow_subset <- nrow(data_treat_pre)
      n_students_deleted_pre <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_pre$m_rate=NULL

      # Deleting students with too many skipped answers: data_treat_post.csv-----------------
      nrow_all <- nrow(data_treat_post)
      n <- as.numeric(length(data_treat_post[,-1]))
      data_treat_post <- data_treat_post %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

      data_treat_post <- subset(data_treat_post, m_rate < scale1())
      nrow_subset <- nrow(data_treat_post)
      n_students_deleted_post <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_post$m_rate=NULL

      # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
      nrow_all <- nrow(data_treat_post2)
      n <- as.numeric(length(data_treat_post2[,-1]))
      data_treat_post2 <- data_treat_post2 %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

      data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
      nrow_subset <- nrow(data_treat_post2)
      n_students_deleted_post2 <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_post2$m_rate=NULL

      n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
      colnames(n_students_deleted) <- c("n")
      rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

      # Clean data (e.g., Replace skipped answers with "0")
      n_col <- ncol(data_treat_pre)
      for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
      data_treat_pre[is.na(data_treat_pre)]= 0
      for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
      data_treat_post[is.na(data_treat_post)]= 0
      for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
      data_treat_post2[is.na(data_treat_post2)]= 0

      # Change column names with their origin'sir
      colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
      colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
      colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

      # Calculate average scores
      data_treat_pre <- data_treat_pre %>%
        mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
      data_treat_post <- data_treat_post %>%
        mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
      data_treat_post2 <- data_treat_post2 %>%
        mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

      # Merge pre/post/post2 data
      data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
      treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
      names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

      # Convert Data Frame to a Long Format & Define the Variable
      avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
      df_Long <- melt(avg_score_df)
      names(df_Long) <- c("id", "Time", "Score")
      # Name Time(group) -> 1=Pre, 2=post, 3=Post2
      df_Long$id <- factor(df_Long$id)
      df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

      # Descriptive Statistics
      descriptive_statistics <- df_Long %>%
        group_by(Time) %>%
        get_summary_stats(Score, type="common")
      boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

      # Check Outliers
      outliers <- df_Long %>%
        group_by(Time) %>%
        identify_outliers(Score)

      # Check normality of Residuals
      res.aov <- aov(Score~Time, data=df_Long)
      shapiro_wilk_test <- shapiro.test(resid(res.aov))
      Residuals <- res.aov$residuals
      normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

      # One-way Repeated Measures ANOVA
      res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
      one_way_repeated_anova <- result <- get_anova_table(res.aov)
      repeated_anova_p.value <- result$p

      # Pairwise Comparisons
      one_way_repeated_anova_pwc <- pwc <- df_Long %>%
        pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

      # Friedman Test (Non-parametric)
      friedman_test <- friedman.test(avg_score_df)

      # Pairwise Comparisons - Friedman
      friedman_pwc <- df_Long %>%
        wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

      })

      #------------------------------------------------------------------------------

      n_students_deleted_pre_5 <- reactive({
      # binding for global variable
      m_rate <- Time <- Score <- id <- NULL

      # Read all datasets
      data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
      data_treat_post<- read_csv(data11(), show_col_types = FALSE)
      data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

      # Converting multiple-choice answers to binary
      if (input$m_choice5 == "1") {
        if (is.null(answer_key5())) {
          stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
        } else {
          answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
          nrow <- nrow(answer_keys)
          for (i in 1:nrow) {
            data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
            data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
            data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
            data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
            data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
            data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
          }
        }
      }

      # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
      nrow_all <- nrow(data_treat_pre)
      n <- as.numeric(length(data_treat_pre[,-1]))
      data_treat_pre <- data_treat_pre %>%
        mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

      data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
      nrow_subset <- nrow(data_treat_pre)
      n_students_deleted_pre <- nrow_all-nrow_subset

      # Delete the column of "m_rate" to exclude from the analysis down the road.
      data_treat_pre$m_rate=NULL

   })

      #------------------------------------------------------------------------------

      n_students_deleted_post_5 <- reactive({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

       # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

      })

      #------------------------------------------------------------------------------

      n_students_deleted_post2_5 <- reactive({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

      })

      #------------------------------------------------------------------------------

      descriptive_statistics_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(descriptive_statistics)

      })

      #------------------------------------------------------------------------------

      boxplots_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(boxplots)

      })

      #------------------------------------------------------------------------------

      shapiro_wilk_test_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(shapiro_wilk_test)

      })

      #------------------------------------------------------------------------------

      normal_qq_plot_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(normal_qq_plot)

      })

      res.aov_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(res.aov)

      })

      #------------------------------------------------------------------------------

      one_way_repeated_anova_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(one_way_repeated_anova)

      })

      #------------------------------------------------------------------------------

      one_way_repeated_anova_pwc_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(one_way_repeated_anova_pwc)

      })

      #------------------------------------------------------------------------------

      friedman_test_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(friedman_test)

      })

      #------------------------------------------------------------------------------

      friedman_pwc_5 <- reactive ({
        # binding for global variable
        m_rate <- Time <- Score <- id <- NULL

        # Read all datasets
        data_treat_pre <- read_csv(data10(), show_col_types = FALSE)
        data_treat_post<- read_csv(data11(), show_col_types = FALSE)
        data_treat_post2 <- read_csv(data12(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice5 == "1") {
          if (is.null(answer_key5())) {
            stop("If m_choice is TRUE, then you need to put answer_key5() (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key5(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_treat_pre[,i+1][data_treat_pre[,i+1]!=answer_keys$key[i]]=0
              data_treat_pre[,i+1][data_treat_pre[,i+1]==answer_keys$key[i]]=1
              data_treat_post[,i+1][data_treat_post[,i+1]!=answer_keys$key[i]]=0
              data_treat_post[,i+1][data_treat_post[,i+1]==answer_keys$key[i]]=1
              data_treat_post2[,i+1][data_treat_post2[,i+1]!=answer_keys$key[i]]=0
              data_treat_post2[,i+1][data_treat_post2[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
        nrow_all <- nrow(data_treat_pre)
        n <- as.numeric(length(data_treat_pre[,-1]))
        data_treat_pre <- data_treat_pre %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

        data_treat_pre <- subset(data_treat_pre, m_rate < scale1())
        nrow_subset <- nrow(data_treat_pre)
        n_students_deleted_pre <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_pre$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post.csv-----------------
        nrow_all <- nrow(data_treat_post)
        n <- as.numeric(length(data_treat_post[,-1]))
        data_treat_post <- data_treat_post %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

        data_treat_post <- subset(data_treat_post, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post)
        n_students_deleted_post <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post$m_rate=NULL

        # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
        nrow_all <- nrow(data_treat_post2)
        n <- as.numeric(length(data_treat_post2[,-1]))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

        data_treat_post2 <- subset(data_treat_post2, m_rate < scale1())
        nrow_subset <- nrow(data_treat_post2)
        n_students_deleted_post2 <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_treat_post2$m_rate=NULL

        n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
        colnames(n_students_deleted) <- c("n")
        rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_treat_pre)
        for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
        data_treat_pre[is.na(data_treat_pre)]= 0
        for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
        data_treat_post[is.na(data_treat_post)]= 0
        for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
        data_treat_post2[is.na(data_treat_post2)]= 0

        # Change column names with their origin'sir
        colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
        colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
        colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

        # Calculate average scores
        data_treat_pre <- data_treat_pre %>%
          mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
        data_treat_post <- data_treat_post %>%
          mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
        data_treat_post2 <- data_treat_post2 %>%
          mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

        # Merge pre/post/post2 data
        data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
        treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
        names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

        # Convert Data Frame to a Long Format & Define the Variable
        avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
        df_Long <- melt(avg_score_df)
        names(df_Long) <- c("id", "Time", "Score")
        # Name Time(group) -> 1=Pre, 2=post, 3=Post2
        df_Long$id <- factor(df_Long$id)
        df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

        # Descriptive Statistics
        descriptive_statistics <- df_Long %>%
          group_by(Time) %>%
          get_summary_stats(Score, type="common")
        boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

        # Check Outliers
        outliers <- df_Long %>%
          group_by(Time) %>%
          identify_outliers(Score)

        # Check normality of Residuals
        res.aov <- aov(Score~Time, data=df_Long)
        shapiro_wilk_test <- shapiro.test(resid(res.aov))
        Residuals <- res.aov$residuals
        normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

        # One-way Repeated Measures ANOVA
        res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
        one_way_repeated_anova <- result <- get_anova_table(res.aov)
        repeated_anova_p.value <- result$p

        # Pairwise Comparisons
        one_way_repeated_anova_pwc <- pwc <- df_Long %>%
          pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        # Friedman Test (Non-parametric)
        friedman_test <- friedman.test(avg_score_df)

        # Pairwise Comparisons - Friedman
        friedman_pwc <- df_Long %>%
          wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

        return(friedman_pwc)

      })

      ###############################################################################
      ## Output rendering (or_repeated) #rrr

      output$n_students_deleted_5 <- renderUI({
        one_way_repeated_anova()
        n_students_deleted_5 <-c("",
                               "======================================",
                               paste("The number of students deleted: ", n_students_deleted_pre_5(), " student(s) from the pre-test data, ", n_students_deleted_post_5(), " student(s) from the post-test data, and ", n_students_deleted_post2_5(), " student(s) from the post2-test data have been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
        HTML(paste(n_students_deleted_5, collapse = "<br/>"))
      })

      output$descriptive_statistics_title_5 <- renderUI({
        one_way_repeated_anova()
        descriptive_statistics_title_5 <-c("",
                                             "======================================",
                                             paste("Descriptive Statistics: Average Scores", sep=""))
        HTML(paste(descriptive_statistics_title_5, collapse = "<br/>"))
      })

      output$descriptive_statistics_5 <- renderTable({
        one_way_repeated_anova()
        descriptive_statistics_5()
      })

      output$boxplot_reference_5 <- renderUI({
        one_way_repeated_anova()
        boxplot_reference_5 <-c("--> Refer to the boxplot below for a visual presentation of the descriptive statistics above:")
        HTML(paste(boxplot_reference_5, collapse = "<br/>"))
      })

      output$boxplots_5 <- renderPlot({
        one_way_repeated_anova()
        boxplots_5()
      })

      output$assumption_testing_title_5 <- renderUI({
        one_way_repeated_anova()
        assumption_testing_title_5 <- c("",
                                      "======================================",
                                      paste("Results of Checking Assumptions", sep=""))
        HTML(paste(assumption_testing_title_5, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_title_5 <- renderUI({
        one_way_repeated_anova()
        shapiro_wilk_test_title_5 <-c("",
                                    "-------------------------------------",
                                    paste("Normality - Shapiro-Wilk Test Result", sep=""))
        HTML(paste(shapiro_wilk_test_title_5, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_5 <- renderPrint({
        one_way_repeated_anova()
        shapiro_wilk_test_5()
      })

      output$shapiro_wilk_test_interpret_5 <- renderPrint({
        one_way_repeated_anova()
        if (shapiro_wilk_test_5()$p.value > 0.05) {
          paste0("--> Interpretation: the average test score was normally distributed at each time point, as assessed by Shapiro-Wilk test (p>0.05). Refer to the normal Q-Q plot below to visually inspect the normality of residuals.")
        } else {
          paste0("--> Interpretation: the assumption of normality has NOT been met; the average score was not normally distributed at least at one time point. Although the repeated measures ANOVA is known to be robust to a violation of the assumption of normality, you may want/need to refer to the result of the Friedman test, which is a non-parametric version of one-way repeated measures ANOVA in you report.")
        }
      })

      output$normal_qq_plot_5 <- renderPlot({
        one_way_repeated_anova()
        normal_qq_plot_5()
      })

      output$sphericity_title_5 <- renderUI({
        one_way_repeated_anova()
        sphericity_title_5 <-c("",
                                      "-------------------------------------",
                                      paste("Sphericity", sep=""))
        HTML(paste(sphericity_title_5, collapse = "<br/>"))
      })

      output$sphericity_5 <- renderPrint({
        one_way_repeated_anova()
        paste0("--> The assumption of sphericity has been checked during the computation of the ANOVA test (the Mauchly's test has been internally run to assess the sphericity assumption). Then, the Greenhouse-Geisser sphericity correction has been automatically applied to factors violating the sphericity of assumption.")
      })

      output$one_way_repeated_anova_title_5 <- renderUI({
        one_way_repeated_anova()
        one_way_repeated_anova_title_5 <-c("",
                          "======================================",
                          paste("Results of the main One-way Repeated Measures ANOVA (Parametric)", sep=""))
        HTML(paste(one_way_repeated_anova_title_5, collapse = "<br/>"))
      })

      # output$res.aov_5 <- renderTable({
      #   one_way_repeated_anova()
      #   res.aov_5()
      # })

      output$one_way_repeated_anova_5 <- renderTable({
        one_way_repeated_anova()
        one_way_repeated_anova_5()
      })

      output$one_way_repeated_anova_interpret_5 <- renderPrint({
        one_way_repeated_anova()
        if (one_way_repeated_anova_5()$p<0.001) {
          paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",one_way_repeated_anova_5()$DFn," ",one_way_repeated_anova_5()$DFd,")=",one_way_repeated_anova_5()$F,", p<0.001, eta2(g)=",one_way_repeated_anova_5()$ges,".")
        } else if (one_way_repeated_anova_5()$p<0.01) {
          paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",one_way_repeated_anova_5()$DFn," ",one_way_repeated_anova_5()$DFd,")=",one_way_repeated_anova_5()$F,", p<0.01, eta2(g)=",one_way_repeated_anova_5()$ges,".")
        } else if (one_way_repeated_anova_5()$p<0.05) {
          paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",one_way_repeated_anova_5()$DFn," ",one_way_repeated_anova_5()$DFd,")=",one_way_repeated_anova_5()$F,", p<0.05, eta2(g)=",one_way_repeated_anova_5()$ges,".")
        } else {
          paste0("--> Interpretation: The average test score at different time points of the intervention are not statistically different: F(",one_way_repeated_anova_5()$DFn," ",one_way_repeated_anova_5()$DFd,")=",one_way_repeated_anova_5()$F,", p>0.05, eta2(g)=",one_way_repeated_anova_5()$ges,".")
        }
      })

      output$one_way_repeated_anova_pwc_title_5 <- renderUI({
        one_way_repeated_anova()
        one_way_repeated_anova_pwc_title_5 <-c("",
                                           "======================================",
                                           paste("One-way Repeated Measures ANOVA - Pairwise Comparisons", sep=""))
        HTML(paste(one_way_repeated_anova_pwc_title_5, collapse = "<br/>"))
      })

      output$one_way_repeated_anova_pwc_5 <- renderTable({
        one_way_repeated_anova()
        one_way_repeated_anova_pwc_5()
      })

      output$one_way_repeated_anova_pwc_interpret_5 <- renderPrint({
        one_way_repeated_anova()
        # Between Pre and Post
        if (one_way_repeated_anova_pwc_5()$p.adj[2]<0.001) {
          if((descriptive_statistics_5()$mean[2]-descriptive_statistics_5()$mean[1])>0) {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.001).")
          } else {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[2]<0.01) {
          if((descriptive_statistics_5()$mean[2]-descriptive_statistics_5()$mean[1])>0) {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.01).")
          } else {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[2]<0.05) {
          if((descriptive_statistics_5()$mean[2]-descriptive_statistics_5()$mean[1])>0) {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.05).")
          } else {
            paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
          }
        } else {
          paste0("--> Interpretation for 1: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post-test score (",descriptive_statistics_5()$mean[2],") are not significantly different (p.adj>0.05).")
        }
        # Between Post and Post2
        if (one_way_repeated_anova_pwc_5()$p.adj[1]<0.001) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.001).")
          } else {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.001).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[1]<0.01) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.01).")
          } else {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.01).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[1]<0.05) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.05).")
          } else {
            paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.05).")
          }
        } else {
          paste0("--> Interpretation for 2: The average post-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are not significantly different (p.adj>0.05).")
        }
        # Between Pre and Post2
        if (one_way_repeated_anova_pwc_5()$p.adj[3]<0.001) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.001).")
          } else {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[3]<0.01) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.01).")
          } else {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[2],") and the average Post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
          }
        } else if (one_way_repeated_anova_pwc_5()$p.adj[3]<0.05) {
          if((descriptive_statistics_5()$mean[3]-descriptive_statistics_5()$mean[2])>0) {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.05).")
          } else {
            paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[2],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
          }
        } else {
          paste0("--> Interpretation for 3: The average pre-test score (",descriptive_statistics_5()$mean[1],") and the average post2-test score (",descriptive_statistics_5()$mean[3],") are not significantly different (p.adj>0.05).")
        }
      })

      # output$nonparametric_test_title_5 <- renderUI({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   nonparametric_test_title_5 <-c("",
      #                            "======================================",
      #                            paste("Non-parametric test", sep=""))
      #   HTML(paste(nonparametric_test_title_5, collapse = "<br/>"))
      # })
      #
      # output$nonparametric_test_interpret_5 <- renderPrint({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   paste0("As the result of the Shapiro-Wilk normality test show above, the assumption of normality of residuals has been violated (i.e., p-value from the Shapiro-Wilk test is less than 0.05). Although the repeated measures ANOVA is fairly 'robust' to violations of normality, you may want/need to use the Friedman test results presented below:")
      # })
      #
      # output$friedman_test_title_5 <- renderUI({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   friedman_test_title_5 <-c("",
      #                                      "======================================",
      #                                      paste("Friedman Rank Sum Test - Result (Nonparametric)", sep=""))
      #   HTML(paste(friedman_test_title_5, collapse = "<br/>"))
      # })
      #
      # output$friedman_test_5 <- renderTable({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   friedman_test_5()
      # })
      #
      # output$friedman_test_interpret_5 <- renderPrint({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   if (friedman_test$p.value<0.001) {
      #     paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.001).")
      #   } else if (friedman_test$p.value<0.01) {
      #     paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.01).")
      #   } else if (friedman_test$p.value<0.05) {
      #     paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.05).")
      #   } else {
      #     paste0("--> Interpretation: the median test score is not significantly different at the different time points during the intervention (p>0.05).")
      #   }
      # })
      #
      # output$friedman_pwc_title_5 <- renderUI({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   friedman_pwc_title_5 <-c("",
      #                             "======================================",
      #                             paste("Friedman Rank Sum Test - Pairwise Comparisons", sep=""))
      #   HTML(paste(friedman_pwc_title_5, collapse = "<br/>"))
      # })
      #
      # output$friedman_pwc_5 <- renderTable({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   friedman_pwc_5
      # })
      #
      # output$friedman_pwc_interpret_5 <- renderPrint({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_5()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   one_way_repeated_anova()
      #   # Between Pre and post
      #   if (friedman_pwc$p.adj[2]<0.001) {
      #     if((descriptive_statistics_5()$median[2]-descriptive_statistics_5()$median[1])>0) {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.001).")
      #     } else {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
      #     }
      #   } else if (friedman_pwc$p.adj[2]<0.01) {
      #     if((descriptive_statistics_5()$median[2]-descriptive_statistics_5()$median[1])>0) {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.01).")
      #     } else {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
      #     }
      #   } else if (friedman_pwc$p.adj[2]<0.05) {
      #     if((descriptive_statistics_5()$median[2]-descriptive_statistics_5()$median[1])>0) {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.05).")
      #     } else {
      #       paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
      #     }
      #   } else {
      #     paste0("--> Interpretation for 1: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post-test score (",descriptive_statistics_5()$median[2],") are not significantly different (p.adj>0.05).")
      #   }
      #   # Between post and Post2
      #   if (friedman_pwc$p.adj[1]<0.001) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.001).")
      #     } else {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.001).")
      #     }
      #   } else if (friedman_pwc$p.adj[1]<0.01) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.01).")
      #     } else {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.01).")
      #     }
      #   } else if (friedman_pwc$p.adj[1]<0.05) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.05).")
      #     } else {
      #       paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.05).")
      #     }
      #   } else {
      #     paste0("--> Interpretation for 2: the median post-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are not significantly different (p.adj>0.05).")
      #   }
      #   # Between Pre and Post2
      #   if (friedman_pwc$p.adj[3]<0.001) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.001).")
      #     } else {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
      #     }
      #   } else if (friedman_pwc$p.adj[3]<0.01) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.01).")
      #     } else {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
      #     }
      #   } else if (friedman_pwc$p.adj[3]<0.05) {
      #     if((descriptive_statistics_5()$median[3]-descriptive_statistics_5()$median[2])>0) {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.05).")
      #     } else {
      #       paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[2],") and the median post2-test score (",descriptive_statistics_5()$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
      #     }
      #   } else {
      #     paste0("--> Interpretation for 3: the median pre-test score (",descriptive_statistics_5()$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are not significantly different (p.adj>0.05).")
      #   }
      # })


      ###############################################################################
      ###############################################################################
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
      # rrr
      output$demo_group_diff_output <- renderUI({
        if (input$selected_function == "Demographic Group Differences Analysis") {
          tagList(
            textOutput("error6_1"),
            textOutput("error6_2"),
            htmlOutput("n_students_deleted_6"),
            htmlOutput("descriptive_statistics_title_6"),
            tableOutput("descriptive_statistics_6"),
            htmlOutput("boxplot_reference_6"),
            plotOutput("boxplots_6"),
            htmlOutput("assumption_testing_title_6"),
            htmlOutput("shapiro_wilk_test_title_6"),
            verbatimTextOutput("shapiro_wilk_test_6"),
            verbatimTextOutput("shapiro_wilk_test_interpret_6"),
            plotOutput("normal_qq_plot_6"),
            htmlOutput("levene_test_title_6"),
            tableOutput("levene_test_6"),
            verbatimTextOutput("levene_test_interpret_6"),
            htmlOutput("one_way_anova_title_6"),
            verbatimTextOutput("one_way_anova_6"),
            htmlOutput("one_way_anova_pwc_title_6"),
            verbatimTextOutput("one_way_anova_pwc_6"),
            htmlOutput("kruskal_wallis_test_title_6"),
            verbatimTextOutput("explain_nonparametric_6"),
            tableOutput("kruskal_wallis_test_6"),
            htmlOutput("kruskal_wallis_test_pwc_title_6"),
            tableOutput("kruskal_wallis_test_pwc_6"),
          )
        }
      })

      ####################################################################

      observe({
        column_names <- names(data())[-1]
        updateSelectInput(session, "group_var", choices = column_names)
      })

      ##############################
      error6_1 <- reactive({
        input$submit6
        validate(
          need(input$file13, "Please upload a data file to analyze!"),
        )
        paste("The data file name uploaded for demographic group differences analysis: ", input$file13$name)
      })
      output$error6_1 <- renderText({
        error6_1()
      })

      error6_2 <- reactive({
        input$submit6
        validate(
          need(input$file14, "Please upload a demographic group data file to use for analysis!")
        )
        paste("The demographic group data file name uploaded for demographic group differences analysis: ", input$file14$name)
      })
      output$error6_2 <- renderText({
        error6_2()
      })
      ##############################

      ##############################
      observeEvent(input$submit6, {

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        # Check homogeneity of variances
        levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

        ## Kruskal-Wallis Test: Non-parametric version of one-way ANOVA
        kruskal_wallis_test <- kruskal.test(average_score ~ group, data=data_original)
        suppressWarnings({
          kruskal_wallis_test_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group,
                                                          p.adjust.method = "BH")
        })

        ####### Present the results in the console panel for the convenience of users
        message("==============================")
        message("The number of students deleted: ",n_students_deleted, " student(s) has(have) been deleted from the data since they have more than ",as.numeric(scale1())*100,"% of skipped answers.")
        message("", sep="\n")
        message("======================")
        message("Descriptive Statistics")
        print(descriptive_statistics)
        message("Refer to the boxplot in the 'Plots' panel.")
        print(boxplots)
        message("", sep="\n")
        message("==============================")
        message("Results of Testing Assumptions", sep="\n")
        message("# Normality of Residuals:")
        print(shapiro_wilk_test)
        if (shapiro_wilk_test$p.value > 0.05) {
          message("## Interpretation: the assumption of normality by group has been met (p>0.05).", sep="\n")
        } else {
          message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue).", sep="\n")
        }
        message("Refer to the histogram and the normal Q-Q plot in the 'Plots' panel to visually inspect the normality of residuals.", sep="\n")
        hist(Residuals)
        print(normal_qq_plot)

        message("-------------------------")
        message("Homogeneity of Variances:")
        print(levene_test)
        if (levene_test$`Pr(>F)`[1] > 0.05) {
          message("## Interpretation: the assumption of equality of variances has been met (p>0.05).", sep="\n")
        } else {
          message("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05).", sep="\n")
        }
        plot(one_way_anova,1)
        if (levene_test$`Pr(>F)`[1]>0.05) {
          message("===================================================================================")
          message("Results of One-way ANOVA: Group Difference(s) (Parametric: Equal variances assumed)")
          print(summary(one_way_anova))
          message("----------------------------------------------")
          message("Pairwide Comparisons (Equal variances assumed)")
          print(one_way_anova_pwc)
        } else {
          message("Since the assumption of equal variances has NOT been satisfied, the Welch one-way test and the Games-Howell post-hoc test results are presented below.")
          message("=====================================================================================")
          message("Results of One-way ANOVA: Group Difference(s) (Parametric: Unequal variances assumed)")
          print(summary(welch_anova_test))
          message("--------------------")
          message("Pairwide Comparisons (Unequal variances assumed)")
          print(games_howell_test)
        }
        if (shapiro_wilk_test$p.value < 0.05) {
          message("----------------------------------------------------------")
          message("As shown above in the the result of the Shapiro-Wilk test, the assumption of normality is violated. Although ANOVA is known to be robust to a violation of normality, you may want/need to use the Kruskal-Wallis test result presented below")
          message("===============================================")
          message("Results of Kruskal_Wallis Test (Non-parametric)")
          print(kruskal_wallis_test)
          print(kruskal_wallis_test_pwc)
        }

      })
      ##############################

      ##############################
      demo_group_diff <- eventReactive(input$submit6, {

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        # Check homogeneity of variances
        levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

        ## Kruskal-Wallis Test: Non-parametric version of one-way ANOVA
        kruskal_wallis_test <- kruskal.test(average_score ~ group, data=data_original)
        suppressWarnings({
          kruskal_wallis_test_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group,
                                                          p.adjust.method = "BH")
        })

      })
      ##############################

      ##############################
      n_students_deleted_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        nrow_all-nrow_subset

      })
      ##############################


      ##############################
      descriptive_statistics_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        return(descriptive_statistics)

      })
      ##############################


      ##############################
      boxplots_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        return(boxplots)

      })
      ##############################


      ##############################
      shapiro_wilk_test_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        return(shapiro_wilk_test)

      })
      ##############################


      ##############################
      normal_qq_plot_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        return(normal_qq_plot)

      })
      ##############################


      ##############################
      levene_test_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        # Check homogeneity of variances
        levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

        return(levene_test)

      })
      ##############################


      ##############################
      one_way_anova_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        return(one_way_anova)

      })
      ##############################


      ##############################
      one_way_anova_pwc_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        return(one_way_anova_pwc)

      })
      ##############################


      ##############################
      welch_anova_test_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)

        return(welch_anova_test)

      })
      ##############################


      ##############################
      games_howell_test_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        return(games_howell_test)

      })
      ##############################


      ##############################
      kruskal_wallis_test_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        # Check homogeneity of variances
        levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

        ## Kruskal-Wallis Test: Non-parametric version of one-way ANOVA
        kruskal_wallis_test <- kruskal.test(average_score ~ group, data=data_original)
        suppressWarnings({
          kruskal_wallis_test_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group,
                                                          p.adjust.method = "BH")
        })

        return(kruskal_wallis_test)

      })
      ##############################


      ##############################
      kruskal_wallis_test_pwc_6 <- reactive({

        # binding for global variable
        m_rate <- all_of <- group <- average_score <- NULL

        # Reading
        data_original <- read_csv(data13(),col_types = cols())
        n_col <- ncol(data_original)
        demographic_data <- read_csv(data14(), show_col_types = FALSE)

        # Converting multiple-choice answers to binary
        if (input$m_choice6 == "1") {
          if (is.null(answer_key6())) {
            stop("If m_choice is 'Yes,' then you need to upload 'Data with Multiple-choice Answers (e.g., 'answer_keys.csv').")
          } else {
            answer_keys <- read_csv(answer_key6(), show_col_types = FALSE)
            nrow <- nrow(answer_keys)
            for (i in 1:nrow) {
              data_original[,i+1][data_original[,i+1]!=answer_keys$key[i]]=0
              data_original[,i+1][data_original[,i+1]==answer_keys$key[i]]=1
            }
          }
        }

        # Deleting students with too many skipped answers: data_original.csv-----------------
        nrow_all <- nrow(data_original)
        n <- as.numeric(length(data_original[,-1]))
        data_original <- data_original %>%
          mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

        data_original <- subset(data_original, m_rate < scale1())
        nrow_subset <- nrow(data_original)
        n_students_deleted <- nrow_all-nrow_subset

        # Delete the column of "m_rate" to exclude from the analysis down the road.
        data_original$m_rate=NULL
        message("", sep="\n")
        #----------------------------------------------------------------

        # Clean data (e.g., Replace skipped answers with "0")
        n_col <- ncol(data_original)
        for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
        data_original[is.na(data_original)]= 0

        # Calculate average scores
        data_original <- data_original %>%
          mutate(avg_score = round(rowMeans(data_original[,-1]),3))

        # Merge test data and demographic data
        data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

        data_original <- select(data_original, c('avg_score', all_of(input$group_var)))
        names(data_original) <- c("average_score", "group")
        data_original <- data_original[complete.cases(data_original),]

        # Descriptive statistics
        descriptive_statistics <- data_original %>%
          group_by(group) %>%
          get_summary_stats(average_score, type = "mean_sd")
        boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

        # Check Outliers
        outliers <- data_original %>%
          group_by(group) %>%
          identify_outliers(average_score)

        # Conduct one-way ANOVA and pairwise sub-group comparisons
        one_way_anova <- aov(average_score ~ factor(group), data=data_original)
        one_way_anova_pwc <- TukeyHSD(one_way_anova)

        welch_anova_test <- data_original %>%
          welch_anova_test(average_score ~ group)
        games_howell_test <- data_original %>%
          games_howell_test(average_score ~ group)

        # Check normality of residuals
        shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
        normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
        Residuals <- residuals(one_way_anova)

        # Check homogeneity of variances
        levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

        ## Kruskal-Wallis Test: Non-parametric version of one-way ANOVA
        kruskal_wallis_test <- kruskal.test(average_score ~ group, data=data_original)
        suppressWarnings({
          kruskal_wallis_test_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group,
                                                          p.adjust.method = "BH")
        })

        return(kruskal_wallis_test_pwc)

      })
      ##############################

      ############################################################
      ## Output rendering (or_demo)

      output$n_students_deleted_6 <- renderUI({
        demo_group_diff()
        n_students_deleted_6 <-c("",
                               "======================================",
                               paste("The number of students deleted: ", n_students_deleted_6(), " student(s) has(have) been deleted since they have more than ",as.numeric(scale1())*100,"% of skipped answers.", sep=""))
        HTML(paste(n_students_deleted_6, collapse = "<br/>"))
      })

      output$descriptive_statistics_title_6 <- renderUI({
        demo_group_diff()
        descriptive_statistics_title_6 <-c("",
                                             "======================================",
                                             paste("Descriptive Statistics", sep=""))
        HTML(paste(descriptive_statistics_title_6, collapse = "<br/>"))
      })

      output$descriptive_statistics_6 <- renderTable({
        demo_group_diff()
        descriptive_statistics_6()
      })

      output$boxplot_reference_6 <- renderUI({
        demo_group_diff()
        boxplot_reference_6 <-c("--> Refer to the boxplot below for a visual presentation of the descriptive statistics above:")
        HTML(paste(boxplot_reference_6, collapse = "<br/>"))
      })

      output$boxplots_6 <- renderPlot({
        demo_group_diff()
        boxplots_6()
      })

      output$assumption_testing_title_6 <- renderUI({
        demo_group_diff()
        assumption_testing_title_6 <- c("",
                                      "======================================",
                                      paste("Results of Testing Assumptions", sep=""))
        HTML(paste(assumption_testing_title_6, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_title_6 <- renderUI({
        demo_group_diff()
        shapiro_wilk_test_title_6 <-c("",
                                    "-------------------------------------",
                                    paste("Normality of Residuals - Shapiro-Wilk Test Result", sep=""))
        HTML(paste(shapiro_wilk_test_title_6, collapse = "<br/>"))
      })

      output$shapiro_wilk_test_6 <- renderPrint({
        demo_group_diff()
        shapiro_wilk_test_6()
      })

      output$shapiro_wilk_test_interpret_6 <- renderPrint({
        demo_group_diff()
        if (shapiro_wilk_test_6()$p.value > 0.05) {
          paste0("--> Interpretation: the assumption of normality by group has been met (p>0.05). Refer to the normal Q-Q plot below to visually inspect the normality of residuals.")
        } else {
          paste0("--> Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue). Refer to the normal Q-Q plot below to visually inspect the normality of residuals.")
        }
      })

      output$normal_qq_plot_6 <- renderPlot({
        demo_group_diff()
        normal_qq_plot_6()
      })

      output$levene_test_title_6 <- renderUI({
        demo_group_diff()
        levene_test_title_6 <-c("",
                              "-------------------------------------",
                              paste("Homogeneity of Variances - Levene Test Result", sep=""))
        HTML(paste(levene_test_title_6, collapse = "<br/>"))
      })

      output$levene_test_6 <- renderTable({
        demo_group_diff()
        levene_test_6()
      })

      output$levene_test_interpret_6 <- renderPrint({
        demo_group_diff()
        if (levene_test_6()$`Pr(>F)`[1] > 0.05) {
          paste0("--> Interpretation: the assumption of equality of variances has been met (p>0.05).")
        } else {
          paste0("--> Interpretation: the assumption of equality of variances has NOT been met (p<0.05).")
        }
      })

      output$one_way_anova_title_6 <- renderUI({
        demo_group_diff()
        if (levene_test_6()$`Pr(>F)`[1]>0.05) {
          one_way_anova_title_6 <-c("",
                                    "======================================",
                                    paste("Results of One-way ANOVA (Parametric: Equal variances assumed)", sep=""))
          HTML(paste(one_way_anova_title_6, collapse = "<br/>"))
        } else {
          one_way_anova_title_6 <-c("",
                                    "======================================",
                                    paste("Results of One-way ANOVA (Parametric: Unequal variances assumed)", sep=""))
          HTML(paste(one_way_anova_title_6, collapse = "<br/>"))
        }
      })

      output$one_way_anova_6 <- renderPrint({
        demo_group_diff()
        if (levene_test_6()$`Pr(>F)`[1]>0.05) {
          one_way_anova_6()
        } else {
          welch_anova_test_6()
        }
      })

      output$one_way_anova_pwc_title_6 <- renderUI({
        demo_group_diff()
        if (levene_test_6()$`Pr(>F)`[1]>0.05) {
          one_way_anova_pwc_title_6 <-c("",
                                    "-------------------------------------",
                                    paste("Pairwide Comparisons (Equal variances assumed)", sep=""))
          HTML(paste(one_way_anova_pwc_title_6, collapse = "<br/>"))
        } else {
          games_howell_test_title_6 <-c("",
                                    "-------------------------------------",
                                    paste("Pairwide Comparisons (Unequal variances assumed)", sep=""))
          HTML(paste(games_howell_test_title_6, collapse = "<br/>"))
        }
      })

      output$one_way_anova_pwc_6 <- renderPrint({
        demo_group_diff()
        if (levene_test_6()$`Pr(>F)`[1]>0.05) {
          one_way_anova_pwc_6()
        } else {
          games_howell_test_6()
        }
      })

      # output$kruskal_wallis_test_title_6 <- renderUI({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_6()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   demo_group_diff()
      #   kruskal_wallis_test_title_6 <-c("",
      #                             "======================================",
      #                             paste("Results of Kruskal_Wallis Test (Non-parametric)", sep=""))
      #   HTML(paste(kruskal_wallis_test_title_6, collapse = "<br/>"))
      # })
      #
      # output$explain_nonparametric_6 <- renderPrint({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_6()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   demo_group_diff()
      #   paste("As shown above in the the result of the Shapiro-Wilk test, the assumption of normality is violated. Although ANOVA is known to be robust to a violation of normality, you may want/need to use the Kruskal-Wallis test result presented below:", sep="")
      # })
      #
      # output$kruskal_wallis_test_6 <- renderTable({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_6()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   demo_group_diff()
      #   kruskal_wallis_test_6()
      # })
      #
      # output$kruskal_wallis_test_pwc_title_6 <- renderUI({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_6()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   demo_group_diff()
      #   kruskal_wallis_test_pwc_title_6 <-c("",
      #                                       "-------------------------------------",
      #                                       paste("Pairwise Comparisons (Non-parametric)", sep=""))
      #   HTML(paste(kruskal_wallis_test_pwc_title_6, collapse = "<br/>"))
      # })
      #
      # output$kruskal_wallis_test_pwc_6 <- renderTable({
      #   # If the normality assumption for the parametric ANOVA is met, abort the rendering process
      #   if (shapiro_wilk_test_6()$p>0.05) {
      #     return(NULL)  # Abort rendering and produce no output
      #   }
      #   demo_group_diff()
      #   kruskal_wallis_test_pwc_6()
      # })

}

shinyApp(ui, server)
