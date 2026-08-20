library(shiny)

library(corrplot)

library(rmarkdown)

library(bslib)
library(DT)

library(ggplot2)

library(plotly)



# Load external functions

source("clustering.R")

source("anomaly.R")

source("data_cleaning.R")

source("pattern_detection.R")



ui <- fluidPage(
  
  
  
  theme = bs_theme(
    
    version = 5,
    
    bootswatch = "flatly",
    
    primary = "#0d6efd",
    
    secondary = "#6c757d"
    
  ),
  
  
  
  titlePanel(
    
    div(
      
      style = "text-align:center;",
      
      h1("📊 Zynith AI💫"),
      
      h4("Intelligent Dataset Analytics & Pattern Detection Platform")
      
    )
    
  ),
  
  
  
  fluidRow(
    
    
    
    column(
      
      3,
      
      wellPanel(
        
        
        
        style = "

  background:#ffffff;

  border-radius:18px;

  box-shadow:0 4px 12px rgba(0,0,0,0.1);

  text-align:center;

  padding:15px;

  ",
        
        
        
        h4("📄 Rows"),
        
        div(
          
          style="font-size:34px;

         font-weight:bold;

         color:#2563EB;",
          
          textOutput("rows", inline = TRUE)
          
        )
        
      )
      
    ),
    
    
    
    column(
      
      3,
      
      wellPanel(
        
        
        
        style = "

  background:#ffffff;

  border-radius:18px;

  box-shadow:0 4px 12px rgba(0,0,0,0.1);

  text-align:center;

  padding:15px;

  ",
        
        
        
        h4("📊 Columns"),
        
        div(
          
          style="font-size:34px;

         font-weight:bold;

         color:#16A34A;",
          
          textOutput("columns", inline = TRUE)
          
        )
        
      )
      
    ),
    
    
    
    column(
      
      3,
      
      wellPanel(
        
        h4("❗ Missing"),
        
        div(
          
          style="font-size:34px;

         font-weight:bold;

         color:#DC2626;",
          
          textOutput("missing", inline = TRUE)
          
        )      )
      
    ),
    
    
    
    column(
      
      3,
      
      wellPanel(
        
        h4("💚 Health"),
        
        div(
          
          style="font-size:34px;

         font-weight:bold;

         color:#059669;",
          
          textOutput("health", inline = TRUE)
          
        )      )
      
    )
    
    
    
  ),
  
  
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      
      wellPanel(
        
        
        
        style="

      background:#ffffff;

      border-radius:18px;

      box-shadow:0 4px 12px rgba(0,0,0,0.1);

      padding:18px;

    ",
        
        
        
        h4("📂 Upload Dataset"),
        
        
        
        fileInput(
          
          "file",
          
          NULL,
          
          accept = c(".csv")
          
        ),
        
        
        
        p(
          
          "Supported format: CSV (.csv)",
          
          style="color:gray;"
          
        )
        
        
        
      )
      
      
      
    ),
    
    
    
    mainPanel(
      
      
      
      h3("Dataset Summary"),
      
      uiOutput("summary"),
      
      
      
      h3("🤖 AI Summary"),
      
      uiOutput("aiSummary"),
      
      
      
      
      
      hr(),
      
      
      
      h3("Dataset Statistics"),
      
      tableOutput("statistics"),
      
      br(),
      
      hr(),
      
      
      
      h3("Dataset Preview"),
      
      tableOutput("table"),

      hr(),
      
      h3("📊 Interactive Data Visualization"),
      
      selectInput(
        "chartType",
        "Select Chart",
        choices = c(
          "Histogram",
          "Box Plot",
          "Scatter Plot"
        )
      ),
      
      selectInput(
        "xColumn",
        "Select X Column",
        choices = NULL
      ),
      
      selectInput(
        "yColumn",
        "Select Y Column (Scatter Only)",
        choices = NULL
      ),
      
      plotlyOutput("interactivePlot", height = "500px"),
      hr(),
      
      actionButton(
        inputId = "removeDup",
        label = "Remove Duplicates",
        class = "btn-primary"
      ),
br(),
br(),

selectInput(
  "missingMethod",
  "Handle Missing Values",
  choices = c("Mean", "Median", "Mode", "Remove Rows")
),

actionButton("cleanData", "Clean Dataset"),

br(),
br(),

downloadButton("downloadData", "⬇ Download Cleaned CSV"),

br(),
br(),

downloadButton("downloadReport", "📄 Download Report"),

hr(),

h3("🤖 AI Insights"),
verbatimTextOutput("aiInsights"),

hr(),

h3("💡 AI Recommendations"),
verbatimTextOutput("recommendation"),

hr(),
h3("🧠 AI Dataset Type"),

wellPanel(
  h4("Detected Problem Type"),
  textOutput("datasetType")
),

hr(),

h3("📈 Pattern Detection"),
verbatimTextOutput("patterns"),

hr(),

h3("🏥 Dataset Health Score"),
verbatimTextOutput("healthScore"),
h2("🤖 Zynith AI Chatbot"),

textInput(
  "chatQuestion",
  "Ask Zynith AI:",
  placeholder = "Example: How many rows are there?"
),

actionButton(
  "askAI",
  "Ask AI",
  class = "btn-primary"
),

verbatimTextOutput("chatAnswer")
)
)
)

server <- function(input, output, session){
  cleanedData <- reactiveVal()
  
  data <- reactive({
    
    req(input$file)
    
    read.csv(input$file$datapath)
    
  })
  
  
  observe({
    
    req(data())
    
    nums <- names(data())[sapply(data(), is.numeric)]
    
    updateSelectInput(
      session,
      "xColumn",
      choices = nums
    )
    
    updateSelectInput(
      session,
      "yColumn",
      choices = nums
    )
    
  })
  
  output$table <- renderTable({
    head(data())
    
  })
  
  output$corrPlot <- renderPlot({
    
    numData <- data()[, sapply(data(), is.numeric), drop = FALSE]
    
    corr <- cor(numData)
    
    corrplot(
      corr,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.srt = 45
    )
    
  })
      


  
  output$summary <- renderUI({
    
    req(data())
    
    tagList(
      tags$p(tags$b("📄 Total Rows : "), nrow(data())),
      tags$p(tags$b("📊 Total Columns : "), ncol(data())),
      tags$p(tags$b("❗ Missing Values : "), sum(is.na(data()))),
      tags$p(tags$b("📋 Duplicate Rows : "), sum(duplicated(data())))
    )
    
  })
  
 
  output$corrPlot <- renderPlot({
    
    numData <- data()[, sapply(data(), is.numeric), drop = FALSE]
    
    corr <- cor(numData)
    
    corrplot(
      corr,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.srt = 45
    )
    
  })
  output$clusterPlot <- renderPlot({
    
    cluster_plot(data())
    
  })
  
  output$anomalyPlot <- renderPlot({
    
    anomaly_detect(data())
    
  })
  
  
  output$interactivePlot <- renderPlotly({
    
    req(data())
    req(input$xColumn)
    
    df <- data()
    
    if (input$chartType == "Histogram") {
      
      p <- ggplot(df, aes_string(x = input$xColumn)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "white")
      
    } else if (input$chartType == "Box Plot") {
      
      p <- ggplot(df, aes_string(y = input$xColumn)) +
        geom_boxplot(fill = "orange")
      
    } else {
      
      req(input$yColumn)
      
      p <- ggplot(df,
                  aes_string(
                    x = input$xColumn,
                    y = input$yColumn
                  )) +
        geom_point(color = "red", size = 3)
      
    }
    
    plotly::ggplotly(p)
    
  })
  
  observeEvent(input$removeDup, {
   
    cleaned <- unique(data())
    
    cleanedData(cleaned)
    
    showNotification("Duplicate rows removed!")
    
  })
  
  observeEvent(input$cleanData, {
    
    cleaned <- clean_dataset(
      data(),
      input$missingMethod
    )
    
    cleanedData(cleaned)
    
    showNotification("Dataset cleaned successfully!")
    
  })
  
  
  output$aiSummary <- renderUI({
    
    req(data())
    
    totalRows <- nrow(data())
    totalCols <- ncol(data())
    missing <- sum(is.na(data()))
    
    tagList(
      
      tags$p(paste("📄 Dataset contains", totalRows, "rows and", totalCols, "columns.")),
      
      tags$p(
        if (missing == 0)
          "✅ No missing values detected."
        else
          paste("⚠️", missing, "missing values detected.")
      ),
      
      tags$p("🤖 Dataset is ready for Machine Learning.")
      
    )
    
  })
  
   
  
  # ---------------- Dataset Statistics ----------------
  
  output$statistics <- renderTable({
    
    req(data())
    
    numericData <- data()[, sapply(data(), is.numeric)]
    
    stats <- data.frame(
      Feature = names(numericData),
      Mean = round(sapply(numericData, mean), 2),
      Median = round(sapply(numericData, median), 2),
      SD = round(sapply(numericData, sd), 2),
      Min = round(sapply(numericData, min), 2),
      Max = round(sapply(numericData, max), 2)
    )
    
    stats
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "m",
  width = "100%"
  )
  
  # ---------------- AI Insights ----------------
  
  output$aiInsights <- renderPrint({
    
    req(data())
    
    df <- if (is.null(cleanedData())) data() else cleanedData()
    
    rows <- nrow(df)
    cols <- ncol(df)
    missing <- sum(is.na(df))
    duplicate <- sum(duplicated(df))
    
    cat("📊 AI DATASET INSIGHTS\n")
    cat("-------------------------\n")
    cat("Rows :", rows, "\n")
    cat("Columns :", cols, "\n")
    cat("Missing Values :", missing, "\n")
    cat("Duplicate Rows :", duplicate, "\n\n")
    
    if (missing == 0 && duplicate == 0) {
      cat("✅ Dataset Quality : Excellent")
    } else if (missing < 10) {
      cat("🟡 Dataset Quality : Good")
    } else {
      cat("🔴 Dataset Quality : Needs Cleaning")
    }
    
  })
  
  # ---------------- AI Recommendations ----------------
  
  output$recommendation <- renderPrint({
    
    req(data())
    
    df <- data()
    
    missing <- sum(is.na(df))
    duplicate <- sum(duplicated(df))
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    categorical_cols <- names(df)[sapply(df, function(x)
      is.character(x) || is.factor(x)
    )]
    
    cat("🤖 ZYNITH AI RECOMMENDATIONS\n")
    cat("==============================\n\n")
    
    # Missing Values
    if (missing == 0) {
      
      cat("✅ Data Completeness\n")
      cat("   No missing values detected.\n\n")
      
    } else {
      
      cat("⚠️ Data Completeness\n")
      cat("   ", missing,
          " missing values detected.\n",
          "   Recommendation: Handle missing values before ML.\n\n",
          sep = "")
      
    }
    
    # Duplicate Rows
    if (duplicate == 0) {
      
      cat("✅ Duplicate Check\n")
      cat("   No duplicate rows detected.\n\n")
      
    } else {
      
      cat("⚠️ Duplicate Check\n")
      cat("   ", duplicate,
          " duplicate row(s) detected.\n",
          "   Recommendation: Remove duplicates before training.\n\n",
          sep = "")
      
    }
    
    # Numeric Features
    if (length(numeric_cols) >= 2) {
      
      cat("📊 Feature Analysis\n")
      cat("   ", length(numeric_cols),
          " numeric features available.\n",
          "   Recommendation: Suitable for correlation and clustering analysis.\n\n",
          sep = "")
      
    }
    
    # Categorical Features
    if (length(categorical_cols) > 0) {
      
      cat("🔤 Categorical Features\n")
      cat("   ", length(categorical_cols),
          " categorical feature(s) detected.\n",
          "   Recommendation: Encode categorical variables before ML.\n\n",
          sep = "")
      
    }
    
    # Dataset Size
    if (nrow(df) >= 1000) {
      
      cat("🚀 Dataset Size\n")
      cat("   Large dataset detected.\n")
      cat("   Recommendation: Tree-based models can be considered.\n\n")
      
    } else {
      
      cat("📊 Dataset Size\n")
      cat("   Moderate/small dataset detected.\n")
      cat("   Recommendation: Start with simpler ML models.\n\n")
      
    }
    # Correlation Analysis
    
    if (length(numeric_cols) >= 2) {
      
      corr <- cor(
        df[, numeric_cols, drop = FALSE],
        use = "complete.obs"
      )
      
      strong <- which(
        abs(corr) >= 0.8 & abs(corr) < 1,
        arr.ind = TRUE
      )
      
      if (nrow(strong) > 0) {
        
        pairs <- unique(
          t(apply(strong, 1, function(x)
            sort(x)
          ))
        )
        
        cat("🔗 Correlation Analysis\n")
        cat("------------------------------\n")
        
        for (i in 1:nrow(pairs)) {
          
          r <- pairs[i, 1]
          c <- pairs[i, 2]
          
          cat(
            "⚠️ Strong correlation: ",
            colnames(corr)[r],
            " ↔ ",
            colnames(corr)[c],
            " = ",
            round(corr[r, c], 2),
            "\n",
            sep = ""
          )
        }
        
        cat(
          "💡 Recommendation: Consider feature selection before ML training.\n\n"
        )
        
      } else {
        
        cat("✅ Correlation Analysis\n")
        cat("   No strong correlations detected.\n\n")
        
      }
      
    }
    
    cat("💡 Overall Recommendation\n")
    cat("------------------------------\n")
    cat("Clean and preprocess the dataset before Machine Learning.\n")
    cat("Use visualization and pattern analysis to understand the features.")
    
  })
  output$chatAnswer <- renderText({

  req(input$askAI)
  req(data())

  df <- data()

  question <- tolower(input$chatQuestion)

  rows <- nrow(df)
  cols <- ncol(df)
  missing <- sum(is.na(df))
  duplicates <- sum(duplicated(df))

  numeric_cols <- names(df)[sapply(df, is.numeric)]

  if (grepl("row|rows", question)) {

    paste(
      "🤖 Zynith AI:",
      "\nYour dataset contains",
      rows,
      "rows."
    )

  } else if (grepl("column|columns|feature|features", question)) {

    paste(
      "🤖 Zynith AI:",
      "\nYour dataset contains",
      cols,
      "columns."
    )

  } else if (grepl("missing|null|empty", question)) {

    if (missing == 0) {

      "🤖 Zynith AI:\n✅ No missing values detected."

    } else {

      paste(
        "🤖 Zynith AI:\n⚠️",
        missing,
        "missing values detected."
      )

    }

  } else if (grepl("duplicate|duplicates", question)) {

    paste(
      "🤖 Zynith AI:\n📋",
      duplicates,
      "duplicate row(s) detected."
    )

  } else if (grepl("numeric|number", question)) {

    paste(
      "🤖 Zynith AI:\n📊 Numeric columns:",
      paste(numeric_cols, collapse = ", ")
    )

  } else if (grepl("clean|quality|health", question)) {

    paste(
      "🤖 Zynith AI:\n",
      "Missing values:",
      missing,
      "\nDuplicate rows:",
      duplicates,
      "\nDataset health:",
      round(
        (1 - missing / (rows * cols)) * 100
      ),
      "%"
    )

  } else {

    paste(
      "🤖 Zynith AI:\n",
      "I can answer questions about rows, columns,",
      "missing values, duplicates, numeric features,",
      "and dataset quality."
    )

  }

})
  
  # ---------------- Pattern Detection ----------------
  
  output$patterns <- renderText({
    
    req(data())
    
    detect_patterns(data())
    
  })
  
  # ---------------- Dataset Health Score ----------------
  output$healthScore <- renderPrint({
    
    req(data())
    
    df <- if (is.null(cleanedData())) data() else cleanedData()
    
    missing <- sum(is.na(df))
    duplicate <- sum(duplicated(df))
    
    score <- round(
      100 -
        (missing / (nrow(df) * ncol(df))) * 50 -
        (duplicate / nrow(df)) * 50
    )
    
    score <- max(score, 0)
    
    cat(score, "/100")
    
  })
  
  
  output$rows <- renderText({
    req(data())
    nrow(data())
  })
  
  
  output$columns <- renderText({
    req(data())
    ncol(data())
  })
  
  
  output$missing <- renderText({
    req(data())
    sum(is.na(data()))
  })
  
  
  output$health <- renderText({
    req(data())
    
    total <- nrow(data()) * ncol(data())
    missing <- sum(is.na(data()))
    
    score <- round((1 - missing / total) * 100)
    
    paste0(score, "%")
  })
  
  
  # ---------------- Download Cleaned CSV ----------------
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      "Cleaned_Dataset.csv"
    },
    
    content = function(file) {
       
      req(cleanedData())
      
      write.csv(cleanedData(), file, row.names = FALSE)
      
    }
    
  )
  
  # ---------------- Download Report ----------------
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      "PatternSense_Report.txt"
    },
    
    content = function(file) {
      
      report <- c(
        "Zynith AI💫 Report",
        "=====================",
        paste("Rows:", nrow(data())),
        paste("Columns:", ncol(data())),
        paste("Missing Values:", sum(is.na(data()))),
        paste("Duplicate Rows:", sum(duplicated(data())))
      )
      
      writeLines(report, file)
      
    }
    
  )
  
}

shinyApp(ui = ui, server = server)