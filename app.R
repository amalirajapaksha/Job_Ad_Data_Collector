# --- START OF app.R ---

library(shiny)
library(tidyverse)
library(DT)
library(googlesheets4)

# --- CONFIGURATION & DATA SETUP ---

# 1. GOOGLE SHEET ID
sheet_id <- "1Uto4VFyZSJ-gzCGpY-MY46nLbNWkYZXZjobDZItQzb4"

# 2. SERVICE ACCOUNT AUTHENTICATION (WORKS ON shinyapps.io)
SERVICE_ACCOUNT_FILE <- "job-ad-shiny-app-172156ca0df8.json"

# ✅ Authenticate using the JSON key file
suppressMessages({
  gs4_auth(path = SERVICE_ACCOUNT_FILE)
})

# 3. DATA COLUMN DEFINITIONS
data_cols <- c(
  "ID", "Consultant", "DateRetrieved", "DatePublished", "Job_title", "Company",
  "R", "SAS", "SPSS", "Python", "MAtlab", "Scala", "C#", "MS Word", "Ms Excel",
  "OLE/DB", "Ms Access", "Ms PowerPoint", "Spreadsheets", "Data_visualization",
  "Presentation_Skills", "Communication", "BigData", "Data_warehouse",
  "cloud_storage", "Google_Cloud", "AWS", "Machine_Learning", "Deep Learning",
  "Computer_vision", "Java", "C++", "C", "Linux/Unix", "SQL", "NoSQL", "RDBMS",
  "Oracle", "MySQL", "PHP", "SPL", "web_design_and_development_tools", "AI",
  "Natural_Language_Processing(NLP)", "Microsoft Power BI", "Google_Analytics",
  "graphics_and_design_skills", "Data_marketing", "SEO", "Content_Management",
  "Tableau", "D3", "Alteryx", "KNIME", "Spotfire", "Spark", "S3", "Redshift",
  "DigitalOcean", "Javascript", "Kafka", "Storm", "Bash", "Hadoop",
  "Data_Pipelines", "MPP_Platforms", "Qlik", "Pig", "Hive", "Tensorflow",
  "Map/Reduce", "Impala", "Solr", "Teradata", "MongoDB", "Elasticsearch", "YOLO",
  "agile execution", "Data_management", "pyspark", "Data_mining", "Data_science",
  "Web_Analytic_tools", "IOT", "Numerical_Analysis", "Economic",
  "Finance_Knowledge", "Investment_Knowledge", "Problem_Solving", "Team_Handling",
  "Debtor_reconcilation", "Payroll_management", "Bayesian", "Optimization",
  "Knowledge_in", "City", "Educational_qualifications", "Salary", "URL",
  "Search_Term", "Job_Category", "Experience_Category", "Location", "Mode",
  "Payment Frequency", "BSc_needed", "MSc_needed", "PhD_needed", "English_needed",
  "year"
)

skill_cols <- data_cols[7:94]
binary_qual_cols <- c("BSc_needed", "MSc_needed", "PhD_needed", "English_needed")

programming_langs <- c("R", "SAS", "SPSS", "Python", "MAtlab", "Scala", "C#", "Java", "C++", "C", "Javascript")
data_ml_concepts <- c("BigData", "Machine_Learning", "Deep Learning",
                      "Natural_Language_Processing(NLP)", "AI", "Data_science",
                      "IOT", "Numerical_Analysis", "Bayesian", "Optimization")
databases_tools_cloud <- c("SQL", "NoSQL", "RDBMS", "Oracle", "MySQL", "MongoDB", "Elasticsearch",
                           "Google_Cloud", "AWS", "S3", "Redshift", "DigitalOcean", "Spark",
                           "Hadoop", "Hive", "Tensorflow", "Microsoft Power BI", "Tableau", "D3")

other_binary_skills <- setdiff(skill_cols, c(programming_langs, data_ml_concepts, databases_tools_cloud))

# ----------------------------
# Function: load_data
# ----------------------------
load_data <- function(sheet_id, col_names) {
  suppressMessages({
    data <- read_sheet(sheet_id, col_types = "c")
  })
  missing_cols <- setdiff(col_names, names(data))
  for (col in missing_cols) data[[col]] <- NA_character_
  data <- data %>% select(all_of(col_names))
  data$ID <- suppressWarnings(as.numeric(data$ID))
  return(data)
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(tags$title("Data-Related Job Ad Entry")),
  titlePanel(span(icon("chart-line"), "Job Ad Data Collector")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Core Job Ad Info"),
      selectInput("consultant_name", "Consultant:", 
                  choices = c("Y.M.A.P.Rajapaksha", "S.G.V.S.Samaranayaka", "I.V.N.N.Wijesekara", "G.R.Jayasinghe", "M.N.D.Gunarathna", "N.G.S.Minimuthu", "L.P.U.D.N. Vasana", "Y.G.D.M.S.Thathsarani", "P.A.H.N.Dharmasena", "G.K.L.L.S.Premathilaka", "N. L. N. Kumarage", "H.L.D.H. Sandamini"), selected = "Y.M.A.P.Rajapaksha"),
      
      textInput("date_retrieved", "Date Retrieved:", value = ""),
      textInput("date_published", "Date Published:", value = ""),
      textInput("year", "Year:", value = ""),
      
      hr(),
      textInput("job_category", "Job Category:", value = ""),
      textInput("experience_category", "Experience Category:", value = ""), 
      
      hr(),
      actionButton("submit_button", "Submit New Entry", class = "btn-success", icon = icon("upload")),
      br(), br(),
      downloadButton("download_data", "Download Full Data (.csv)")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("General Details", icon = icon("info-circle"),
                 textInput("job_title", "Job Title:"),
                 textInput("company_name", "Company:"),
                 textInput("url", "URL:"),
                 textInput("search_term", "Search Term:"),
                 textInput("city", "City:"),
                 textInput("location", "Location:"),
                 textInput("mode", "Mode:"),
                 textInput("educational_qualifications", "Educational Qualifications:"),
                 textInput("salary", "Salary (text allowed):", value = ""),
                 textInput("payment_frequency", "Payment Frequency:", value = ""),
                 textInput("knowledge_in", "Knowledge In:")
        ),
        
        tabPanel("Programming Languages", icon = icon("code"),
                 p("**Check all programming languages mentioned in the job ad (1/0):**"),
                 fluidRow(lapply(programming_langs, function(col) column(3, checkboxInput(col, col, value = FALSE))))
        ),
        
        tabPanel("Data & ML Concepts", icon = icon("brain"),
                 p("**Check all Data Science/ML concepts/skills mentioned (1/0):**"),
                 fluidRow(lapply(data_ml_concepts, function(col) column(3, checkboxInput(col, col, value = FALSE))))
        ),
        
        tabPanel("Tools, DB & Cloud", icon = icon("database"),
                 p("**Check all Databases, Tools, and Cloud Platforms mentioned (1/0):**"),
                 fluidRow(lapply(databases_tools_cloud, function(col) column(3, checkboxInput(col, col, value = FALSE))))
        ),
        
        tabPanel("Other Skills & Quals", icon = icon("list-check"),
                 p("**Check any other skills/qualifications mentioned (1/0):**"),
                 fluidRow(
                   column(3, checkboxInput("BSc_needed", "BSc Needed", value = FALSE)),
                   column(3, checkboxInput("MSc_needed", "MSc Needed", value = FALSE)),
                   column(3, checkboxInput("PhD_needed", "PhD Needed", value = FALSE)),
                   column(3, checkboxInput("English_needed", "English Needed", value = FALSE))
                 ),
                 hr(),
                 fluidRow(lapply(other_binary_skills, function(col) column(3, checkboxInput(col, col, value = FALSE))))
        ),
        
        tabPanel("View All Data", icon = icon("table"),
                 h3("Currently Collected Data (Refreshes on New Entry)"),
                 DTOutput("data_table"))
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  data_storage <- reactiveVal({ load_data(sheet_id, data_cols) })
  
  observeEvent(input$submit_button, {
    latest_data <- load_data(sheet_id, data_cols)
    new_id <- max(latest_data$ID, 0, na.rm = TRUE) + 1
    
    new_row <- as_tibble(setNames(rep(list(NA_character_), length(data_cols)), data_cols))
    new_row$ID <- as.character(new_id)
    new_row$year <- input$year
    
    # ----- Text inputs -----
    new_row$Consultant <- input$consultant_name
    new_row$DateRetrieved <- input$date_retrieved
    new_row$DatePublished <- input$date_published
    new_row$Job_title <- input$job_title
    new_row$Company <- input$company_name
    new_row$URL <- input$url
    new_row$Search_Term <- input$search_term
    new_row$City <- input$city
    new_row$Location <- input$location
    new_row$Mode <- input$mode
    new_row$Educational_qualifications <- input$educational_qualifications
    new_row$Salary <- input$salary
    new_row$`Payment Frequency` <- input$payment_frequency
    new_row$Knowledge_in <- input$knowledge_in
    new_row$Job_Category <- input$job_category
    new_row$Experience_Category <- input$experience_category
    
    # ----- Checkbox inputs -----
    for (col in data_cols) {
      if (is.null(new_row[[col]]) || is.na(new_row[[col]])) {
        val <- input[[col]]
        new_row[[col]] <- if (isTRUE(val)) "1"
        else if (isFALSE(val)) "0"
        else if (!is.null(val)) as.character(val)
        else NA_character_
      }
    }
    
    suppressMessages(sheet_append(sheet_id, new_row))
    data_storage(load_data(sheet_id, data_cols))
    showNotification("✅ Data saved to Google Sheets", type = "message", duration = 3)
    # --- Clear all text and selection inputs after saving ---
    updateTextInput(session, "date_retrieved", value = "")
    updateTextInput(session, "date_published", value = "")
    updateTextInput(session, "year", value = "")
    updateTextInput(session, "job_title", value = "")
    updateTextInput(session, "company_name", value = "")
    updateTextInput(session, "url", value = "")
    updateTextInput(session, "search_term", value = "")
    updateTextInput(session, "city", value = "")
    updateTextInput(session, "location", value = "")
    updateTextInput(session, "mode", value = "")
    updateTextInput(session, "educational_qualifications", value = "")
    updateTextInput(session, "salary", value = "")
    updateTextInput(session, "payment_frequency", value = "")
    updateTextInput(session, "knowledge_in", value = "")
    updateTextInput(session, "job_category", value = "")
    updateTextInput(session, "experience_category", value = "")
    
    # --- Reset all checkboxes to FALSE ---
    for (col in c(skill_cols, binary_qual_cols, other_binary_skills)) {
      updateCheckboxInput(session, col, value = FALSE)
    }
  })
  
  output$data_table <- renderDT({
    df <- data_storage()
    df %>% arrange(desc(ID))
  }, options = list(pageLength = 10))
  
  output$download_data <- downloadHandler(
    filename = function() paste0("job_ad_data_export-", Sys.Date(), ".csv"),
    content = function(file) write_csv(data_storage(), file, na = "")
  )
}

# ----------------------------
# Run App
# ----------------------------
shinyApp(ui = ui, server = server)


