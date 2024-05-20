# Set Working Directory

setwd("c://users/ndavi/onedrive/ondeche/hivos")


# Load Packages
library(RODBC)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)


# Show Package Documentation
# RShowDoc("RODBC", package = "RODBC")


# Define Functions
char_clean <- function(tbl) {
  names(tbl) <- col_names
  
  # Data Transformation
  tbl[c(6, 7)] = NULL
  char <- col_names[-c(4, 5, 6, 7)]
  for (i in seq(1, length(char))) {
    tbl[char[i]] = as.character(pull(tbl[char[i]]))
  }
  return(tbl)
}


date_clean <- function(tbl){
  # Data Transformation
  d <- col_names[c(4, 5)]
  for (j in seq(1, length(d))) {
    tbl[d[j]] = as.Date(pull(tbl[d[j]]), "%y-%m-%d")
  }
  return(tbl)
}


# Connect to Database
c1 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=c:/users/ndavi/onedrive/ondeche/hivos/app.accdb")


# Column Names
col_names <- c("thematicArea", "resultArea", "assDocNo", "contrDate", "assDateDoc", "thematicID", "resultID", "projectName", "grantee", "grantAmount", "partnerName", "projectGoal", "indicators", "outcomes", "lessonBestPractice")


# Disconnect from Database
# odbcCloseAll()
#dbDisconnect(c2)


# Create App
ui <- dashboardPage(
  dashboardHeader(title = "HIVOS DASHBOARD"),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard")),
      
      menuItem("Widgets", tabName="widgets", icon=icon("th")))
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
        fluidRow(
          box(radioButtons(inputId = "thematic",
                                 label = "Thematic Areas",
                                 choices = pull(sqlQuery(c1, "SELECT ThematicArea FROM Thematic_Areas_Table")),
                                 selected = NULL)),
          
          box(uiOutput(outputId="result"))),
        
        fluidRow(dataTableOutput(outputId="table"))),
      
      
      tabItem(tabName="widgets",
        h2("Widgets tab content"),
        fluidRow(
          box(selectInput(inputId = "country",
                          label = "Country",
                          choices = sqlQuery(c1, "SELECT Country FROM ABPP_Country"),
                          selected = "Kenya",
                          multiple = FALSE), width=4),
          
          box(uiOutput(outputId="year"), width=4),
          
          box(selectInput(inputId = "gender",
                          label = "Gender",
                          choices = sqlQuery(c1, "SELECT Gender FROM Gender"),
                          selected = "Kenya",
                          multiple = FALSE), width=4)),
        
        fluidRow(
          box(plotOutput(outputId="bar"), width=4),
          
          box(plotOutput(outputId="bar2"), width=4),
          
          box(plotOutput(outputId="bar3"), width=4)
        )))
  )
)



server <- function(input, output) {
  ra <- reactive({
    sqlQuery(c1, paste0("SELECT ResultArea FROM Result_Areas_Table WHERE ThematicID = (SELECT ThematicID FROM Thematic_Areas_Table WHERE ThematicArea = '", input$thematic, "')"))
  })
  
  
  output$result <- renderUI({
    selectInput(inputId = "result",
                label = "Result Areas",
                choices = ra(),
                selected = "Cap. dev. for improved economic position entrepreneurs",
                multiple = FALSE)
  })
  
  ta <- reactive({
    # Retrieve Database Table Names
    tab_names <- sqlTables(c1, tableType = "TABLE")$TABLE_NAME
    tab_loop <- tab_names[-c(2, 3, 7, 8, 9, 14, 18)]
    
   
    # Import Database Tables
    tbls_df <- tab_loop %>%
      lapply(function(t) sqlQuery(c1, paste0("SELECT t4.ThematicArea, t4.ResultArea, t1.* FROM ", t, " AS t1 INNER JOIN (SELECT * FROM Result_Areas_Table AS t2 INNER JOIN Thematic_Areas_Table AS t3 ON t2.ThematicID = t3.ThematicID WHERE t3.ThematicID = (SELECT ThematicID FROM Thematic_Areas_Table WHERE ThematicArea = '", input$thematic, "')) AS t4 ON t1.ResultID = t4.ResultID WHERE t4.ResultID = (SELECT ResultID FROM Result_Areas_Table WHERE ResultArea = '", input$result, "')"))) %>%
      lapply(char_clean) %>%
      lapply(date_clean) %>%
      bind_rows()
  })
  
  
  output$table <- renderDataTable({
    ta()
  })
  
  
  y <- reactive({
    sqlQuery(c1, paste0("SELECT Year FROM ABPP_Summary WHERE CountryID = (SELECT CountryID FROM ABPP_Country WHERE Country = '", input$country, "')"))
  })
  
  
  output$year <- renderUI({
    selectInput(inputId = "year",
                label = "Year",
                choices = y(),
                selected = 2015,
                multiple = FALSE)
  })
  
  
  country <- reactive({
    sqlQuery(c1, paste0("SELECT Year, Achievement FROM ABPP_Summary WHERE CountryID = (SELECT CountryID FROM ABPP_Country WHERE Country = '", input$country, "')"))
  })
  
  
  output$bar <- renderPlot({
    ggplot(data=country(), aes(x=Year, y=Achievement)) +
      geom_col() +
      ggtitle("ABPP Achievement Per Country")
  })
  
  
  year <- reactive({
    sqlQuery(c1, paste0("SELECT t2.Country, t1.Achievement FROM ABPP_Summary AS t1 LEFT OUTER JOIN ABPP_Country AS t2 ON t1.CountryID = t2.CountryID WHERE Year = ", input$year, "AND Country <> 'ABPP RESULTS'"))
  })
  
  
  output$bar2 <- renderPlot({
    ggplot(data=year(), aes(x=Country, y=Achievement)) +
      geom_col() +
      ggtitle("ABPP Achievemnet Per Year")
  })
  
  
  activities <- reactive({
    sqlQuery(c1, paste0("SELECT t3.Activity, t4.Gender, t3.Count FROM (SELECT t2.Activity, t1.GenderID, t1.Count FROM EE_Events AS t1 INNER JOIN EE_Activity AS t2 ON t1.ActivityID = t2.ActivityID) AS t3 INNER JOIN Gender AS t4 ON t3.GenderID = t4.GenderID WHERE t4.Gender = '", input$gender, "'"))
  })
  
  
  output$bar3 <- renderPlot({
    ggplot(data=activities(), aes(x=Activity, y=Count)) +
      geom_col(position="dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Journalism Activities")
  })
}


# Initialize App
shinyApp(ui, server)
