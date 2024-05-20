# Set Working Directory

setwd("c://users/ndavi/onedrive/ondeche/hivos")

# Load Packages
library(RODBC)
library(dplyr)
library(tidyr)
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
  char <- col_names[-c(2, 5, 6, 7, 12)]
  for (i in seq(1, length(char))) {
    tbl[char[i]] = as.character(tbl[char[i]])
  }
  return(tbl)
}


date_clean <- function(tbl){
  # Data Transformation
  d <- col_names[c(2, 5, 6, 12)]
  for (j in seq(1, length(d))) {
    tbl[d[j]] = as.Date(tbl[d[j]], "%y-%m-%d")
  }
  return(tbl)
}


# Connect to Database
c1 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=c:/users/ndavi/onedrive/ondeche/hivos/app3.accdb")


# Table Names
tab_names <- sqlTables(c1, tableType = "TABLE")$TABLE_NAME


# Column Names
col_names_01 <- c("documentNo", "accountDate", "description", "bPartner", "from", "to", "status", "currency", "totalFund", "Region" , "country")
col_names_02 <- c("documentNo", "orgUnit", "assDocNo", "assDateDoc", "criteriumName", "score", "indicatorsOutcomes")


# Import Database Tables
tbls_df <- sqlQuery(c1, paste0("SELECT DISTINCT t1.documentNO, t1.totalFundingAmount AS totalFund, t6.orgUnit FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNo = t6.documentNO ORDER BY totalFundingAmount DESC")) %>%
  arrange(desc(totalFund))

# EDA
class(tbls_df)
length(tbls_df)
names(tbls_df)
tbls_df[1:5, ]

# Plot
ggplot(data=tbls_df, aes(x=orgUnit, y=totalFund)) +
  geom_col(fill="maroon") +
  xlab("Org Unit") +
  ylab("Total Fund") +
  ggtitle("Total Funding Per Org Unit")

# write.csv(tbls_df, "bp_funding.csv", row.names=FALSE)


# # Disconnect from Database
# odbcCloseAll()
# dbDisconnect(c2)


# Create App
ui <- dashboardPage(
  dashboardHeader(title = "HIVOS DASHBOARD"),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Total Funding Amount", tabName="dashboard", icon=icon("dashboard")),
      
      menuItem("Indicators and Outcomes", tabName="indicatorsOutcomes", icon=icon("th")),
      
      menuItem("Widgets", tabName="widgets", icon=icon("th")))
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
        h2("Total Funding Amount"),
        fluidRow(
          box(radioButtons(inputId = "thematic",
                           label = "Thematic Areas",
                           choices = pull(sqlQuery(c1, "SELECT ThematicArea FROM ThematicAreas")),
                           selected = NULL)),
          
          box(uiOutput(outputId="result"))),
        
        fluidRow(dataTableOutput(outputId="table"))),
      
      
      tabItem(tabName="indicatorsOutcomes",
        h2("Indicators and Outcomes"),
        fluidRow(
          box(
            h4("Thematic Area"),
            h5(textOutput(outputId="thematic_area"))),
          
          box(
            h4("Result Area"),
            h5(textOutput(outputId="result_area")))
        ),
        
        fluidRow(dataTableOutput(outputId="table_io"))),
      
      
      tabItem(tabName="widgets",
        h2("Widgets"),
        fluidRow(
          box(plotOutput(outputId="bar"), width=3),
          
          box(plotOutput(outputId="bar2"), width=6),
          
          box(plotOutput(outputId="bar3"), width=3)),
        
        fluidRow(
          box(plotOutput(outputId="bar4"), width=4),
          
          box(plotOutput(outputId="bar5"), width=8)),
        
        fluidRow(
          box(plotOutput(outputId="bar6"), width=12)
        )
      )
    )
  )
)



server <- function(input, output) {
  ra <- reactive({
    sqlQuery(c1, paste0("SELECT t1.ResultArea FROM ResultAreas AS t1 INNER JOIN ThematicAreas AS t2 ON t1.ThematicID = t2.ThematicID WHERE t2.ThematicArea = '", input$thematic, "'"))
  })

  output$result <- renderUI({
    selectInput(inputId = "result",
                label = "Result Areas",
                choices = ra(),
                selected = "Cap. dev. for improved economic position entrepreneurs",
                multiple = FALSE)
  })

  
  ta <- reactive({
    # Import Database Tables
    tbls_df_01 <- sqlQuery(c1, paste0("SELECT DISTINCT t1.documentNo, t1.accountDate, t1.description, t1.businessPartner, t1.dateFrom, t1.finalDateTo, t1.docStatus, t1.Currency, t1.totalFundingAmount, t1.Region, t1.Country FROM AmountTable AS t1 INNER JOIN (SELECT t2.* FROM ResultAreaTable AS t2 INNER JOIN ResultAreas AS t3 ON t2.resultID = t3.ResultID WHERE t3.ResultArea = '", input$result, "') AS t4 ON t1.documentNO = t4.documentNO"))
    names(tbls_df_01) <- col_names_01
    tbls_df_01
  })

  output$table <- renderDataTable({
    ta()
  })
  
  
  output$thematic_area <- renderText({
    input$thematic
  })
  
  
  output$result_area <- renderText({
    input$result
  })
  
  
  io <- reactive({
    # Import Database Tables
    tbls_df_02 <- sqlQuery(c1, paste0("SELECT t1.documentNo, t6.orgUnit, t6.assDocNO, t6.assDateDoc, t6.criteriumName, t6.Score, t6.longDescription FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID WHERE t4.ThematicArea = '", input$thematic, "') AS t5 ON t2.resultID = t5.ResultID WHERE t5.ResultArea = '", input$result, "') AS t6 ON t1.documentNO = t6.documentNO"))
    names(tbls_df_02) <- col_names_02
    tbls_df_02
  })
  
  output$table_io <- renderDataTable({
    io()
  })
  
  
  output$bar <- renderPlot({
    rf <- sqlQuery(c1, paste0("SELECT t1.totalFundingAmount AS totalFund, t1.Region FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNO = t6.documentNO"))
    ggplot(data=rf, aes(x=Region, y=totalFund)) +
      geom_col(aes(fill=Region)) +
      guides(fill=FALSE) +
      ylab("Total Fund") +
      ggtitle("Total Funding Per Region") +
      scale_fill_brewer(type="qual", palette="Blues", direction=-1, aesthetics="fill")
  })
  
  
  output$bar2 <- renderPlot({
    cf <- sqlQuery(c1, paste0("SELECT t1.totalFundingAmount AS totalFund, t1.Country, t1.Region FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNO = t6.documentNO WHERE t1.Country IN ('Kenya', 'Uganda', 'Tanzania, United Republic Of')"))
    ggplot(data=cf, aes(x=Country, y=totalFund)) +
      geom_col(aes(fill=Country)) +
      ylab("Total Fund") +
      coord_flip() +
      ggtitle("Total Funding Per Country") +
      scale_fill_brewer(type="qual", palette="Accent", direction=-1, aesthetics="fill")
  })
  
  
  output$bar3 <- renderPlot({
    yf <- sqlQuery(c1, paste0("SELECT DISTINCT t1.documentNO, t1.totalFundingAmount AS totalFund, t1.accountDate FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNo = t6.documentNO")) %>%
      separate(col=accountDate, into=c("year", "_"), sep=4)
    
    ggplot(data=yf, aes(x=year, y=totalFund)) +
      geom_col(fill="maroon") +
      xlab("Year") +
      ylab("Total Fund") +
      ggtitle("Total Funding Per Year")
  })
  
  
  output$bar4 <- renderPlot({
    tf <- sqlQuery(c1, paste0("SELECT t1.totalFundingAmount AS totalFund, t6.ThematicArea FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNO = t6.documentNO"))
    ggplot(data=tf, aes(x=ThematicArea, y=totalFund)) +
      geom_col(aes(fill=ThematicArea)) +
      guides(fill=FALSE) +
      xlab("Thematic Area") +
      ylab("Total Fund") +
      ggtitle("Total Funding Per Thematic Area")
  })
  
  
  output$bar5 <- renderPlot({
    ref <- sqlQuery(c1, paste0("SELECT t1.totalFundingAmount AS totalFund, t6.ResultArea, t6.ThematicArea FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNO = t6.documentNO"))
    ggplot(data=ref, aes(x=ResultArea, y=totalFund)) +
      geom_col(aes(fill=ThematicArea)) +
      xlab("Result Area") +
      ylab("Total Fund") +
      coord_flip() +
      # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Total Funding Per Result Area")
  })
  
  
  output$bar6 <- renderPlot({
    bp <- sqlQuery(c1, paste0("SELECT DISTINCT t1.documentNO, t1.totalFundingAmount AS totalFund, t1.businessPartner FROM AmountTable AS t1 INNER JOIN (SELECT t2.*, t5.* FROM ResultAreaTable AS t2 INNER JOIN (SELECT t3.*, t4.* FROM ResultAreas AS t3 INNER JOIN ThematicAreas AS t4 ON t3.ThematicID = t4.ThematicID) AS t5 ON t2.resultID = t5.ResultID) AS t6 ON t1.documentNo = t6.documentNO ORDER BY totalFundingAmount DESC")) %>%
      arrange(desc(totalFund))
    bp <- bp[1:10, ]  

    ggplot(data=bp, aes(x=businessPartner, y=totalFund)) +
      geom_col(fill="maroon") +
      xlab("Business Partner") +
      ylab("Total Fund") +
      coord_flip() +
      ggtitle("Total Funding Per Business Partner (Top 10)")
  })
}


# Initialize App
shinyApp(ui, server)
