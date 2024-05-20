# Set Working Directory
setwd("c://users/ndavi/onedrive/ondeche/hivos")


# Install Packages
#install.packages(c("readxl", "dplyr", "shiny", "shinydashboard", "DT"))


# Load Packages
library(readxl)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(reshape2)


# Import Workbooks
filename <- c("Green Entrepreneurship.xlsx", "Expression & Engagement.xlsx", "Sexual Rights & Diversity.xlsx")
wbs <- c()
for (i in seq(1, length(filename))) {
  sheetnames <- excel_sheets(filename[i])
  wb <- lapply(sheetnames, read_excel, path = filename[i])
  names(wb) <- sheetnames
  wb <- data.frame(bind_rows(wb))
  wbs <- bind_rows(wbs, wb)
}


# Data Transformation
wbs$Contr..date <- as.Date(pull(wbs["Contr..date"]), "%y-%m-%d")
wbs$Ass..datedoc <- as.Date(pull(wbs["Ass..datedoc"]), "%y-%m-%d")
wbs$Project.Goal <- as.character(wbs$Project.Goal)
wbs$Indicators <- as.character(wbs$Indicators)
wbs$Outcomes <- as.character(wbs$Outcomes)
wbs$Lesson.Best.Practice <- as.character(wbs$Lesson.Best.Practice)


# Import Dataset
abp <- read.csv("ABP summary.csv")
names(abp) <- c("Country", seq(2014,2017))

abp_g <- gather(abp, key="Year", value="Achievement", -"Country")
abp_g$Year <- as.factor(abp_g$Year)
head(abp_g)


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
                                 choices = pull(unique(wbs["Thematic.Areas"])),
                                 selected = "Green Entrepreneurship")),
          
          box(uiOutput(outputId="result"))),
        
        fluidRow(dataTableOutput(outputId="table"))),
      
      
      tabItem(tabName="widgets",
        h2("Widgets tab content"),
        fluidRow(
          box(selectInput(inputId = "country",
                           label = "Country",
                           choices = pull(unique(abp_g["Country"])),
                           selected = "Kenya",
                           multiple = FALSE)),
          
          box(uiOutput(outputId="year"))),
        
        fluidRow(
          box(plotOutput(outputId="bar")),
          
          box(plotOutput(outputId="bar2"))
        )))
  )
)


server <- function(input, output) {
  ra <- reactive({
    wbs %>%
      filter(Thematic.Areas == input$thematic) %>%
      select(Result.Areas) %>%
      unique() %>%
      pull()
  })
  
  
  output$result <- renderUI({
    selectInput(inputId = "result",
                label = "Result Areas",
                choices = ra(),
                selected = "Cap. dev. for improved economic position entrepreneurs",
                multiple = FALSE)
  })
  
  
  output$table <- renderDataTable({
    wbs %>%
      filter(Thematic.Areas == input$thematic) %>%
      filter(Result.Areas == input$result)
  })
  
  
  y <- reactive({
    abp_g %>%
      filter(Country == input$country) %>%
      select(Year) %>%
      unique() %>%
      pull()
  })
  
  
  output$year <- renderUI({
    selectInput(inputId = "year",
                label = "Year",
                choices = y(),
                selected = 2015,
                multiple = FALSE)
  })
  
  
  country <- reactive({
    abp_g %>%
      filter(Country == input$country)
  })
  
  
  output$bar <- renderPlot({
    ggplot(data=country(), aes(x=Year, y=Achievement)) +
      geom_col()
  })
  
  
  year <- reactive({
    abp_g %>%
      filter(Year == input$year) %>%
      filter(Country != "ABPP RESULTS")
  })
  
  
  output$bar2 <- renderPlot({
    ggplot(data=year(), aes(x=Country, y=Achievement)) +
      geom_col()
  })
}


# Initialize App
shinyApp(ui, server)

#write.csv(abp_g, "abp_g.csv")
