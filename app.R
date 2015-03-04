## app.R ##
library(shiny)
library(shinydashboard)

library(dplyr)
library(ggvis)

# Read data

data <- read.csv("drug_quant.csv",header=TRUE)
data <- as.tbl(data)
data$Date <- as.Date(as.character(data$Date),format ="%Y-%m")

drugs <- as.vector((unique(data$BNF.Description)))
drugs <- c("None",drugs)

# Define UI

ui <- dashboardPage(
    dashboardHeader(title = "Prescriptions"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
            menuItem("Plots", tabName = "plots", icon = icon("line-chart")),
            menuItem("Data Table", tabName = "raw_data", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        #Boxes need to be put in a row (or column)
         tabItems(

            # Second tab item
            tabItem(tabName = "instructions",
                h2("Instructions for use"),
                p("Here is a paragraph explaining what this is about.")

            ),

            # Plots
            tabItem(tabName = "plots",
                fluidRow(
                    titlePanel("Presciptions Plot"),
                    box(p("Please select up to four drugs to plot the usage of."),
                        width = 12),
                    box(selectInput("drug1", "Drug 1", choices = drugs),
                                     width = 3),
                    box(selectInput("drug2", "Drug 2", choices = drugs),
                                     width = 3),
                    box(selectInput("drug3", "Drug 3", choices = drugs),
                                     width = 3),
                    box(selectInput("drug4", "Drug 4", choices = drugs),
                                     width = 3),
                    box(ggvisOutput("ggvis_plot"), width = 12)
                )
            ),

            # Data table
            tabItem(tabName = "raw_data",
                fluidRow(
                    titlePanel("Presciptions Data Table"),
                    box(dataTableOutput("table"), width = 8),
                    box(p("Here is a paragraph explaining the table."),
                        width = 3
                        )
                )
            )

        )
    )
)

# Do processing

server <- function(input, output) {
    
   # Render drug selectors
#    output$drug1selector <- renderUI({
#    # Select drugs to plot with drop down menu
#    # Add names, so that we can add all=0
#  #   names(drugs) <- drugs
#  #  drugs <- c(None = 0, drugs)
#    selectizeInput("drug1", "Drug 1", choices = drugs, selected = drugs[1])
#    })

    # Process plot
    
    vis <- reactive({
                data %>%
                filter(BNF.Description == input$drug1 |
                       BNF.Description == input$drug2 |
                       BNF.Description == input$drug3 |
                       BNF.Description == input$drug4) %>%
                ggvis(~Date, ~Volume, stroke = ~factor(BNF.Description)) %>%
                group_by(BNF.Description) %>%
                layer_paths() %>%
                add_axis("x",
                         properties=axis_props(labels=list(angle=45))) %>%
                add_legend("stroke", title = "Drug")
                })
    vis %>%  bind_shiny("ggvis_plot")

    # Render data table
    output$table <- renderDataTable(data,options = list(pageLength = 5))
}

shinyApp(ui, server)