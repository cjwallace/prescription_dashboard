## app.R ##
library(shiny)
library(shinydashboard)

library(dplyr)
library(ggvis)

# Read data

data <- read.csv("drug_quant.csv",header=TRUE)
data <- as.tbl(data)
data$Date <- as.Date(paste(data$Date,"-01",sep=""))

drugs <- as.vector((unique(data$BNF.Description)))
drugs <- c("None",drugs)

prac_data <- read.csv("practice_summary.csv",header=TRUE)

# Define UI

ui <- dashboardPage(
    dashboardHeader(title = "Prescriptions"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
            menuItem("Drug Plots", tabName = "plots", icon = icon("line-chart")),
            menuItem("Data Table", tabName = "raw_data", icon = icon("th")),
            menuItem("Practice Summary", tabName = "practices", icon = icon("user-md"))
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
                    titlePanel("Prescriptions Plot"),
                    box(p("Please select up to four drugs to plot."),
                        width = 12),
                    box(selectInput("drug1", "Drug 1", choices = drugs),
                                     width = 3),
                    box(selectInput("drug2", "Drug 2", choices = drugs),
                                     width = 3),
                    box(selectInput("drug3", "Drug 3", choices = drugs),
                                     width = 3),
                    box(selectInput("drug4", "Drug 4", choices = drugs),
                                     width = 3),
                    box(ggvisOutput("ggvis_plot"), width = 12),
                    box(ggvisOutput("ggvis_plot_2"), width = 12)
                )
            ),

            # Data table
            tabItem(tabName = "raw_data",
                fluidRow(
                    titlePanel("Prescriptions Data Table"),
                    box(dataTableOutput("table"), width = 8),
                    box(p(paste("The raw data for the plots is contained in the searchable",
                                "table to the right. Volume is the total number of the item",
                                "prescribed per within the indicated month.",
                                "The Average Cost is the sum of the actual costs of all units",
                                "of the item prescribed divided by Volume, in pounds sterling",
                                "and fractions thereof.")),
                        width = 4
                    )
                )
            ),

            # Practice graph
            tabItem(tabName = "practices",
                fluidRow(
                    titlePanel("Practice Summary Plot"),
                    box(ggvisOutput("practices_plot"), width = 9),
                    box(p(paste("Here is a plot of all practices (nationally)",
                                "showing their total prescription volume (number of",
                                "items) vs. the total cost of those items in",
                                "December 2014.")),
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
    
    vis1 <- reactive({
                data %>%
                filter(BNF.Description == input$drug1 |
                       BNF.Description == input$drug2 |
                       BNF.Description == input$drug3 |
                       BNF.Description == input$drug4) %>%
                ggvis(~Date, ~Volume, stroke = ~factor(BNF.Description)) %>%
                group_by(BNF.Description) %>%
                layer_paths() %>%
                add_axis("x",
                         properties=axis_props(title=list(fontSize = 18)),
                         title = "Date",
                         title_offset = 50) %>%
                add_axis("y",
                         properties=axis_props(title=list(fontSize = 18)),
                         title = "Volume",
                         title_offset = 75) %>%
                add_legend("stroke", title = "Drug",
                           properties=legend_props(title=list(fontSize = 18)),
                          )
                })
    vis1 %>%  bind_shiny("ggvis_plot")

    vis2 <- reactive({
                data %>%
                filter(BNF.Description == input$drug1 |
                       BNF.Description == input$drug2 |
                       BNF.Description == input$drug3 |
                       BNF.Description == input$drug4) %>%
                ggvis(~Date, ~Average.Cost, stroke = ~factor(BNF.Description)) %>%
                group_by(BNF.Description) %>%
                layer_paths() %>%
                add_axis("x",
                         properties=axis_props(title=list(fontSize = 18)),
                         title = "Date",
                         title_offset = 50) %>%
                add_axis("y",
                         properties=axis_props(title=list(fontSize = 18)),
                         title = "Average Cost",
                         title_offset = 75) %>%
                add_legend("stroke", title = "Drug",
                           properties=legend_props(title=list(fontSize = 18)),
                          )
                })
    vis2 %>%  bind_shiny("ggvis_plot_2")

    # Render data table
    output$table <- renderDataTable(data,
                                    options = list(pageLength = 5,
                                                   lengthMenu = c(5,10,25,50),
                                                   colnames = c("Index",
                                                                "BNF Description",
                                                                "Date",
                                                                "Volume",
                                                                "Average Cost")))

    vis3 <- reactive({
                # Helper function
                pcode <- function(x) {
                  if(is.null(x)) return(NULL)
                  row <- prac_data[prac_data$ID == x$ID, ]
                  row$Practice.Code
                  #paste0(names(row), ": ", format(row), collapse = "<br />")
                }
               
                prac_data %>%
                    ggvis(~Volume,~Cost) %>%
                    layer_points(opacity := 0.45, fill = ~factor(Cluster), key := ~ID) %>%
                    add_tooltip(pcode,"hover") %>%
                    layer_model_predictions(model = "lm") %>%
                    add_axis("x",
                             properties=axis_props(title=list(fontSize = 18)),
                             title = "Volume",
                             title_offset = 50) %>%
                    add_axis("y",
                             properties=axis_props(title=list(fontSize = 18)),                      
                             title = "Cost (pounds)",
                             title_offset = 75) %>%
                    add_legend("fill", title = "Cluster",
                               properties=legend_props(title=list(fontSize = 18),
                                                       labels=list(fontSize = 16))#,
                               #values = c("Outlier","Central")
                                )
                })
    vis3 %>%  bind_shiny("practices_plot")

}

shinyApp(ui, server)