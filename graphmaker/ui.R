#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("LIA Graph"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Input Data"),
            selectInput("input_type",
                        "Select Input Type",
                        choices = list("Single", "Multiple"),
                        selected = "Single"),
            conditionalPanel(
                condition = "input.input_type == 'Multiple'",
                fileInput("iso_file", "Select Csv file",
                          accept = ".csv"),
                uiOutput("main_table_names"),
                ),
            conditionalPanel(
                condition = "input.input_type == 'Single'",
                fluidRow(
                    column(
                        width = 4,
                        numericInput("pb64", "Enter 206 Pb:", 
                                     value = 18.5,
                                     step = 0.1),
                    ),
                    column(
                        width = 4,
                         numericInput("pb74", "Enter 207 Pb:", 
                             value = 15.7,
                             step = 0.1)
                    ),
                    column(
                        width = 4,
                                        numericInput("pb84", "Enter 208 Pb:", 
                             value = 38.7,
                             step = 0.1)
                    )
                )
            ),
            
            
            # Data base filtering -----------------------------------------------------
            
            h4("References Database"),
            selectInput(
                "base_group",
                "Select Grouping",
                list(
                    "Country" = "Country",
                    "Region" = "corrected Region",
                    "Mine" = "Mine"
                )
            ),
            numericInput("numbuer_of_distances",
                         "No. of points per sample",
                         value = 5,
                         min = 1,
                         max = 10,
                         step = 1),
            uiOutput("group_selector"),
            uiOutput("pb_filter"),
            uiOutput("cu_filter"),
            
            
            # Plot editing ------------------------------------------------------------
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                
                tabPanel(
                    "Table",
                    dataTableOutput("main_table2"),
                    downloadButton("downloadData", "Download Distance Table")
                ),
                
                tabPanel(
                    "Graph Maker",
                    fluidRow(
                        column(
                            width = 5,
                            textInput("title", "Title", 
                                      placeholder = "Enter title..."),
                            fluidRow(
                              column(
                                  width = 3,
                                  checkboxInput("iso_lines", 
                                                "Toggle Iso Lines", TRUE)
                              ),
                              column(
                                width = 3,
                                checkboxInput("point_lables", 
                                              "Toggle point lables", FALSE)
                              )
                            ),
                            
                            
                            
                            fluidRow(
                                column(
                                    4,
                                    numericInput(
                                        "main_shape",
                                        "Main Shape:",
                                        value = 16,
                                        min = 0,
                                        max = 25
                                    )
                                ),
                                column(
                                    4,
                                    numericInput(
                                        "main_size",
                                        "Main Size:",
                                        value = 1,
                                        min = 0,
                                        max = 4,
                                        step = 0.5
                                    )
                                ),
                                column(
                                    4,
                                    textInput("main_color", "Main Color:", value = "black") # Changed from placeholder to value for better reliability
                                )
                            ),
                            fluidRow(column(
                                5,
                                numericInput(
                                    "base_shape",
                                    "Base Shape:",
                                    value = 3,
                                    min = 0,
                                    max = 25
                                )
                            ), column(
                                5,
                                numericInput(
                                    "base_size",
                                    "Base Size:",
                                    value = 0.5,
                                    min = 0,
                                    max = 4,
                                    step = 0.5
                                )
                            )),
                            fluidRow(column(
                                5,
                                textInput("main_iso_color", "Main Iso Color:", value = "grey30")
                            ), column(
                                5,
                                textInput("second_iso_color", "Second Iso Color:", value = "grey70")
                            )),
                            numericInput(
                                "legend_ncol",
                                "Legend Columns",
                                value = 4,
                                min = 1,
                                max = 6,
                                step = 1
                            ),
                            numericInput(
                                "image_sclae",
                                "Img Scale",
                                value = 180,
                                min = 100,
                                max = 200
                            )
                        ),
                        column(
                            width = 6,
                            plotOutput("plot")
                            
                        )
                    )
                    
                )
            )
        )
    )
)
