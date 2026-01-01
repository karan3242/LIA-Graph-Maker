#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    main_table1 <- reactive({read_csv(input$iso_file$datapath)})
    
    output$main_table_names <- renderUI({
        choices <- unique(names(main_table1()))
        selectInput("colnames_select",
                    "Select iso colums grom 206, 207 and 208 in that order",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })

    
    main_table2 <- reactive({
        main_table1() %>% 
            select(input$colnames_select)
    })
    

# Base Table --------------------------------------------------------------

    

    base_table1 <- reactive({data_base %>% 
            select(input$base_group, `Presence of Pb`, `Presence of Cu`, `206Pb/204Pb`, `207Pb/204Pb`, `208Pb/204Pb`)})
    
    output$group_selector <- renderUI({
        choices <- unique(base_table1()[[1]])
        selectInput("group_selector",
                    "Select Sample Groups",
                    choices = choices,
                    selected = choices[as.integer(runif(3, 1, length(choices)))],
                    multiple = TRUE)
    })
    
    output$pb_filter <- renderUI({
        choices <- unique(base_table1()[[2]])
        selectInput("pb_select",
                    "Select Pb Values",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    output$cu_filter <- renderUI({
        choices <- unique(base_table1()[[3]])
        selectInput("cu_select",
                    "Select Cu Values",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    base_table2 <- reactive({
        req(input$group_selector)
        
        base_table1() %>% 
            filter(.[[1]] %in% input$group_selector &
                   .[[2]] %in% input$pb_select &
                       .[[3]] %in% input$cu_select)
            
    })
    
    output$plot <- renderPlot({
        graph(df = main_table2(),
              df2 = base_table2(),
              col2 = as.factor(base_table2()[[1]]),
              legend = unique(as.factor(base_table2()[[1]])),
              legend_col = unique(as.numeric(as.factor(base_table2()[[1]]))),
              title = input$title,
              pch = input$main_shape,
              cex = input$main_size,
              col = input$main_color,
              pch2 = input$base_shape,
              cex2 = input$base_size,
              iso_lines = input$iso_lines,
              t_color = input$main_iso_color,
              iso_color = input$second_iso_color)}, 
        width = function(){3 * req(input$image_sclae)}, 
        height = function(){4 * req(input$image_sclae)}
        )
    
    # output$table <- renderTable({base_table2()})

}
