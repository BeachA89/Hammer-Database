

ui <- dashboardPage(
    dashboardHeader (title = "Hammer Throw Database", titleWidth = 450),
    dashboardSidebar(
        # fileInput("file1", "Choose files", multiple = TRUE,
        #           accept = c("text/csv",
        #                      "text/comma-separated-values,text/plain",
        #                      ".csv")),
        # actionButton("goButton", "Go!"),
        
        # selectInput("distance", "Distance:",
        #             choices = c("500" = "Labelled_data_500",
        #                         "1000" = "Labelled_data_1000")),
        # selectInput("Report_Type", "Report Type:",
        #             c("Single Race" = "Single Race",
        #               "Two Races" = "Two Races",
        #               "vs Top 10" = "vs Top 10")),    
        uiOutput("select_Name"),
        selectInput("Report_Type", "Report Type:",
                 c("Single Comp" = "Single Comp",
                   "Distance Comparison" = "Distance Comparison")),    
        uiOutput("select_Competition"),
        checkboxInput("checkbox1", ">70", value = FALSE),
        checkboxInput("checkbox2", "68-70", value = FALSE),
        checkboxInput("checkbox3", "66-68", value = FALSE),
        checkboxInput("checkbox4", "64-66", value = FALSE)
        
        # 
        # 
        # 
        # selectInput("Distance1", "Distance:",
        #          choices = c(">70" = ">70",
        #                      "68-70" = "68-70",
        #                      "66-68" = "66-68",
        #                      "64-66" = "64-66")),
        # selectInput("Distance2", "Distance:",
        #             choices = c(">70" = ">70",
        #                         "68-70" = "68-70",
        #                         "66-68" = "66-68",
        #                         "64-66" = "64-66"))
        
        
    ),
    dashboardBody(
        
        fluidRow(
            h3(textOutput("Summaryhead"))
        ),
        fluidRow(
            box(title = "Release Data and Footfall Timing", status = "primary", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                DT::dataTableOutput("datatable1"))
        ),
        fluidRow(
            box(title = "Turn Time", status = "primary", solidHeader = TRUE,width = 12,
                collapsible = TRUE,plotOutput("ggplot1"))
        ),
        
        fluidRow(
            box(title = "Turn Time", status = "primary", solidHeader = TRUE,width = 12,
                collapsible = TRUE,plotOutput("ggplot2"))
        ),
        fluidRow(
            box(title = "Turn Time", status = "primary", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                DT::dataTableOutput("datatable2"))
        ),
        fluidRow(
            box(title = "Right Foot Down to Left Foot Toe Up", status = "primary", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                DT::dataTableOutput("datatable3"))
        ),

        fluidRow(
            box(title = "", status = "primary", solidHeader = TRUE,width = 12,
                collapsible = TRUE,plotOutput("ggplot3"))
        )#,
        
        #fluidRow(
        #    box(title = "Distance Comparison", status = "primary", solidHeader = TRUE, width = 12,
        #        collapsible = TRUE,
        #        DT::dataTableOutput("datatable_comparison"))
        #)

        #    fluidRow(
        #      box(title = "Time vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
        #          collapsible = TRUE,plotOutput("ggplot"))
        #    )
        
        
        
    )
    
)










# Create Shiny app ----
# shinyApp(ui, server)