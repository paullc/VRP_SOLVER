library(shiny)
library(leaflet)
library(shinyjs)

ui <- fluidPage(
        h1(HTML("<CENTER>Vehicle Routing Problem Solver</CENTER>")),      
        sidebarLayout(
                sidebarPanel(
                fileInput("datosvrp","Upload *.xlsx file",accept = c(".xlsx")),
                radioButtons("Costsel", "Select the cost variable", c("Distance" = "Distance", "Time" = "Time"),selected = "Distance"),
                fluidRow(column(width = 5, 
                                numericInput("Capacity", "Enter the trucks' capacity (Don't use thousands separator)", 
                                             value = 10000, step = 50)),
                         column(width = 3, 
                                 textInput("Unit", "Enter the units for the capacity", value = "Unit"))
                        ),
                useShinyjs(),
                actionButton("Run","Run"),
                helpText("Insert a number greater than 0 to run the program")
                ),
                mainPanel(
                  uiOutput("main")
                )
        )
)
        

server <- function(input, output, session) {
       
        observe({
            toggleState(id = "Run", condition = input$Capacity > 0)
        })
  
        observeEvent(
          input$Run, updateTabsetPanel(session, inputId="tabset", selected = "Solution")
        )
  
        filedata<-reactive({
                infile<-input$datosvrp
                if(is.null(infile)) return(NULL)
                library(xlsx)
                dat <- read.xlsx(infile$datapath,sheetIndex = 1, header = F)
                colnames(dat) <- c("Name","Latitude","Longitude","Demand")
                rownames(dat) <- seq(from = 0, to = (nrow(dat)-1), by = 1)
                dat
        })
        
        datavrp<-reactive(withProgress(message = "Calculation in progress", detail = "Please Wait", value = 0,{
                if(is.null(filedata()))
                        return ()
                else{
                        source('get_matrix_alt.R', local = T)
                        dat <- get_matrix_alt(filedata())
                        if(is.null(dat))
                            incProgress(0)
                        else{
                            incProgress(1)
                            Sys.sleep(0.25)
                            dat
                            }
                        }
        }))
        
        selection<-reactive({switch (input$Costsel,
                                    Distance = "D",
                                    Time = "T")
        })
        
        cap<-reactive({
             input$Capacity
        })
        
        optimal<-eventReactive(input$Run,{
                source('savings_alg_alt.R', local = T)
                savings_alg_alt(datavrp(), cap(), selection()) 
        })
        
        nesarcs<-eventReactive(input$Run,{
          source('get_geometry.R', local = T)
          get_geometry(datavrp(), optimal()) 
        })
        
        solution <- reactive({
          results<-optimal()
          sol <- cbind(results[[1]], results[[2]], results[[3]], results[[4]])   
          sol <- data.frame(sol)
          sol[] <- lapply(sol,as.character)
          sol[,2:4] <- lapply(sol[,2:4],as.numeric)
          sol[,2:4] <- round(sol[,2:4], 2)
          colnames(sol) <- c("Route",paste("Load (", input$Unit, ")", sep = ""),"Dist (km)","Time (minutes)")
          sol
        })
        
        plot<-reactive(withProgress(message = "Calculation in progress", detail = "Please Wait", value = 0,{
                 if(is.null(optimal)) return ()
                 else{source('mapgen.R', local = T)
                      graph <- map(filedata(), nesarcs())
                      if(is.null(graph))
                           incProgress(0)
                      else{
                           incProgress(1)
                           Sys.sleep(0.25)
                           graph
                          }
                 }
                 
         }))
        
        output$Optimal_Solution<-renderLeaflet({
                 plot()
         })
        
        output$Info <- renderTable({
                filedata()
        })
        
        output$Distance <- renderTable({
                dist <- datavrp()$Distance
                colnames(dist) <- seq(0, (ncol(dist)-1),1)
                rownames(dist) <- seq(0, (ncol(dist)-1),1)
                dist
        })
        
        output$Time <- renderTable({
                tim <- datavrp()$Time
                colnames(tim) <- seq(0, (ncol(tim)-1),1)
                rownames(tim) <- seq(0, (ncol(tim)-1),1)
                tim
        })
        
        output$solution <- renderTable({
          solution()
        })
        
        # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
        output$main <- renderUI({
          if(is.null(filedata())){
            wellPanel(
              h5("By", tags$img(src='logoiner.png', heigth=400, width=400), align = "center"),
              br(),
              div(),
              includeHTML('README.html')
            )
          }
          else 
                tabsetPanel(id = "tabset",
                tabPanel("Readme", 
                           includeHTML('README.html')
                       ),
                tabPanel("Problem Information",
                       tableOutput("Info")
                       ),
                tabPanel("Cost Matrices",
                       h4("Distance Matrix (km)"),   
                       tableOutput("Distance"),
                       h4("Time Matrix (min)"),   
                       tableOutput("Time"), align = "center"
                       ),
                tabPanel("Solution",
                         if (input$Run == 0){
                           h3("Click the Run button to see the solution", align = "center")
                         }
                         else{
                           tableOutput("solution")
                         }   
                        ),
               tabPanel("Map",
                        if (input$Run == 0){
                          h3("Click the Run button to see the map", align = "center")
                        }
                        else{
                          leafletOutput("Optimal_Solution", height = 600)
                        }
                        )
            )
        })

}

shinyApp(ui = ui, server = server)