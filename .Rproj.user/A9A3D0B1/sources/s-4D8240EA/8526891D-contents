# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(openxlsx)
library(DT)
library(shinyBS)

source('dataset_manip.R')

ui <- fluidPage(
   
   tabsetPanel(
      tabPanel("Import Table 4 data",
               hr(),
               h4("Input the Tableur Results file containg - Table 4 Estimation Pop"),
               fileInput('file', 'Input'),
               bsAlert('alert1'),
               uiOutput('select_sheet'),
               tableOutput('table'),
               h4("After importing Table 4 - go to other tabs")
               ),
      tabPanel("Import and Compare Table 3 - Current",
               hr(),
               h4("Select the file/sheet with  Table 3 - Synthesis & Classif Current and press the Load Data button"),
               fileInput('file2', 'Input'),
               uiOutput('select_sheet2'),
               actionButton('run', 'Load data'),
               hr(),
               h4('Table below shows areas with difference in calculation of phase or names that are not the same'),
               DTOutput('table2'),
               hr(),
      ),
      #show output of the current join
      tabPanel("Import and Compare Table 3 - Projected",
               hr(),
               h4("Input the file containg Table 3 - Synthesis & Classif Projected and press the Load Data button"),
               fileInput('file3', 'Input'),
               uiOutput('select_sheet3'),
               actionButton('run3', 'Load data'),
               hr(),
               h4('Table below shows areas with difference in calculation of phase or names that are not the same'),
               DTOutput('table3'),
               hr(),
      )
   )
)



server <- function(input, output, session) {

   workbook_reactive <- reactiveVal()
   workbook_reactive2 <- reactiveVal()
   workbook_reactive3 <- reactiveVal()
   
   # triggers only when event happens
   observeEvent(input$file,{
      print('New file uploadedd')
      workbook_reactive(NULL)
      closeAlert(session, 'al1')
      closeAlert(session, 'al2')
      infile <- input$file
      dataFile <- infile$datapath
      print(dataFile)
      
      excel_file <- try(openxlsx::loadWorkbook(dataFile), silent = T)
      
      if(class(excel_file) == "try-error"){
         createAlert(session, 'alert1','al1', content = 'Not an XLSX')
      }else{
         createAlert(session, 'alert1','al2', content = 'Correct File')
         workbook_reactive(excel_file)
      }

   })
   
   # FILE 2
   observeEvent(input$file2,{
      print('New file uploadedd')
      workbook_reactive2(NULL)
      # closeAlert(session, 'al1')
      # closeAlert(session, 'al2')
      infile <- input$file2
      dataFile <- infile$datapath
      print(dataFile)
      
      excel_file <- try(openxlsx::loadWorkbook(dataFile), silent = T)
      
      if(class(excel_file) == "try-error"){
         createAlert(session, 'alert1','al1', content = 'Not an XLSX')
      }else{
         createAlert(session, 'alert1','al2', content = 'Correct File')
         workbook_reactive2(excel_file)
      }
      
   })
   
   # FILE 3
   observeEvent(input$file3,{
      print('New file uploadedd')
      workbook_reactive3(NULL)
      # closeAlert(session, 'al1')
      # closeAlert(session, 'al2')
      infile <- input$file3
      dataFile <- infile$datapath
      print(dataFile)
      
      excel_file <- try(openxlsx::loadWorkbook(dataFile), silent = T)
      
      if(class(excel_file) == "try-error"){
         createAlert(session, 'alert1','al1', content = 'Not an XLSX')
      }else{
         createAlert(session, 'alert1','al2', content = 'Correct File')
         workbook_reactive3(excel_file)
      }
      
   })
   
   output$select_sheet <- renderUI({
      req(!is.null(workbook_reactive()))
      sheets_sel <- names(workbook_reactive())
      selectInput('mysheet', label = 'Select Sheet', choices = sheets_sel)
   })
   
   output$select_sheet2 <- renderUI({
      req(!is.null(workbook_reactive2()))
      sheets_sel <- names(workbook_reactive2())
      selectInput('mysheet2', label = 'Select Sheet', choices = sheets_sel)
   })
   
   output$select_sheet3 <- renderUI({
      req(!is.null(workbook_reactive3()))
      sheets_sel <- names(workbook_reactive3())
      selectInput('mysheet3', label = 'Select Sheet', choices = sheets_sel)
   })
   
   dataset_reactive <- reactive({
      req(!is.null(workbook_reactive()))
      req(input$mysheet)
      df <- openxlsx::read.xlsx(xlsxFile = workbook_reactive(),
                          sheet = input$mysheet,
                          startRow = 1) 
      df[-c(1:3),]
   })
   
   dataset_reactive2 <- reactiveVal()
   dataset_reactive3 <- reactiveVal()
   
   observeEvent(input$run,{
      df <- openxlsx::read.xlsx(xlsxFile = workbook_reactive2(),
                                sheet = input$mysheet2,
                                startRow = 1) 
  
      df = snc_format(df) %>% 
         full_join(
            dataset_clean(dataset_reactive(), sheet_type = 'current')
         ) %>% select(-adm0_name) %>% mutate(across(foodconsumption_phase:final_phase, round, 0)) %>% filter(final_phase_calc != final_phase | is.na(final_phase_calc) | is.na(final_phase))
     
      dataset_reactive2(df)

   })

   observeEvent(input$run3,{
      df <- openxlsx::read.xlsx(xlsxFile = workbook_reactive3(),
                                sheet = input$mysheet3,
                                startRow = 1) 
      
      df = snc_format(df) %>% 
         full_join(
            dataset_clean(dataset_reactive(), sheet_type = 'projected')
         ) %>% select(-adm0_name) %>% mutate(across(foodconsumption_phase:final_phase, round, 0)) %>% filter(final_phase_calc != final_phase | is.na(final_phase_calc) | is.na(final_phase))
      
      dataset_reactive3(df)
   })
     
   
   output$table <- renderTable({
      dataset_reactive()
   })


   output$table2 <- renderDT({
      
      dataset2 <- dataset_reactive2()
      
      datatable(dataset2, filter = "top",
                extensions = 'Buttons',
                options = list( # pageLength = ALL, info = FALSE,
                   lengthMenu = list(c(15, -1), c("15", "All")),
                   dom = 'lBfrtip',
                   buttons = c('copy', 'csv')
                )
      )
   })      
   
   
   
   output$table3 <- renderDT({
      
      dataset3 <- dataset_reactive3()
 
      datatable(dataset3, filter = "top",
                extensions = 'Buttons',
                options = list( # pageLength = ALL, info = FALSE,
                   lengthMenu = list(c(15, -1), c("15", "All")),
                   dom = 'lBfrtip',
                   buttons = c('copy', 'csv')
                )
      )
   })      
           
   

   
   # output current results
   output$dataset_current_dt <- renderDT({
      
      dataset <- dataset_reactive()
      dataset_current <- dataset_clean(dataset, sheet_type = 'current')
      
      datatable(dataset_current, filter = "top",
                           extensions = 'Buttons',
                           options = list( # pageLength = ALL, info = FALSE,
                                               lengthMenu = list(c(15, -1), c("15", "All")),
                                               dom = 'lBfrtip',
                                               buttons = c('copy', 'csv')
                                             )
               )
         })
   
   output$dataset_projected_dt <- renderDT({
      
      dataset <- dataset_reactive()
      dataset_current <- dataset_clean(dataset, sheet_type = 'projected')
      
      datatable(dataset_current, filter = "top",
                extensions = 'Buttons',
                options = list( # pageLength = ALL, info = FALSE,
                   lengthMenu = list(c(15, -1), c("15", "All")),
                   dom = 'lBfrtip',
                   buttons = c('copy', 'csv')
                )
      )
   })
   
   
   
} 

# Run the application 
shinyApp(ui = ui, server = server)
