#######################################################
###                      UI                         ###
#######################################################

codeUI <- function(id){
  ns <- NS(id)
  
  tabPanel(title="Code",icon=icon("code"),
           
           selectInput508(ns("code_language"),
                          label = "Select Programming Language:",
                          choices = c("R","SAS","Stata")),
           
           uiOutput(ns("code"))
      )
  
}


#######################################################
###                     SERVER                      ###
#######################################################

codeModule <- function(input, output, session, inputs){
   
  grps <- reactive(c(inputs()$rows, inputs()$cols))
  year <- reactive(max(inputs()$years))
  yr <- reactive(substring(year(),3,4))
  stat <- reactive(inputs()$stat)
 
  r_code <- reactive({

    main_code <- get_r_code(grps=grps(),stat=stat(),yr=yr())

    tagList(
      tags$p(
        "The following R code can be used to generate the selected estimates.
         For trend estimates, example code is shown for the most recent year selected."),
      
      tags$p("Load R packages and define subgroups (if applicable):"),
      tags$pre(tags$code(load_code(grps(),stat(),year=year(),lang="r"))),
      
      tags$p("Run analyses based on selected statistic and grouping variables:"),
      tags$pre(tags$code(main_code))
    )

  })
  

  sas_code <- reactive({
    
    main_code <- get_sas_code(grps=grps(),stat=stat(),yr=yr())
    
    tagList(
      tags$p("The following SAS code can be used to generate the selected estimates.
        For trend estimates, example code is shown for the most recent year selected."),

      tags$p("Load data and define subgroups (if applicable):"),
      tags$pre(tags$code(load_code(grps(),stat(),year=year(),lang="sas"))),
      
      tags$p("Run analyses based on selected statistic and grouping variables:"),
      tags$pre(tags$code(main_code))
    )
  })
  
  stata_code <- reactive({
    tagList(
      tags$p("You must be an economist. We would love to have Stata code here.
             If you would like to contribute, send us an email!")
    )
  })

  output$code <- renderUI({

    if(input$code_language=="R") return(r_code())
    if(input$code_language=="SAS") return(sas_code())
    if(input$code_language=="Stata") return(stata_code())
  })
  

}

