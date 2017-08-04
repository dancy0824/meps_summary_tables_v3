#######################################################
###                      UI                         ###
#######################################################

codeUI <- function(id){
  ns <- NS(id)
  
  tabPanel(title="Code",icon=icon("code"),
           
           div(class='code-select',
             selectInput508(ns("code_language"),
                            label = "Select Programming Language:",
                            #choices = c("R","SAS","Stata")),
                            choices = c("R","SAS")),
             
             downloadButton508( ns("dl_code"), 
                                class = 'download-button',
                                label = 'Download Code', 
                                icon=icon('download'))
            ),
           
           tags$p(
             "The following code can be used to generate the selected estimates.
              For trend estimates, example code is shown for the most recent year selected:"),
           
           tags$pre(tags$code(uiOutput(ns("code"),role="region","aria-live"="polite")))
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
  
  lang <- reactive(input$code_language)
 
  r_code <- reactive({
    main_code <- get_r_code(grps=grps(),stat=stat(),yr=yr())
    paste(load_code(grps(),stat(),year=year(),lang="r"),main_code)
  })
  
  sas_code <- reactive({
    main_code <- get_sas_code(grps=grps(),stat=stat(),yr=yr())
    paste(load_code(grps(),stat(),year=year(),lang="sas"),main_code)
  })
  
  stata_code <- reactive({
    tagList(
      tags$p("You must be an economist. We would love to have Stata code here.
             If you would like to contribute, send us an email!")
    )
  })

  out_code <- reactive({
    if(lang()=="R") return(r_code())
    if(lang()=="SAS") return(sas_code())
    if(lang()=="Stata") return(stata_code())
  })
  
  output$code <- renderUI(out_code())

  ## Download code
  
  output$dl_code <- downloadHandler(
    filename = function(){ sprintf('meps-%s-code.%s',lang(),lang()) },
    content = function(file){
       write(out_code(),file=file) 
    })   

}

