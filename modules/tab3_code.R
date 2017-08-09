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
           
           tags$p(HTML(
             "To run the code, first download and unzip the required public use data files from the
             <a href = 'https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp' target='_blank_'>
              MEPS data files page</a>, and save them to your computer.  
             More information on downloading and analyzing MEPS data in R, SAS, and Stata can be found 
             at the <a href = 'https://github.com/HHS-AHRQ/MEPS#accessing-meps-hc-data' target='_blank_'>AHRQ GitHub site</a>."
           )),
           
           tags$p("The following code can be used to generate the selected estimates, where
            the SAS transport data files (.ssp) have been saved to the folder 'C:\\MEPS'.
            For trend estimates, example code is shown for the most recent year selected:"),

           tags$pre(uiOutput(ns("code"),role="region","aria-live"="polite"))
      )
  
}


#######################################################
###                     SERVER                      ###
#######################################################

codeModule <- function(input, output, session, inputs){
   
  rows <- reactive(inputs()$rows)
  cols <- reactive(inputs()$cols)
  year <- reactive(max(inputs()$years))
  stat <- reactive(inputs()$stat)
  
  lang <- reactive(input$code_language)
 
  r_code <- reactive({
    paste(get_r_code(rows=rows(),cols=cols(),stat=stat(),year=year()))
  })
  
  sas_code <- reactive({
    paste(get_sas_code(rows=rows(),cols=cols(),stat=stat(),year=year()))
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

