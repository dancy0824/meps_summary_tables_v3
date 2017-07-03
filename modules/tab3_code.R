#######################################################
###                   FUNCTIONS                     ###
#######################################################

readCode <- function(fileName,path=""){
  fileLoc = paste0(path,fileName)
  readChar(fileLoc,file.info(fileLoc)$size)
}

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

get_sas_code <- function(subgrps,stat='totEXP'){

# Generic code loading
  load_code <- readCode("../shared/code/sas/load_fyc.sas")

  real_grps <- subgrps %>% unique
  if(length(real_grps) > 1) real_grps <- real_grps[real_grps!="ind"]

  ## add agevar if needed

  grp_code <- sapply(real_grps, function(x)
    readCode(sprintf("../shared/code/sas/grps/%s.sas",x))) %>%
    paste(collapse="\n")

  stat_code <- readCode(sprintf("code/sas/stats/%s.sas",stat))

# SAS-specific

  format <- paste(real_grps,paste0(real_grps,"."),collapse=" ")
  domain <- paste(real_grps,collapse="*")

  paste(load_code, grp_code, stat_code, collapse="\n") %>%
    rsub(type='sas',
         yy = 14, year = 2014,
         format = format,
         domain = domain)

  #%>% writeLines
}


# 
# get_sas_code(c('ind','sex'),stat="totEXP") %>% writeLines
# 


codeModule <- function(input, output, session, inputs){
  
  # r_code <- reactive({
  #   yr = max(inputs()$years) %>% substring(3,4)
  # 
  #   main_code <- fyc_code(stat=inputs()$stat,yr=yr, 
  #                         subgrps=c(inputs()$rows, inputs()$cols))
  # 
  #   tagList(
  #     tags$p(
  #       "The following R code can be used to generate the selected estimates.
  #       For trend estimates, example code is shown for the most recent year selected."),
  #     
  #     tags$p(
  #       "Load R packages and define function to download MEPS data from web:"
  #     ),
  #       
  #     tags$pre(tags$code(snp_load$load_packages)),
  #     
  #     tags$p("Run analyses based on selected statistic and grouping variables:"),
  #     tags$pre(tags$code(main_code))
  #    
  #   )
  # })
  # 
  # sas_code <- reactive({
  #   tagList(
  #     tags$p("The following SAS code can be used to generate the selected estimates.
  #       For trend estimates, example code is shown for the most recent year selected."),
  #     
  #     tags$pre(tags$code(
  #       get_sas_code(subgrps = c(inputs()$rows,inputs()$cols))
  #     ))
  #   )
  # })
  # 
  # stata_code <- reactive({
  #   tagList(
  #     tags$p("You must be an economist. We would love to have Stata code here. 
  #            If you would like to contribute, send us an email!")
  #   )
  # })
  # 
  # output$code <- renderUI({
  #   
  #   if(input$code_language=="R") return(r_code())
  #   if(input$code_language=="SAS") return(sas_code())
  #   if(input$code_language=="Stata") return(stata_code())
  # })  
  
  
  # output$load_code <- renderText({
  #   snp_load$load_packages
  # })
  # 
  # output$code <- renderText({
  #   
  
  #    
  #   # code_string_use(
  #   #   stat = stat(), 
  #   #   grp1=inputs()$rows, 
  #   #   grp2 = inputs()$cols,
  #   #   year=max(inputs()$years))
  #   
  # })
  # outputOptions(output,"code",suspendWhenHidden = FALSE) 


# 
#   r_code <- reactive({
#     yr = max(inputs()$years) %>% substring(3,4)
# 
#     main_code <- fyc_code(stat=inputs()$stat,yr=yr,
#                           subgrps=c(inputs()$rows, inputs()$cols))
# 
#     tagList(
#       tags$p(
#         "The following R code can be used to generate the selected estimates.
#         For trend estimates, example code is shown for the most recent year selected."),
# 
#       tags$p(
#         "Load R packages and define function to download MEPS data from web:"
#       ),
# 
#       tags$pre(tags$code(snp_load$load_packages)),
# 
#       tags$p("Run analyses based on selected statistic and grouping variables:"),
#       tags$pre(tags$code(main_code))
# 
#     )
#   })
# 
#   sas_code <- reactive({
#     tagList(
#       tags$p("The following SAS code can be used to generate the selected estimates.
#         For trend estimates, example code is shown for the most recent year selected."),
# 
#       tags$pre(tags$code(
#         get_sas_code(subgrps = c(inputs()$rows,inputs()$cols))
#       ))
#     )
#   })
# 
#   stata_code <- reactive({
#     tagList(
#       tags$p("You must be an economist. We would love to have Stata code here.
#              If you would like to contribute, send us an email!")
#     )
#   })
# 
  output$code <- renderUI({
    "hi"
    # if(input$code_language=="R") return(r_code())
    # if(input$code_language=="SAS") return(sas_code())
    # if(input$code_language=="Stata") return(stata_code())
  })

}

