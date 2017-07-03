
# Accessibility and Quality of Care App



# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# options(shiny.reactlog=T)


source("../../preamble.R", chdir=T, local=T)

source("care_snippets.R",local=TRUE)$value

info <- source("info.R")$value

load("tables/CARE_TABLES.Rdata")



###########################################

## this bad here -- change it

stat_labels <- list("pctPOP"="Percent of population")

grp_labels <- subgrps %>% invertList  
care_labels <- c(care_subgrps %>% invertList)

care_labels$Year = "Year"

###########################################

body_ui <- bootstrapPage(
 
  div(class='select-box',
      
      fluidRow508(
        col508(width="one-half",h1(info$title), p(info$description), p(info$instructions) ),

        col508(width="one-half",
               
               tags$form(class = "usa-form-large",    
                         
                         rcInput("care",type="cols",choices=care_subgrps,label="Select variable:"),
                         
                         div(
                           checkboxInput508("showSEs",label="Show Standard Errors",inline=T),
                           uiOutput("CInote",class="inline control-message")
                         ),
                         
                         dataViewInput("care"),
                         
                         yearInput("care",min=min(care_tables$Year),max=max(care_tables$Year)),
                         
                         rcInput("care",type="rows",choices=subgrps,class="hide-if-trend")
                         
               )
        )
        
      )),
  
    
  div(class='usa-grid',
      addLinks(
        tabsetPanel(type="pills",
                    tableUI("care"),
                    plotUI("care"),
                    codeUI("care")
        ),
        downloadUI("care"), class="right"),
      
      hc_info("care")
  )
)



##################################

ui <- htmlTemplate("../../../template.html", body = body_ui)

df <- care_tables
grp_labels = care_labels

server <- function(input, output, session) {
  
  stat <- reactive('pctPOP')
  
  adj <- reactive({
    D = 1E-2; d = 1; lab = "";
    return(list(D=D,d=d,label=lab))
  })
  
  
  showSEs <- reactive({
    input$showSEs 
  }) 
  
  footnotes <- reactive({  
    tbl <- meps_table()
    footnotes = list()
    footnotes$suppress <- ifelse(any(tbl$suppress,na.rm=T),suppressed_message,"")
    footnotes$star     <- ifelse(any(tbl$star,na.rm=T),rse_message,"")
    return(footnotes)
  })
  
  ##############
  
  meps_inputs <- callModule(inputModule,"care")
  
  
  # in:  stat, adj, inputs(years,cols,rows,rowsX)
  # out: labels, caption
  meps_labels <- 
    callModule(notesModule,"care", 
               stat = stat, 
               inputs = meps_inputs, 
               adj = adj)
  
  
  # in:  fulldata, rows, cols, stat
  # out: filtered_data
  meps_table <- callModule(dataModule,"care", df=care_tables, 
                           stat = stat,
                           inputs = meps_inputs)
  
  
  # in:  filtered_data, caption, footnotes, row_label, col_label, showSEs
  # out: table, downloadtable
  callModule(tableModule,"care",
             tbl = meps_table,    
             showSEs = showSEs,
             controlTotals = controlTotals,
             caption = reactive(meps_labels()$caption),
             labels = reactive(meps_labels()$labels),
             footnotes = footnotes,
             adj = adj)
  
  # in:  filtered_data, caption, footnotes, row_label, col_label, showSEs
  # out: plot, legend
  callModule(plotModule,"care", 
             tbl = meps_table, 
             stat=stat,
             showSEs = showSEs,
             inputs = meps_inputs,
             adj = adj,
             caption = reactive(meps_labels()$caption))
  
  
  # in:  year, rows, cols
  # out: code
  callModule(codeModule,"care")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
