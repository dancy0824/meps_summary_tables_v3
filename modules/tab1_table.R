#######################################################
###                   FUNCTIONS                     ###
#######################################################

spread_tbl <- function(data,stat,labels=NULL){               print("spread_tbl")
  data %>%
    mutate(cols = factor(cols, levels = unique(cols)),
           rows = factor(rows, levels = unique(rows))) %>%
    select_("rows","cols",stat) %>% distinct %>%
    spread_("cols",stat) %>%
    rename_cols(labels)
}


#######################################################
###                      UI                         ###
#######################################################

tableUI<- function(id){
  ns <- NS(id)
  
  tabPanel(title="Table",icon=icon("table"), 
           uiOutput(ns('meps_table')),
           uiOutput(ns("table_footnotes"))
  )
  
}

downloadUI <- function(id){
  ns <- NS(id)
  
  downloadButton508( ns("csv"), 
    class = 'download-button',
    label = 'Download table', 
    icon=icon('download'))
}


#######################################################
###                     SERVER                      ###
#######################################################

tableModule <- function(input, output, session, tbl, inputs, adj, labels){
  
  formatted_tbl <- reactive({
    denom = adj()$D; digits = adj()$d;
    
    tbl() %>%  
      mutate_at(vars(coef,se),funs(formatNum(./denom,d=digits))) %>%
      mutate(coef = ifelse(star,paste0(coef,"*"),coef),
             coef_se = sprintf("%s  <span class='gray'>(%s)</span>",coef,se))
  }) 
  

# Display table     

  display_tbl <- reactive({                                              print("display_tbl()...")
     tab <- formatted_tbl() %>% 
      spread_tbl(stat = ifelse(inputs()$showSEs,"coef_se","coef"), labels = labels()$labels) 
 
  })
  
  output$meps_table <- renderUI({                                     cat("\n\nOUTPUT$MEPS_TABLE..\n")
    HTML508table(body = display_tbl(), caption = labels()$caption)
  })
  
  output$table_footnotes <- renderText(paste0(labels()$footnotes,collapse=""))
  
  
# Download table

  coef_tab <- reactive(formatted_tbl() %>% spread_tbl(stat="coef",labels=labels()$labels))
  se_tab   <- reactive(formatted_tbl() %>% spread_tbl(stat="se",  labels=labels()$labels))
  
  output$csv <- downloadHandler(
    filename = function(){ paste('meps-hc-accessed-',Sys.Date(),'.csv',sep='') },
    content = function(file){
      
      write.table(caption(),file,sep=",",row.names=F,col.names=F)
      add.table(coef_tab(),file)
      
      if(!controlTotals()){
        cap <- caption() %>% str_replace(.,word(.,1),word(.,1)%>%tolower)
        se_caption <- paste("Standard errors for",cap)
        add.table(se_caption,file,col.names="")
        add.table(se_tab(),file)
      }
      
      foots <- footnotes() %>% rm_html %>% rm_brks %>% rm_xspc
      foots <- foots[foots!=""]
      if(controlTotals()) foots = c(foots,controlTotals_message)

      for(fn in c(" ",foots," ")) add.table(fn,file,col.names=F)
      #add.table(paste("Source:",source_text()),file,col.names=F)
    })   

}


