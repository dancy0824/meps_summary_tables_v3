#######################################################
###                   FUNCTIONS                     ###
#######################################################

spread_tbl <- function(data,stat,labels=NULL,pivot=FALSE){    
  spread_by = ifelse(pivot,"rows","cols")
  
  data %>%
    arrange(-Year) %>%
    mutate(cols = factor(cols, levels = unique(cols)),
           rows = factor(rows, levels = unique(rows))) %>%
    select_("rows","cols",stat) %>% distinct %>%
    spread_(spread_by,stat) %>%
    rename_cols(labels)
}


#######################################################
###                      UI                         ###
#######################################################


tableUI<- function(id,DT=F){
  ns <- NS(id)
  
  if(DT){#tbl <- DT::dataTableOutput(ns('meps_DT'))
     tbl <- uiOutput(ns('meps_DT'),role="region","aria-live"="polite")
  }else{ tbl <- uiOutput(ns('meps_table'),role="region","aria-live"="polite")}
  
  tabPanel(title=tags$span(class='tab-title table-tab',"Table"), 
           
           
        tags$figure( # wrap table and footnotes in figure tag
           
           downloadButton508( ns("csv"), 
                              class = 'download-button',
                              label = 'Download table'), 
                              #icon = icon('download')),
           
           uiOutput(ns("table_caption"),inline=T,role="region","aria-live"="polite"),
           #uiOutput(ns('meps_table'),role="region","aria-live"="polite"),
           #dataTableOutput(ns('meps_table')),
           tbl,
           uiOutput(ns("table_footnotes"),role="region","aria-live"="polite")
        )
  )
  
}



#######################################################
###                     SERVER                      ###
#######################################################

tableModule <- function(input, output, session, meps_inputs,pivot=F){
  
  adj <- reactive(meps_inputs()$adj)
  tbl <- reactive(meps_inputs()$tbl)
  inputs <- reactive(meps_inputs()$inputs)
  labels <- reactive(meps_inputs()$labels)
  
  table_caption <- reactive(labels()$caption %>% gsub("<SE>","(standard errors)",.))
  dl_caption <- reactive(labels()$caption %>% gsub(" <SE>","",.))
  
  footnotes <- reactive(labels()$footnotes)
  controlTotals <- reactive(labels()$controlTotals)
  
  
  output$table_caption <- renderUI(tags$caption(table_caption()))
  
  
  formatted_tbl <- reactive({
    denom = adj()$D; digits = adj()$d;
    
    tbl() %>% 
      mutate_at(vars(coef,se),funs(formatNum(./denom,d=digits))) %>%
      mutate(coef = str_pad(coef,width=max(nchar(coef),na.rm=T),side='left')) %>% # padding for DT sort
      mutate(coef = ifelse(grepl("--",coef),"               <a href='#suppress' class='footnote'>--</a>",coef),
              se  = ifelse(se=="--","<a href='#suppress' class='footnote'>--</a>",se)) %>%
      mutate(coef = ifelse(star,paste0(coef,"<a href='#RSE' class='footnote'>*</a>"),coef),
             coef_se = sprintf("%s  <span class='se gray'>(%s)</span>",coef,se))
  }) 
  

# Display table    

  display_tbl <- reactive({ 
   validate(need(nrow(formatted_tbl()) > 0,"Loading..."))

   formatted_tbl()  %>% 
      spread_tbl(stat=ifelse(inputs()$showSEs,"coef_se","coef"), labels=labels()$labels, pivot=pivot) 
  }) 
  
  output$meps_table <- renderUI({
    HTML508table(body = display_tbl(), caption = table_caption())
  })

  
  # Testing checkboxes inside data table
  chkbox = paste(checkboxInput508("hi",label=""))
  
  dt_tbl <- reactive({
    tab <- display_tbl()

    chkcol <- sapply(tab[,1],function(x) paste(checkboxInput508(x,x)))

    tab[,1] <- chkcol

   # print(head(tab))
    
    tab
  })
  
  output$meps_DT <- renderUI({
    HTML508table(body = dt_tbl(), caption = table_caption())
  })
  
  # output$meps_DT <- DT::renderDataTable(
  #   dt_tbl(),escape=F,options=list(paging = FALSE),rownames=F
  #   #isolate(display_tbl()),select="none",escape=F,options=list(paging = FALSE),server=T
  # )

  # proxy = dataTableProxy(session$ns("meps_DT"),session=SESSION)
  # 
  # observe({
  #   replaceData(proxy,display_tbl(),resetPaging=F)
  #   # reloadData(proxy)
  # })
  
  ######################################

  output$table_footnotes <- renderText({
    f_star = footnotes()$star
    f_suppress = footnotes()$suppress
    
    if(f_star != "") f_star = sprintf("<aside id='RSE'>%s</aside>",f_star)
    if(f_suppress != "") f_suppress = sprintf("<aside id='suppress'>%s</aside>",f_suppress)
     
    paste0(c(f_suppress,f_star),collapse="")
  })
  
  outputOptions(output, "table_footnotes", suspendWhenHidden = FALSE)
  outputOptions(output, "meps_table", suspendWhenHidden = FALSE)
  outputOptions(output, "meps_DT", suspendWhenHidden = FALSE)
  
# Download table

  coef_tab <- reactive(formatted_tbl() %>% spread_tbl(stat="coef",labels=labels()$labels,pivot=pivot))
  se_tab   <- reactive(formatted_tbl() %>% spread_tbl(stat="se",  labels=labels()$labels,pivot=pivot))

  output$csv <- downloadHandler(
    filename = function(){ paste('meps-hc-accessed-',Sys.Date(),'.csv',sep='') },
    content = function(file){
      
      write.table(dl_caption(),file,sep=",",row.names=F,col.names=F)
      add.table(coef_tab(),file)
      
      if(!controlTotals()){
        cap <- dl_caption() %>% str_replace(.,word(.,1),word(.,1)%>%tolower)
        se_caption <- paste("Standard errors for",cap)
        add.table(se_caption,file,col.names="")
        add.table(se_tab(),file)
      }
      
      foots <- footnotes() %>% rm_html %>% rm_brks %>% rm_xspc
      foots <- foots[foots!=""]
      if(controlTotals()) foots = c(foots,controlTotals_message)
      
      for(fn in c(" ",foots," ")) add.table(fn,file,col.names=F)
      add.table(rm_html(labels()$source),file,col.names=F)
    })   

}


