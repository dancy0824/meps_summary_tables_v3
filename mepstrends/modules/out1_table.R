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
  
  if(DT){
    tbl <- div(DT::dataTableOutput(ns('meps_DT')),role="region","aria-live"="polite")
    #tbl <- uiOutput(ns('meps_DT'),role="region","aria-live"="polite")
  }else{ tbl <- uiOutput(ns('meps_table'),role="region","aria-live"="polite")}
  
  tabPanel(title=tags$span(class='tab-title table-tab',"Table"), 
       
        tags$figure( # wrap table and footnotes in figure tag
           
           downloadButton508( ns("csv"), 
                              class = 'download-button',
                              label = 'Download table'), 
                              #icon = icon('download')),
           
           uiOutput(ns("table_caption"),inline=T,role="region","aria-live"="polite"),
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

 
# DataTable for Conditions, PMEDs
  
  ## NOTE: can add checkboxes to names for 508 compliance

  ## Save order and selected rows
  v <- reactiveValues(order="2014",dir="desc",rows=NULL)
  
  observe({
    order = input$meps_DT_state$order
    if(!is.null(order)){
      if(length(order)!=0){
         col_num <- order[[1]][[1]]
         col_name <- colnames(dt_tbl())[col_num+1]
         v$order = col_name
         v$dir = order[[1]][[2]]
      }
    }
  })
  
  observe({
    rows_selected = input$meps_DT_rows_selected
    row_names <- dt_tbl()[rows_selected,1]
    v$rows = row_names
  })
  
  dt_tbl <- eventReactive(display_tbl(),{
    tab <- display_tbl() #[1:10,]
    col_names <- v$order
    if(col_names %in% colnames(display_tbl())){
      tab <- tab[order(tab[[col_names]],decreasing=(v$dir=="desc")),]
    }else{
      tab <- tab[order(tab[,2],decreasing=T),]
    }

    tab %>% mutate(selected = (cols %in% v$rows))
  })

  output$meps_DT <- DT::renderDataTable({ 
    tab <- dt_tbl()
    selected <- which(tab$selected)
    tab <- tab %>% select(-selected)
   
    datatable(tab,options(paging=F,stateSave=T),
              rownames=F,escape=F,
              selection = list(mode='multiple',selected=selected,target='row'))
  })
  

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


