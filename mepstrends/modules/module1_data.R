#######################################################
###                   FUNCTIONS                     ###
#######################################################

decorate_tbl <- function(tbl,var_name, se_name=paste0(var_name,"_se"),n_name=NULL){
  
  if(is.null(n_name)) n_name <- ifelse(var_name %in% c("meanEXP","medEXP"),"n_exp","n")
  is.pct = (var_name %>% startsWith("pct")) 
  
  out <- tbl %>%
    mutate_(coef=var_name,se=se_name,sample_size=n_name) %>%
    mutate(RSE = se/coef,
           special_pct = (is.pct & (coef < 0.1) & (RSE < (0.1/coef-1)/1.96)),
           suppress = (sample_size < 60 | RSE > 0.5),
           suppress = replace(suppress, special_pct,FALSE),
           star = (RSE > 0.3 & !suppress)) %>%
    mutate(coef = replace(coef,suppress,NA),
           se   = replace(se,suppress,NA)) 
  
  out <- out %>% select(Year, cols, rows, coef, se, suppress, star)
  
  return(out)
}

adjust_totals <- function(df,var){
  lv <- df[,var]
  if(any(startsWith(lv,"All"))){
    df <- df %>% filter_(sprintf('%s!="Total"',var))
  }
  
  lv <- df[,var]
  if(var != 'ind'){
    df[,var] = replace(lv,lv=="Total","All persons")
  }
  
  df
}

#######################################################
###                       UI                        ###
#######################################################

dataViewInput <- function(id){
  ns <- NS(id)
  radioButtons508(ns("tabs"), "Select data view:", inline=T, 
                  class="em-fieldset controls-trend skinny-display",
                  choices = c("Trends over time"="trend",
                              "Cross-sectional"="cross"))
}

standardErrorInput <- function(id){
  ns <- NS(id)
  div(
    checkboxInput508(ns("showSEs"),label="Show standard errors",inline=T),
    uiOutput(ns("CInote"),class="control-message")
  )
}

yearInput <- function(id,min,max){
  ns <- NS(id)
  tags$fieldset(
    tags$legend("Select years",class = 'usa-sr-only'),
    div(class = "flex-parent",
      div(class="flex-child-fill year-start", 
          selectInput508(ns("year_start"),label="Year:",choices=max:min,selected=min)),
      div(class="flex-child-fill year-main", selectInput508(ns("year"),label= "to:",choices=max:min,selected=max)) 
    )
  )
}

rcInput <- function(id, type="cols", choices=NULL, selected=choices[1], label="Group by:", hide_label=FALSE, class=""){
  ns <- NS(id)
  
  if(hide_label) labelClass = "usa-sr-only" else labelClass = ""
  
  tags$fieldset(class = class,
                tags$label(label, `for` = ns(type), class=labelClass),
                div(class="btn-group flex-parent", role="group",
                    div(class="btn-group flex-child-fill", role="group", selectInput508(ns(type),choices=choices,selected)),
                    div(class="btn-group flex-child", grpInput( ns(type), choices=choices))
                )
  )
}

switchUI <- function(id,class=""){
  ns <- NS(id)
  actionButton508(ns("rc_switch"),
        label = "Switch rows/columns",
        class = "usa-button")
}


#######################################################
###                     SERVER                      ###
#######################################################

dataModule <- function(input, output, session, df, stat, adj, exclude_initial,...){

  ##################
  ##    INPUTS    ##
  ##################
  
  is_trend <- reactive(input$tabs == "trend")
  
  years <- reactive({
    if(!is_trend()) return(input$year)
    return(input$year_start:input$year)
  })
  
  cols = reactive({
    if(input$cols==input$rows & !is_trend()) return("ind")
    return(input$cols)
  }) 
  
  rows = reactive({
    if(is_trend()) return("ind")
    return(input$rows)
  })
  
  rowsX = reactive({
    if(is_trend()) return("Year")
    if(!is_trend() & input$rows == "ind") return("Year")
    return(input$rows)
  }) 
  
  ##################
  ##     DATA     ##
  ##################  
  
  # Standard Errors and Control Totals
  controlTotals <- reactive({
    tot_vars = c("Year","ind","agegrps","race","sex","poverty","region")
    (stat()=="totPOP") &
      (rows() %in% tot_vars) &
      (input$cols %in% tot_vars)
  })
  
  showSEs <- reactive({
    input$showSEs & !controlTotals()
  })
  
  output$CInote <- renderUI({
    if(controlTotals() & input$showSEs) return(controlTotals_message) else return(HTML("<br>"))
  })
  
# Initialize reactiveValues
  all1 <- df %>% select(levels1,grp1) %>% rename(levels=levels1,grp=grp1)
  all2 <- df %>% select(levels2,grp2) %>% rename(levels=levels2,grp=grp2)
  
  initial_values <- bind_rows(all1,all2) %>% 
    filter(!levels %in% exclude_initial) %>% 
    distinct %>% unstack 

  values = reactiveValues()
  
  observe({
    isolate({
      for(i in names(initial_values)) values[[i]] = initial_values[[i]] %>% gsub(" ","_",.)
    })
  })

  col_levels <- callModule(groupBy,"cols",var = reactive(input$cols),df = subgrp_tbl,values = values,initial=initial_values,...)
  row_levels <- callModule(groupBy,"rows",var = reactive(rows()), df = subgrp_tbl,values = values,initial=initial_values,...)
  
  observeEvent(input$rc_switch,{
    in1 <- input$cols; in2 <- input$rows;
    updateSelectInput(session,inputId="cols",selected=in2)
    updateSelectInput(session,inputId="rows",selected=in1)
  })

  select_years <- reactive({                              
    df %>% filter(Year %in% years())
  })
  
  subgrp_tbl <- reactive({                                 
    rows <- rows()
    cols <- cols()
    
    tbl <- select_years()
    
    ## Add marginal totals ##
    rows_tot = c(rows,"ind")
    cols_tot = c(cols,"ind")
    
    tab <- tbl %>% filter(grp1 %in% rows_tot, grp2 %in% cols_tot)
    tab[,rows] = tab$levels1
    tab[,cols] = tab$levels2
    tab <- tab %>% select(-grp1,-grp2,-levels1,-levels2)

    rev <- tbl %>% filter(grp1 %in% cols_tot, grp2 %in% rows_tot)
    rev[,rows] = rev$levels2
    rev[,cols] = rev$levels1
    rev <- rev %>% select(-grp1,-grp2,-levels1,-levels2)

    all <- bind_rows(tab,rev) %>% distinct

    all %>% adjust_totals(rows) %>% adjust_totals(cols)
  })
  
  select_levels <- reactive({                               
    subgrp_tbl() %>%
      mutate_(cols = input$cols, rows=rows()) %>% # for trend, rows() = 'ind'
      filter(cols %in% col_levels(), rows %in% row_levels()) %>%
      mutate_(rows = rowsX())  ## maybe change this so row_levels = years if row is year?
  })
  
  
  ####################
  ##     LABELS     ##
  ####################  
  stat_label <- reactive({
    paste0(stat_labels[[stat()]], adj()$label)
  })

  dat_labels <- reactive({
    list(rows = grp_labels[[rowsX()]], Total=stat_label())
  })

  year_caption <- reactive({
    if(input$tabs=="cross") return(input$year)
    c(min(years()),max(years())) %>% unique %>% paste0(collapse="-")
  })

  se_caption <- reactive({
    if(!showSEs()) return("")
    return(" <SE>")
  })

  caption <- reactive({
    get_caption(stat_label(),rows(),cols(),se_caption(),year_caption())
  })

  source_text <- reactive( # also used in download table
    paste("<b>Source:</b>",sprintf("%s, %s, %s, %s.",CFACT,AHRQ,MEPS,year_caption()))
  )

  footnotes <- reactive({
    tab <- decorated_tbl()
    footnotes = list()
    footnotes$suppress <- ifelse(any(tab$suppress,na.rm=T),suppressed_message,"")
    footnotes$star     <- ifelse(any(tab$star,na.rm=T),rse_message,"")
    return(footnotes)
  })

  ####################
  ##     OUTPUT     ##
  ####################  

  inputs <- reactive({
    list(years=years(),
         cols=cols(),
         rows=rows(),
         rowsX=rowsX(),
         is_trend=is_trend(),
         showSEs=showSEs(),
         stat=stat(),
         controlTotals=controlTotals())
  }) 
  
  labels <- reactive({
    list(labels = dat_labels(),
         source = source_text(),
         caption = caption(),
         footnotes = footnotes(),
         controlTotals=controlTotals())
  })
  
  decorated_tbl <- reactive({
    select_levels() %>% decorate_tbl(var_name=stat())
  })
  
  outlist <- reactive(
    list(adj=adj(),inputs=inputs(),tbl=decorated_tbl(),labels=labels())
  ) %>% debounce(350)

 # return(reactive(list(inputs=inputs(), tbl=decorated_tbl())))
  return(outlist)
}



