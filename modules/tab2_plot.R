#######################################################
###                   FUNCTIONS                     ###
#######################################################


build_legend <- function(names,colors,type="bar",showSEs=F){
  
  listy = list()
  for(i in 1:length(names)){
    listy[[i]] =
      tags$li(
        tags$div(class = paste0("legend-",type),
                 style = paste0('background-color: ',colors[i])),
        names[i]
      )
  }
  
  if(showSEs & type != "line"){
    listy[[i+1]] <- 
      tags$li(
        tags$div(class = "legend-CI"), 
        "95% Confidence Interval"
      )
  }
  tagList(listy)
}

point_graph <- function(dat,showSEs,legend_title,colors){
  brk = dat$x[1]
  
  dat$one = 1
  n = length(dat$x)
  jitter = 1:n*(1/(2*n))
  jitter = jitter - mean(jitter)
  
  dat$x = dat$x + jitter   
  
  p <- ggplot(dat,aes(x = x, y = y, fill=grp)) +
    scale_fill_manual(name=legend_title,values=colors)+
    scale_x_continuous(breaks=brk)
  
  if(showSEs){p <- p + geom_errorbar(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),width = 0) #+ 
    #  geom_line(aes(x=one,y=one,color="95% Confidence Interval"))+
    #  scale_color_manual(values=c("95% Confidence Interval" = 'black'))
  }
  
  p + geom_point(aes(col=grp),size = 2) +
    scale_color_manual(name=legend_title,values=colors) + 
    expand_limits(y=0,x=c(min(dat$x)-1,max(dat$x)+1)) + 
    theme_minimal(base_size=16) 
}


line_graph <- function(dat,showSEs,legend_title,colors,showLeg=TRUE){
  brks = waiver()
  yrs <- min(dat$x):max(dat$x)
  if(length(yrs) <= 3) brks = yrs
  
  p <- ggplot(dat,aes(x = x, y = y, fill=grp)) +
    scale_fill_manual(name=legend_title,values=colors)+
    scale_x_continuous(breaks=brks)
  
  if(showSEs){p <- p + geom_ribbon(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),alpha=0.3)}
  
  p + geom_line(aes(col=grp),size = 1,show.legend = showLeg) +
    geom_point(aes(col=grp),size = 2,show.legend = showLeg) +
    scale_color_manual(name=legend_title,values=colors) + 
    expand_limits(y=0) + 
    theme_minimal(base_size=16) +
    guides(colour = guide_legend(reverse=T),
           fill = guide_legend(reverse=T))
}

bar_graph <- function(dat,showSEs,legend_title,colors,showLeg=TRUE,hide.y.axis=FALSE,br="\n"){

  dat <- dat %>%
    mutate(
      one = 1,
      x = abbrev(x),
      x = meps_wrap(x,br),
      x = factor(x, levels = rev(unique(x))))
  
  #save(dat,file="plotdata.Rdata")
  
  p <- ggplot(dat,aes(x = x, y = y, fill=grp)) +
    scale_fill_manual(name=legend_title, values=colors,drop=FALSE) +
    geom_bar(stat = "identity", position = "dodge", colour="white",show.legend = showLeg) + 
    scale_x_discrete(drop=FALSE) + 
    theme_minimal(base_size=16)
  
  if(showSEs){
    p <- p +
      geom_errorbar(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),
                    width = 0, position = position_dodge(width = 0.9)) + 
      geom_line(aes(color="95% Confidence Interval"))+
      scale_color_manual(name="",values=c("95% Confidence Interval" = 'black'))
  }
  
  if(hide.y.axis){ # Remove x axis on bar charts if year
    p <- p +
      theme(axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  p + coord_flip() +
    guides(fill = guide_legend(reverse=T,order=1),
           color = guide_legend(order=2))  
}

meps_graph <- function(graph_type,...,showLeg=TRUE,hide.y.axis=FALSE,br="\n"){
  if(graph_type=="line") return(line_graph(showLeg=showLeg,...))
  if(graph_type=="bar") return(bar_graph(br=br,showLeg=showLeg, hide.y.axis=hide.y.axis,...))
  return(point_graph(...))
}


#######################################################
###                      UI                         ###
#######################################################


plotUI<- function(id){
  ns <- NS(id)
  
  tabPanel(title="Plot",icon=icon("bar-chart"), 
           
           downloadButton508( ns("png"), 
                              class = 'download-button',
                              label = 'Download Plot', 
                              icon=icon('download')),
           
           uiOutput(ns("plot_caption"),inline=T,role="region","aria-live"="polite"),
           uiOutput(ns("sub_caption"),role="region","aria-live"="polite"),

           fluidRow(
             column(width = 9,
                    div(class="square",
                        div(class="content",
                            plotlyOutput(ns("plot"),height="100%",width="100%")
                        )
                    )
             ),
             
             column(width = 3,
                    div(
                      uiOutput(ns("legend"))
                    )
             )

           ),
           
           uiOutput(ns("plot_footnote"),role="region","aria-live"="polite")
  )
  
}


#######################################################
###                     SERVER                      ###
#######################################################

plotModule <- function(input, output, session, tbl, inputs, adj, labels){
  
  cols <- reactive(inputs()$cols)
  rows <- reactive(inputs()$rows)
  rowsX <- reactive(inputs()$rowsX)
 
  legend_label <- reactive(grp_labels[[inputs()$cols]])
  
  is_trend <- reactive(input$tabs == "trend")
  
  graph_type <- reactive({
    if(input$tabs == "trend"){
      if( length(unique(tbl()$Year))==1) return("point")
      return("line")
    }
    if(rows() == 'ind' & cols() == 'ind') return("point")
    return("bar")
  })

  sub_caption <- reactive({
    if(graph_type()=="line" & grepl("<SE>",labels()$caption)) 
      return("Shading indicates 95% confidence interval")
    return("")
  })
  
  caption <- reactive(labels()$caption %>% gsub(" <SE>","",.))
  meps_source <- reactive(labels()$source %>% gsub("<.*?>","",.))
  
  output$plot_footnote <- renderText(labels()$footnotes$suppress %>% gsub(" -- Estimates","<em>Note:</em> Some estimates",.))
  output$plot_caption  <- renderUI(tags$caption(caption()))
  output$sub_caption <- renderUI(tags$div(class="sub-caption",sub_caption()))

  ############# Data ############# 
  
  plot_data <- reactive({
    D  <- adj()$D
    df <- tbl() %>% mutate(x = rows, grp = cols)
    
    if(is_trend()) df$x = df$Year
    
    df %>%
      mutate(y = coef/D, y_se = se/D) %>%
      select(grp, x, y, y_se)  %>% 
      mutate(
        grp = abbrev(grp),
        grp = meps_wrap(grp),
        #grp = str_wrap(grp,16),
        grp = factor(grp, levels = rev(unique(grp))))
  })
  
  ############# Formats ############# 
  
  colors <- reactive({
    ncolors = length(unique(plot_data()$grp))
    b_colors <- colorRampPalette(brewer.pal(max(ncolors,3),"Dark2"))
    b_colors(ncolors)
  })
  
  format_type <- reactive({
    if(grepl('percent',caption())) return(percent)
    if(grepl('expend',caption())) return(dollar)
    return(comma)
  })
  
  # This function needs to be inside server, so it can capture reactives
  # (also, reactive won't work with ggplot/ggplotly(?) so need a function)
  gv <- function(br){
    
    print(rows())
    print(rowsX())
    print(cols())
    
    gp <- meps_graph(
      graph_type(),
      dat = plot_data(),
      showSEs=inputs()$showSEs,
      legend_title=legend_label(),
      showLeg = (cols()!='ind'),
      hide.y.axis = (rows()=='ind'),colors=colors(),br=br)
    
    
    gp + ylab("") + xlab(grp_labels[[rowsX()]]) + 
      scale_y_continuous(labels = format_type()) 
  }

############# Dispay (PLOTLY) ############# 

  output$plot <- renderPlotly({
    
    gp <- gv(br="<br>")

    side_labels <- gp$data$x %>% as.character
    max_length <- side_labels %>% nchar %>% max
    br_length <- str_split(side_labels,"<br>") %>% lapply(nchar) %>% unlist %>% max
    marg = -12 + 7*(max_length-br_length)

    pp <- gp + theme( legend.position = "none",
                       plot.margin = margin(l = -marg))
 
    ggplotly(pp) %>%
      config(collaborate=F,displaylogo=F,
        modeBarButtonsToRemove=c("toImage","lasso2d","pan2d","select2d","zoomIn2d","zoomOut2d","resetScale2d")) %>%
      layout(font=list(family="Arial"),
             margin=list(r=30,t=20))
  })
  
  output$legend <- renderUI({
  
    if(cols()=="ind") return("")
    
    tagList(
      tags$label(legend_label()),
      tags$ul(class = "test-legend",
            build_legend(unique(plot_data()$grp), colors = rev(colors()), 
                         type=graph_type(), showSEs=inputs()$showSEs)
            # tags$li(tags$span(class = "block", style='background-color: purple'),"thing one prpl"),
            # tags$li(tags$span(class = "block", style='background-color: red'),"thing two red")
      )
    )
  })
  
  ############# Download (GGPLOT) ############# 

  outgg <- function(){
    gp <- gv(br="\n") + 
      labs(title = str_wrap(caption(),60),
           subtitle = str_wrap(sub_caption(),60),
           caption = str_wrap(meps_source(),100)) +
      theme(plot.caption = element_text(size = 10),
            plot.margin = margin(t=10,r=20,l=10,b=10),
            legend.text = element_text(size=11))
    
    nlevels = unique(gp$data$grp) %>% length
    
    if(nlevels <= 6) gp <- gp + theme(legend.key.size = unit(2.5,'lines'))
    
    gp
  }
  
  output$png <- downloadHandler(
    filename = function(){ paste('meps-hc-',Sys.Date(),'.png',sep='') },
    content = function(file) {
      ggsave(file,plot=outgg(),device = "png",width=10,height=6.25)
    })  

}