#######################################################
###                   FUNCTIONS                     ###
#######################################################

build_legend <- function(names,colors,type="bar"){
  listy = list(); 
  n = length(names)
  for(i in 1:n){
    listy[[i]] =
      tags$li(
        tags$div(class = paste0("legend-",type),
                 style = paste0('background-color: ',colors[i])),
        names[i])
  }
  
  # convert to 2 columns on small screen
  split = ceiling(n/2)
  first_col = tags$div(class = "col-xs-6 col-sm-12",tagList(listy[1:split]))
  if(length(listy)==1) return(first_col)
  
  second_col = tags$div(class = "col-xs-6 col-sm-12",tagList(listy[(split+1):n]))
  return(tagList(first_col,second_col))
}

build_legend(names=c("name1"),colors=c("red"),type="line")


############# Plots ############# 

bsize <- 14

hide.y.axis <- function(gg){
  gg + theme(axis.title.y = element_blank(),
             axis.text.y  = element_blank(),
             axis.ticks.y = element_blank())
}

add_errorbars <- function(gg,...){
  gg + geom_errorbar(aes(ymin=LL,ymax=UL,alpha="95% Confidence Interval"),width=0,...) +
    scale_alpha_manual(name="",values=1)
}

add_points <- function(gg,legend_label,colors,showLegend=T){
  gg + geom_point(aes(col=grp,text=pretty_label),size=2, show.legend=showLegend) +
    scale_color_manual(name=legend_label,values=colors) + 
    expand_limits(y=0)
}

point_graph <- function(dat,showSEs,legend_label,colors,showLegend=T){    
  yr <- dat$x[1]
  n <- length(dat$x)
  jitter = 1:n*(1/(2*n))
  jitter = jitter - mean(jitter)
  dat$x = dat$x + jitter   
  
  p <- ggplot(dat,aes(x = x, y = y)) + 
    theme_minimal(base_size=bsize) +
    scale_x_continuous(breaks=yr) + 
    expand_limits(x=c(min(dat$x)-1,max(dat$x)+1))
  
  if(showSEs) p <- p %>% add_errorbars
  p <- p %>% add_points(legend_label=legend_label,colors=colors,showLegend=showLegend)
  p + guides(color=guide_legend(order=2,reverse=T))
}

    
line_graph <- function(dat,showSEs,legend_label,colors,showLegend=T){  
  brks = waiver()
  yrs <- min(dat$x):max(dat$x)
  if(length(yrs) <= 3) brks = yrs
  
  p <- ggplot(dat,aes(x = x, y = y, fill=grp)) + 
    theme_minimal(base_size=bsize) + 
    scale_x_continuous(breaks=brks) 
  
  if(showSEs) p <- p+geom_ribbon(aes(ymin=LL,ymax=UL),alpha=0.3,show.legend=F)
  
  p <- p %>% add_points(legend_label=legend_label,colors=colors,showLegend=showLegend)
  p + geom_line(aes(col=grp),size=1, show.legend = showLegend) +
    guides(colour=guide_legend(reverse=T),fill=guide_legend(reverse=T))+
    scale_fill_manual(name=legend_label,values=colors)
}


bar_graph <- function(dat,showSEs,legend_label,colors,showLegend=T,hide_y_axis=F,br="\n"){ 
  
  dat <-dat %>%
    mutate(
      x = abbrev(x),
      x = meps_wrap(x,br),
      x = factor(x, levels = rev(unique(x))))
  
  p <- ggplot(dat,aes(x = x, y = y, fill=grp,text=pretty_label)) + 
    theme_minimal(base_size=bsize) +
    geom_bar(stat="identity",position="dodge",colour="white",show.legend=showLegend) + 
    scale_fill_manual(name=legend_label,values=colors,drop=FALSE) +
    scale_x_discrete(drop=FALSE) 
  
  if(showSEs) p <- p %>% add_errorbars(position=position_dodge(width = 0.9))
  if(hide_y_axis) p <- p %>% hide.y.axis  
  
  p + coord_flip() +
    guides(color=guide_legend(order=2),fill=guide_legend(reverse=T,order=1))  
}

gv <- function(graph_type,...,hide_y_axis=F,br="<br>"){
  if(graph_type=="line") return(line_graph(...))
  if(graph_type=="bar") return(bar_graph(...,hide_y_axis=hide_y_axis,br=br))
  if(graph_type=="point") return(point_graph(...))
}

#######################################################
###                      UI                         ###
#######################################################

plotUI<- function(id){
  ns <- NS(id)
  
  tabPanel(title=tags$span(class='tab-title plot-tab',"Plot"), 
           
           downloadButton508( ns("png"), 
                              class = 'download-button',
                              label = 'Download Plot'),
                              #icon=icon('download')),
           
           uiOutput(ns("plot_caption"),inline=T,role="region","aria-live"="polite"),
           uiOutput(ns("sub_caption"),role="region","aria-live"="polite"),

           fluidRow(
             column(width = 9,
                    div(class="square",div(class="content",
                      plotlyOutput(ns("plot"),height="100%",width="100%")))
             ),
             
             column(width = 3,
                    div(uiOutput(ns("legend")))
             )

           ),
           
           ## temporary -- for debugging download plot

           # div(class = "square",
           #     div(class = "content",
           #         plotOutput(ns("ggplot")))),

           
           uiOutput(ns("plot_footnote"),role="region","aria-live"="polite"),
           uiOutput(ns("sr_table"),class="usa-sr-only",role="region","aria-live"="polite")
  )
}

#######################################################
###                     SERVER                      ###
#######################################################

plotModule <- function(input, output, session, meps_inputs){
  
  adj <- reactive(meps_inputs()$adj)
  tbl <- reactive(meps_inputs()$tbl)
  inputs <- reactive(meps_inputs()$inputs)
  labels <- reactive(meps_inputs()$labels)
  
  cols <- reactive(inputs()$cols)
  rows <- reactive(inputs()$rows)
  rowsX <- reactive(inputs()$rowsX)
  is_trend <- reactive(inputs()$is_trend)
 
  legend_label <- reactive(grp_labels[[cols()]])
  grpLabel <- reactive(grp_labels[[rowsX()]])
 
  graph_type <- reactive({
    if(is_trend()){
      if(length(unique(tbl()$Year))==1) return("point")
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
  meps_source <- reactive(labels()$source %>% rm_html) #gsub("<.*?>","",.))
  
  output$plot_footnote <- renderText(labels()$footnotes$suppress %>% gsub(" -- Estimates","<em>Note:</em> Some estimates",.))
  output$plot_caption <- renderUI(tags$caption(caption()))
  output$sub_caption <- renderUI(tags$div(class="sub-caption",sub_caption()))

  # outputOptions(output, "plot_footnote", suspendWhenHidden = FALSE)
  # outputOptions(output, "plot_caption", suspendWhenHidden = FALSE)
  # outputOptions(output, "sub_caption", suspendWhenHidden = FALSE)
  
  ############# Data ############# 
  
  pre_data <- reactive({
    D  <- adj()$D
    d <- adj()$d
    
    df <- tbl() %>% mutate(x = rows, grp = cols)
    
    if(is_trend()) df$x = df$Year
    
    df %>%
      mutate(y = coef/D, y_se = se/D) %>%
      mutate(
        LL = y - 1.96*y_se,
        UL = y + 1.96*y_se,
        grp = abbrev(grp),
        pretty_y = formatNum(y,d=d),
        pretty_LL = formatNum(LL,d=d),
        pretty_UL = formatNum(UL,d=d),
        pretty_lab = sprintf("%s: %s",grp,pretty_y),
        pretty_CI = sprintf("%s: %s (%s, %s)",grp,pretty_y,pretty_LL,pretty_UL))
  }) 
  
  ## Output CI table for screen readers
  hidden_tbl <- reactive({
    
    dat <- pre_data() %>% 
      select(grp,x,pretty_y,pretty_LL,pretty_UL) %>%
      rename_cols(list(
        grp=legend_label(),
        x=grpLabel(),
        pretty_y = "Point Estimate",
        pretty_LL="Lower 95% Confidence Limit",
        pretty_UL="Upper 95% Confidence Limit")) 
    
    dat[,colnames(dat)!="(none)"]
  })
  
  output$sr_table <- renderUI(HTML508table(body = hidden_tbl()))
  
  plot_data <- reactive({
    validate(need(nrow(pre_data()) > 0,"Loading..."))
    dat <- pre_data() %>%
      mutate(grp = meps_wrap(grp),
            grp = factor(grp, levels = rev(unique(grp))))
    
    if(inputs()$showSEs) dat <- dat %>% mutate(pretty_label = pretty_CI)
    else  dat <- dat %>% mutate(pretty_label = pretty_lab)
    dat %>% mutate(pretty_label = gsub("Total: ","",pretty_label))
  })
  
  ############# Formats and Functions ############# 
  
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

  showLegend <- reactive({
    if(cols()=='ind') return(FALSE)
    return(NA)
  })

############# Dispay (PLOTLY) ############# 
  
  output$plot <- renderPlotly({
    
    gp <- gv(graph_type=graph_type(),
             dat=plot_data(),
             showSEs=inputs()$showSEs,
             legend_label=legend_label(),
             colors=colors(),
             showLegend=showLegend(),
             hide_y_axis=(rows()=='ind'),br="<br>") 
 
    gp <- gp + ylab("") + xlab(grpLabel()) + scale_y_continuous(labels = format_type())

    side_labels <- gp$data$x %>% as.character
    max_length <- side_labels %>% nchar %>% max
    br_length <- str_split(side_labels,"<br>") %>% lapply(nchar) %>% unlist %>% max
    marg = -12 + 7*(max_length-br_length)

    pp <- gp + theme(legend.position = "none",plot.margin=margin(l=-marg))
 
    ggplotly(pp,tooltip=c("text"),hoverinfo="text") %>%
      config(collaborate=F,displaylogo=F,
        modeBarButtonsToRemove=c("toImage","lasso2d","pan2d","select2d","zoomIn2d","zoomOut2d","resetScale2d")) %>%
      layout(font=list(family="Arial"),margin=list(r=30,t=20))
  })
  
  
  output$legend <- renderUI({
    if(cols()=="ind" & graph_type()=="line") return('')
    
    if(inputs()$showSEs & graph_type() != "line"){
      li_CI <- tags$li(tags$div(class = "legend-CI"), "95% Confidence Interval")  
    }else{li_CI = ""}
    
    if(cols()=="ind"){
      li_main = ""; label = "";
    }else{
      li_main = build_legend(unique(plot_data()$grp), colors = rev(colors()), type=graph_type())  
      label = legend_label()
    }

    tagList(
      tags$label(label),
      tags$ul(class = "test-legend",li_main,li_CI)
    )
  })

  # outputOptions(output, "plot", suspendWhenHidden = FALSE)
  # outputOptions(output, "legend", suspendWhenHidden = FALSE)

  ############# Download (GGPLOT) ############# 

  outgg <- function(){
    gp <- gv(graph_type=graph_type(),
             dat=plot_data(),
             showSEs=inputs()$showSEs,
             legend_label=str_wrap(legend_label(),35),
             colors=colors(),
             showLegend=showLegend(),
             hide_y_axis=(rows()=='ind'),br="\n") 
    
    gp <- gp + ylab("") + xlab(grpLabel()) +
      scale_y_continuous(labels = format_type()) +
      labs(title = str_wrap(caption(),60),
           subtitle = str_wrap(sub_caption(),60),
           caption = str_wrap(meps_source(),100)) +
      theme(plot.caption = element_text(size = 8),
            plot.margin = margin(t=10,r=20,l=10,b=10),
            legend.text = element_text(size=11))

    nlevels = unique(gp$data$grp) %>% length
    if(nlevels <= 6) gp <- gp + theme(legend.key.size = unit(2.5,'lines'))
    gp
  }
  
  output$ggplot <- renderPlot({
    outgg()
  })
  
  output$png <- downloadHandler(
    filename = function(){ paste('meps-hc-',Sys.Date(),'.png',sep='') },
    content = function(file) {
      ggsave(file,plot=outgg(),device = "png",width=10,height=6.25)
    })  

}