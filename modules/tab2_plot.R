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
  
  if(showSEs){
    listy[[i+1]] <- 
      tags$li(
        tags$div(class = "legend-CI"), 
        "95% Confidence Interval"
      )
  }
  
  tagList(listy)
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
           
           uiOutput(ns("caption")),

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
           
           uiOutput(ns("plot_footnote"))
  )
  
}


#######################################################
###                     SERVER                      ###
#######################################################

plotModule <- function(input, output, session, tbl, inputs, adj, labels){
  
  is_trend <- reactive(input$tabs == "trend")
  
  output$plot_footnote   <- renderText(labels()$footnotes$suppress %>% gsub(" -- Estimates","<em>Note:</em> Some estimates",.))
  output$caption  <- renderUI(tags$div(class='caption',labels()$caption))
  
  rows  <- reactive(inputs()$rows)
  rowsX <- reactive(inputs()$rowsX)
  
  plot_data <- reactive({
    D  <- adj()$D
    df <- tbl() %>% mutate(x = rows, grp = cols)
    
    if(is_trend()) df$x = df$Year
    
    df <- df %>%
      mutate(y = coef/D, y_se = se/D) %>%
      select(grp, x, y, y_se) 
    
    if(!is_trend()){
      df <- df  %>%
        mutate(
          x = abbrev(x),
          x = str_wrap(x,20),
          x = factor(x, levels = unique(x)))
    }
    
    df %>%  na.omit
  })
  
  colors <- reactive({
    ncolors = length(unique(plot_data()$grp))
    b_colors <- colorRampPalette(brewer.pal(ncolors,"Dark2"))
    b_colors(ncolors)
  })
  
  
  
  
  
  #######################################
  # GGPLOT
 
  ggplot_data <- reactive({
    out <- plot_data() %>%
      mutate(grp = factor(grp, levels = rev(unique(grp))))
    
    if(!is_trend()){
      out <- out %>%
        mutate(x = factor(x, levels = rev(unique(x))))
    }
    
    out
  })    
  
  plot_obj2 <- reactive({
    ggplot(ggplot_data(),aes(x = x, y = y, fill=grp)) +
      scale_fill_manual(values=colors())
  })
  
  line2 <- reactive({ print("line_graph()...")
    p <- plot_obj2() 

    if(inputs()$showSEs){
      p <- p +
       # geom_errorbar(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),width = 0) +
        geom_ribbon(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),alpha=0.3) 
    }
    
    p + geom_line(aes(col=grp),size = 1) +
      geom_point(aes(col=grp),size = 2) +
      scale_color_manual(values=colors())
    
  })
  
  bar2 <- reactive({  
    p <- plot_obj2() +
      geom_bar(stat = "identity", position = "dodge", colour="white") + 
      scale_x_discrete(drop=FALSE) 
    
    if(inputs()$showSEs){
      p <- p +
        geom_errorbar(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),
                      width = 0, position = position_dodge(width = 0.9))
    }
    
    p + coord_flip() +
      guides(fill = guide_legend(reverse=T))
  })

  format_type <- reactive({
    if(grepl('percent',labels()$caption)) return(percent)
    if(grepl('expend',labels()$caption)) return(dollar)
    return(comma)
  })
  
  gv <- function(){
    
    xlabel = grp_labels[[rowsX()]]
    
    if(is_trend()) gp <- line2() else gp <- bar2()
    gp + theme_minimal(base_size=16) +
      xlab(xlabel) + 
      ylab("") +
    theme(legend.title=element_blank()) +
    scale_y_continuous(labels = format_type())
  }
  
  output$plot <- renderPlotly({
    pp <- gv() + theme(legend.position = "none")

    ggplotly(pp) %>%
      config(collaborate=F,displaylogo=F,
        modeBarButtonsToRemove=c("toImage","lasso2d","pan2d","select2d","zoomIn2d","zoomOut2d","resetScale2d")) %>%
      layout(font=list(family="Arial"))
  })
  
  output$legend <- renderUI({
    if(is_trend()) type = "line" else type = "bar"
    
    tags$ul(class = "test-legend",
            build_legend(unique(plot_data()$grp), colors = rev(colors()), type=type, showSEs=inputs()$showSEs)
            # tags$li(tags$span(class = "block", style='background-color: purple'),"thing one prpl"),
            # tags$li(tags$span(class = "block", style='background-color: red'),"thing two red")
    )
  })
  
  ####################################
  
  outgg <- function(){
    gv() + ggtitle(str_wrap(labels()$caption,60))
  }
  
  output$png <- downloadHandler(
    filename = function(){ paste('meps-hc-',Sys.Date(),'.png',sep='') },
    
    content = function(file) {
      ggsave(file,plot=outgg(),device = "png",width=10,height=6.25)
    })  
 
  
}