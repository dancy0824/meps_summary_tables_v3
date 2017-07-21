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
  
  if(showSEs & type == "bar"){
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
           
           uiOutput(ns("plot_caption"),inline=T),
           uiOutput(ns("sub_caption")),

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
  
  rowsX <- reactive(inputs()$rowsX)
  is_trend <- reactive(input$tabs == "trend")
  
  sub_caption <- reactive({
    if(!is_trend() | !grepl("<SE>",labels()$caption)) return("")
    return("Shading indicates 95% confidence interval")
  })
  
  caption <- reactive(labels()$caption %>% gsub(" <SE>","",.))
  
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
      mutate(grp = factor(grp, levels = rev(unique(grp))))
  })
  
  ############# Formats ############# 
  
  colors <- reactive({
    ncolors = length(unique(plot_data()$grp))
    b_colors <- colorRampPalette(brewer.pal(ncolors,"Dark2"))
    b_colors(ncolors)
  })
  
  format_type <- reactive({
    if(grepl('percent',caption())) return(percent)
    if(grepl('expend',caption())) return(dollar)
    return(comma)
  })

  
  ############# Plots ############# 
 
  line <- reactive({ 
    p <- ggplot(plot_data(),aes(x = x, y = y, fill=grp)) +
      scale_fill_manual(values=colors())

    if(inputs()$showSEs){
      p <- p + geom_ribbon(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),alpha=0.3) 
    }
    
    p + geom_line(aes(col=grp),size = 1) +
      geom_point(aes(col=grp),size = 2) +
      scale_color_manual(values=colors()) + 
      expand_limits(y=0)
  })
  
  bar <- reactive({  
    dat <- plot_data() %>%
      mutate(
        one = 1,
        x = abbrev(x),
        x = str_wrap(x,20),
        x = factor(x, levels = unique(x)),
        x = factor(x, levels = rev(unique(x))))
    
    p <- ggplot(dat,aes(x = x, y = y, fill=grp)) +
      scale_fill_manual(values=colors(),drop=FALSE) +
      geom_bar(stat = "identity", position = "dodge", colour="white") + 
      scale_x_discrete(drop=FALSE) 
    
    if(inputs()$showSEs){
      p <- p +
        geom_errorbar(aes(ymin = y-1.96*y_se, ymax = y+1.96*y_se),
                      width = 0, position = position_dodge(width = 0.9)) + 
        geom_line(aes(x=one,y=one,color="95% Confidence Interval"))+
        scale_color_manual(values=c("95% Confidence Interval" = 'black'))
    }
    
    p + coord_flip() +
      guides(fill = guide_legend(reverse=T,order=1),
             color = guide_legend(order=2))
  })

  
  ############# Dispay (PLOTLY) ############# 
  
  gv <- function(){
    xlabel = grp_labels[[rowsX()]]
    
    if(is_trend()) gp <- line() else gp <- bar()
    
    gp + theme_minimal(base_size=16) + xlab(xlabel) + ylab("") +
      theme(legend.title=element_blank()) +
      scale_y_continuous(labels = format_type()) 
  }
  
  output$plot <- renderPlotly({
    pp <- gv() + theme(legend.position = "none")

    ggplotly(pp) %>%
      config(collaborate=F,displaylogo=F,
        modeBarButtonsToRemove=c("toImage","lasso2d","pan2d","select2d","zoomIn2d","zoomOut2d","resetScale2d")) %>%
      layout(font=list(family="Arial"),
             margin=list(r=30,t=20))
  })
  
  output$legend <- renderUI({
    if(is_trend()) type = "line" else type = "bar"
    
    tags$ul(class = "test-legend",
            build_legend(unique(plot_data()$grp), colors = rev(colors()), type=type, showSEs=inputs()$showSEs)
            # tags$li(tags$span(class = "block", style='background-color: purple'),"thing one prpl"),
            # tags$li(tags$span(class = "block", style='background-color: red'),"thing two red")
    )
  })
  
  ############# Download (GGPLOT) ############# 
  
  outgg <- function(){
    gv() + labs(title = str_wrap(caption(),60),
                subtitle = str_wrap(sub_caption(),60))
  }
  
  output$png <- downloadHandler(
    filename = function(){ paste('meps-hc-',Sys.Date(),'.png',sep='') },
    content = function(file) {
      ggsave(file,plot=outgg(),device = "png",width=10,height=6.25)
    })  
 
  
}