#######################################################
###              Select Levels Handler              ###
#######################################################


#######################################################
###                   FUNCTIONS                     ###
#######################################################


isOpen <- function(input,output){
  sprintf("$('#%s').on('shown.bs.dropdown', function() {
          var is_open = 'true';
          Shiny.onInputChange('%s', is_open);
          console.log('Dropdown menu open');
});",input,output)
}

isClosed <- function(input,output){
  sprintf("$('#%s').on('hidden.bs.dropdown', function() {
          var is_open = 'false';
          Shiny.onInputChange('%s', is_open);
          console.log('Dropdown menu cloased');
});",input,output)
}

grpInput <- function(id,choices){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      sprintf("input['%s'] != 'ind'", id), 
      dropdown508(ns("drop"),label='Select Levels',
                  checkboxGroupInput508(ns("levels"), choices = c("(none)")), 
                  actionButton508(ns("reset"),"Reset",usaStyle ="outline")
                  )
    ),
    tags$script(
      isOpen(ns("drop"),ns("drop_open")),
      isClosed(ns("drop"),ns("drop_open"))
    )
  )
}


#######################################################
###                     SERVER                      ###
#######################################################


groupBy <- function(input, output, session, df, var, values, exclude_choices=NULL, initial_values=NULL){

  
# Available choices based on data table, filtered by subgroup (and year)
#  - need to convert spaces to underscores for html accessibility
  
  choices <- reactive({ 
    options <- df()[,var()] %>% unique
    options <- options[!options %in% exclude_choices]
    options %>% gsub(" ","_",.) %>% setNames(options) 
  })
  
  # initial <- reactive({
  #   ch <- choices()
  #   ch[!names(ch) %in% exclude_initial]
  # })
  
  
# Selected values based on cached selections if available, otherwise initial choices
  
  select <- reactive({  
    selected <- values[[var()]]
    selected <- selected[selected %in% choices()]
   # ch <- initial() # need this here so reactive gets triggered
    # print(selected)
    # 
    # if(length(selected)!=0) return(selected)
    # return(ch)
  })

  
# Updates based on whether dropdown is open (see custom javascript in ui module)
#  - need to catch as false if value isn't available yet (I don't know how to initialize a value in js)
  
  drop_open <- reactive({ 
    is_open = input$drop_open
    if(is.null(is_open)) return(FALSE)
    return(is_open == "true")
  })
  

# Update choices and selections only if checkboxgroup is hidden  
  
  observeEvent(select(),{
    if(!drop_open()){                        
      updateCheckboxGroupInput508(
        session, 
        inputId = "levels",
        choices = choices(),
        selected = select())
    }
  })
  
  
# Cache selected values only from active dropdown
  
  observeEvent(input$levels,{
    if(drop_open()){ 
      current  <- values[[var()]]
      shown    <- choices()
      preserve <- current[!current%in%shown]
      
      print(current)
      print(shown)
      print(preserve)
      
      cache <- c(preserve,input$levels)

      # new_selections <- input$levels[]
      
      #values[[var()]] = input$levels
      
      values[[var()]] <- cache
    }
  })
  
  
# Reset levels in active dropdown when reset button is clicked
  
  observeEvent(input$reset, {
    if(drop_open()){
      updateCheckboxGroupInput508(
        session,
        inputId = "levels",
        choices = choices(),
        selected = initial_values[[var()]] %>% gsub(" ","_",.))
    }
  })
  
  
# Return current seleted values (after caching and updating to current values)
  
  select_clean <- reactive({
    select() %>% gsub("_"," ",.)
  })
  
  return(select_clean)
  
}