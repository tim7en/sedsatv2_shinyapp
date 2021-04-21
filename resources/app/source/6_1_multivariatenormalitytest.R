source ("./source/MultivariateTest.R")
multivariate_ui <- function (id){
        ns <- NS(id)
        tagList(
          fluidPage(
            fluidRow(
        
        sidebarLayout(
          sidebarPanel(
            uiOutput (ns('shapiroPvalue')),
            uiOutput (ns('elementDrops')),
            uiOutput (ns('selectTarget')),
            actionButton(ns("applyMFA"), "Apply")
          ),
          mainPanel (
            box(
              title = "MFA", status = "success", height =
                "auto", solidHeader = T, width = "auto",
              use_busy_spinner(spin = "pixel", color = "#1230bf", margins = c(10, 10), spin_id = NULL, height = "250px",
                               width = "250px", position = "bottom-right"),
              shinycssloaders::withSpinner(DTOutput(ns ('resultoutput'))), style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
            )
          )
        )
      )
    )
  )
}

multivariate_server <- function (input, output, session, corrected_function, target_function, target_input) {
  
  # option to pick columns that will not be used for correction
  output$elementDrops <- renderUI({
    ns <- session$ns
    selectInput(ns("elementDrops"), "Select columns to remove from MFA", choices = colnames(target_input()[,-c(1,2)]), multiple = TRUE, width = "100%")
  })
  
  output$shapiroPvalue <- renderUI ({
    ns<- session$ns
    numericInput(ns('shapiroPvalueInput'), 'Select p-value for normality test', min = 0, max = 1, value = 0.05)
  })
  
  #clean list of source files and remove one from the bracket
  sourcelistdrop <- reactive ({
    
    datas_glob <<- corrected_function ()[[1]]
    elementDrops_glob <<- target_function ()[[2]]
    target_glob<<- target_input ()
    
    l_src <- corrected_function ()[[1]] #source list data frames
    
    elementDrops <- target_function ()[[2]]
    target <- target_input ()

    if (length (as.character(unique (elementDrops[,1]))) >0){
      for (j in seq (1, nrow (elementDrops))){
        indx <- which(target[,1] %in% elementDrops[j,1])
        l_src[[indx]] <- l_src[[indx]][,!(names(l_src[[indx]]) %in% elementDrops[j,2])]
      }
      print ('completed')
      return (l_src)
    } else {
      return (l_src) #list of source data
    }
  })
  
  #list of source data
  resultoutput <- eventReactive (input$applyMFA, {
    
    show_spinner()
    
    sourcelistdrop_glob <<- sourcelistdrop ()
    elements_glob <<- input$elementDrops
    shapiro_glob <<- input$shapiroPvalueInput
    
    sourcelistdrop <- lapply (sourcelistdrop(), OUTPUT_MVTEST, drop = input$elementDrops, shapiro_p_val  = input$shapiroPvalueInput)
    l_src <- corrected_function ()[[1]]
    for (i in seq (1, length (sourcelistdrop))){

      myindx <- match (names(sourcelistdrop[[i]]), names(l_src[[i]]))
      
      myna <- which(is.na (myindx))
      if (length(myna)>0){
        sourcelistdrop[[i]] <- sourcelistdrop[[i]][,-myna]
      }
      myindx <- as.numeric(na.omit(myindx))
      l_src[[i]][,myindx] <- sourcelistdrop[[i]] 

    }
    
    l_src
  })
  
  
  output$selectTarget <- renderUI({
    ns <- session$ns
    req (resultoutput())
    selectInput(ns("selectedTarget"), choices = target_input ()[,1], selected = target_input()[1,1], 'Select source to show',width = "100%")
  })
  
  output$resultoutput <- renderDT ({
    indx <- which (target_input()[,1] %in% input$selectedTarget)
    if (length (indx) < 1){
      hide_spinner()
      return (NULL)
    } else {
      hide_spinner()
      datas <- resultoutput()[[indx]]
      datas[,-c(1,2)] <- apply (datas[,-c(1,2)], 2, round, 2)
      #resultoutput()[[indx]][,-c(1,2)]<- apply (resultoutput()[[indx]][,-c(1,2)], 2, round, 2)
      #print (datas)
      resultoutput()[[indx]]
      datas
    }
  })
  
  myreturn <- reactiveValues ()
  #observe({ myreturn$data <- resultoutput()})
  observe({ myreturn$data <- resultoutput()})
  
  return (list(resultoutput, myreturn))
}
  