
dfa_ui <- function (id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        sidebarLayout(
          div(
            style = "width: 50%;",
            sidebarPanel(
              uiOutput(ns("userChoiceDfa")),
              uiOutput(ns("userChoiceRemove")),
              uiOutput (ns('dfaPValues')),
              uiOutput (ns('dfaTolerance')),
              actionButton(ns("applyDFA"), "Apply")
            ), width = 4
          ),
          mainPanel(
            width = 10,
            tabsetPanel(
              tabPanel (
                'DFA data results',
                box(
                  title = "DFA", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns("dfaTableResults"))), style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
                  )
                ),
                br (),
                br (),
                downloadButton (ns('downloadResults'), 'Download')
              ),
              tabPanel (
                'LDA',
                box (
                  title = "LDA", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns('ldaTable')))
                  )
                ),
                br (),
                br (),
                downloadButton (ns('downloadLda'), 'Download')
              ),
              tabPanel (
                'Best DFA, after confusion matrix',
                box (
                  title = "Confusion Matrix DFA", status = "success", height =
                    "auto", solidHeader = T, width = "auto",
                  column(
                    width = 12,
                    withSpinner(DTOutput(ns('confMat')))
                  )
                ),
                br (),
                br (),
                downloadButton (ns('downloadConfDfa'), 'Download')
              ),
              tabPanel (
                'Bi-plot',
                uiOutput (ns ("dfaSelectForPlot")),
                withSpinner(plotOutput (ns("dfaPlot")))
              ),
              tabPanel (
                '3D-plot',
                box (
                  title = "DFA 3D plot", status = "success", height = 1000, solidHeader = T, width = "auto",
                uiOutput (ns ("dfaSelectForPlot2")),
                div (withSpinner (plotlyOutput(ns('dfaPlot2'))), align = "center")
                )
              )
            )
          )
        )
      )
    )
  )
}




dfa_server <- function (input, output, session, corrected_function, target_function, target_input) {
  
  
  output$downloadResults <- downloadHandler(
      file = function() {
        paste("DFA_Results", input, Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(dfaresults(), file, row.names = F)
      }
    )
  
  output$downloadConfDfa <- downloadHandler(
    file = function() {
      paste("Confdfa_Results", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(confmat(), file, row.names = F)
    }
  )
    
  output$downloadLda <- downloadHandler(
    file = function() {
      paste("LDA_Results", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(ldatable(), file, row.names = F)
    }
  )
  
  output$userChoiceDfa <- renderUI({ #buttons for default or user compute
    ns <- session$ns
    radioButtons(
      ns("userChoiceDfa"), "Apply DFA, default or uniform weights?",
      c(
        "Default" = "def",
        "Uniform" = "uni"
      ),
      selected = "def"
    )
  })
  
  
  # option to pick columns that will not be used for correction
  output$userChoiceRemove <- renderUI({
    ns <- session$ns
    selectInput(ns("dfaR"), "Select columns to remove from DFA", choices = colnames(target_function()[[1]]), multiple = TRUE, width = "100%")
  })
  
  #clean list of source files and remove one from the bracket
  source_l_drop <- reactive ({
    l_src <- corrected_function () #source list data frames
    drops <- target_function ()[[2]]
    drops[,2] <- as.character (drops[,2])
    drops[,1] <- as.character (drops[,1])
    target <- target_input ()
    
    l_src_glob <<- l_src
    drops_glob <<- drops
    target_glob <<- target_input()
    

    if (!is.null(input$dfaR)){
      for (i in seq (1, length(l_src))){
        dat <- l_src[[i]]
        l_src[[i]] <- dat[,!(names(dat) %in% input$dfaR)]
      }
    }

    if (length (as.character(unique (drops[,1]))) >0){
      for (j in seq (1, nrow (drops))){
        indx <- which(as.character(target[,1]) %in% as.character(drops[j,1]))
        l_src[[indx]] <- l_src[[indx]][,!(names(l_src[[indx]]) %in% as.character(drops[j,2]))]
      }
      
      return (l_src)
    } else {
      return (l_src) #list of source data
    }
    })
  
  
  output$dfaPValues <- renderUI({
    ns <- session$ns
    #req (dfaresults())
    numericInput(ns ('dfaPValues'), 'DFA p value option', min = 0.001, max = 1, value = 0.01)
  })
  
  output$dfaTolerance <- renderUI({
    ns <- session$ns
    #req (dfaresults())
    numericInput(ns ('dfaTolerance'), 'DFA dfaTolerance option', min = 0.001, max = 1, value = 0.0001)
  })
  
  applydfa <- reactive({
    req(corrected_function())
    req (source_l_drop())
    y <- source_l_drop ()
    y_glob_dfa <<- y
    dat <- stepwiseDFA(y, input$dfaPValues, input$dfaTolerance)
    #print (stepwiseDFA(y[[2]]))
    dat #returns list of data frames (dfa)
  })
  
  
  output$ldaTable <- renderDT ({
    ldatable()
  })
  
  ldatable <- reactive ({
    req (applydfa())
    datas <- applydfa()
    target <- target_input ()
    sourcetable <- corrected_function ()
    #datas <- apply (datas, 2, round, 3)
    dat <- NULL
    datas_glob <<- datas
    target_glob <<- target
    source_glob <<- sourcetable
    
    for (i in seq (1, length(datas))){
      ldDat <- which (names (datas[[i]][[4]]) %in% ("LD1"))
      roundLD <- apply (datas[[i]][[4]][,c(ldDat:ncol(datas[[i]][[4]]))], 2, round, 3 )
      mydat <- data.frame (as.character(target[i,1]), as.character(sourcetable[[i]][,1]))
      mydat <- data.frame (mydat, roundLD)
      dat <- rbind (dat,mydat)
    }
    ldaTable <- as.data.frame(dat)
    names(ldaTable)[c(1,2)] <- c("Target", "Sample")
    ldaTable
  })
  
  output$confMat <- renderDT ({
    confmat()
  })
  
  confmat <- reactive ({
    req (applydfa ())
    datas <- applydfa()
    dat <- NULL
    target <- target_input ()
    for (i in seq (1, length (datas))){
      dat <- rbind (dat, as.character(target[i,1]), datas[[i]][[5]])
    }
    dat
  })
  
  #returns dfa uniform weights
  applydfadefault <- reactive ({
    dat <- target_function()[[1]]
    drops <- target_function ()[[2]]
  
    d <- dim(dat)
    dat_output <- data.frame(matrix(100, nrow = d[1], ncol = (d[2])))
    colnames(dat_output) <- names(dat)
    l_trg <- target_function()[[2]]
    l_trg[,1] <- gsub ("target", "" ,l_trg[,1])
    
    if (!is.null(l_trg)){
      for (i in seq (1, d[1])){
        lsub <- l_trg[which (l_trg[,1] == i),]
        dat_output[i,which (colnames(dat_output) %in% lsub[,2])] <- 0
      }
    }
    
    if (!is.null(input$dfaR)){
      dat_output[, which(colnames(dat_output) %in% input$dfaR)] <- 0
    }
    
    dat_output
  })
  
  
  #returns computed dfa weights
  filterdfaresult <- reactive({
    
    dfa_glob <- applydfa ()
    l_dfa <- list ()
    
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[1]]}
    
    dat <- target_input()
    d <- dim(dat)
    
    dat_output <- data.frame(matrix(0, nrow = d[1], ncol = (d[2])))
    colnames(dat_output) <- names(dat)
    dat_output <- dat_output[,-c(1,2)]
    
    for (i in seq (1, d[1])){
      dat_output[i, na.omit(match (l_dfa[[i]][,1],names(dat_output)))] <- l_dfa[[i]][,3]
    }
    dat_output
  })
  
  output$dfaPlot <- renderPlot ({
    req (dfaresults())
    req (input$dfaNumberInput)
    dfa_glob <- applydfa ()
    l_dfa <- list ()
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[2]]}
    p <- l_dfa[[as.numeric(input$dfaNumberInput)]]
    p
  },width = 1000, heigh = 600)
  
  output$dfaPlot2 <- renderPlotly ({
    req (dfaresults())
    req (input$dfaNumberInput2)
    dfa_glob <- applydfa ()
    l_dfa <- list ()
    for (i in seq (1, length (dfa_glob))) {l_dfa[[i]] <- dfa_glob[[i]][[3]]}
    p <- l_dfa[[as.numeric(input$dfaNumberInput2)]]
    p
  })
  
  output$dfaSelectForPlot <- renderUI({
    ns <- session$ns
    req (dfaresults())
    req (filterdfaresult())
    req (applydfa ())
    s <- seq (1, length (applydfa()))
    selectInput(ns ('dfaNumberInput'), 'Select source', selected = s[1], choices = s )
  })
  
  output$dfaSelectForPlot2 <- renderUI({
    ns <- session$ns
    req (dfaresults())
    req (filterdfaresult())
    req (applydfa ())
    s <- seq (1, length (applydfa()))
    selectInput(ns ('dfaNumberInput2'), 'Select source', selected = s[1], choices = s )
  })
  
  
  dfaresults <- eventReactive(input$applyDFA, (
    if (input$userChoiceDfa == "def") {
      dat <- filterdfaresult ()
      dat
    } else {
      dat <- applydfadefault ()
      dat
    }
  ))
  
  #output data table
  output$dfaTableResults <- renderDT ({
    dfaresults ()
  })
  
  myreturn <- reactiveValues ()
  observe({myreturn$data <- dfaresults()})

  return (list(dfaresults, myreturn))
  
}