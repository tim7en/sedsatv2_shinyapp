source ("./source/correction_func_improved.R")

correction_ui <- function(id) {
  ns <- NS(id)
  tagList(
  sidebarLayout(
    div(
      style = "width: 70%;", # <----- here use shinyjs
      sidebarPanel(
        tabPanel(
          "Initial input",
          uiOutput(ns("correctButton")), #With correction or skip correction
          uiOutput(ns('modelSelectedOutput')), #Raw model or the best model
          uiOutput(ns("sourceAdjustFor")), #Adjust for a selected element
          uiOutput(ns("sourceRemove")), #Remove from adjustment
          
          #add p value of the equation
          uiOutput(ns("sourceEq")),
          uiOutput(ns("sourceShapiro")), #Shapiro test p value of residuals
          
          
          uiOutput(ns("sourceCorrection")), #R2 value
          uiOutput(ns("standardDiv")), #VIF
          
          
          uiOutput(ns("pvalueDW")),
          uiOutput(ns("performDW")), #Durbin watson test #need p value adjustment
          
          uiOutput(ns("pvalueBP")),
          uiOutput(ns("performBP")), #Breush pagan test #need p value adjustment
          
          
          uiOutput(ns("CooksD")), #Cooks distance
          uiOutput(ns("sourceApplyCorrelation")), #Button
          br(),
          br()
        )
      )
    ),
    mainPanel(
      width = 9,
      fluidPage(
        tabsetPanel(
          tabPanel(
            "All Options",
            use_busy_spinner(spin = "pixel", color = "#1230bf", margins = c(10, 10), spin_id = NULL, height = "250px",
                             width = "250px", position = "bottom-right"),
            
            box(
              title = "Available options", status = "success", height =
                "auto", solidHeader = T, width = "12",
              column(
                width = 12,
                shinycssloaders::withSpinner(DTOutput(ns("resOutput"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          tabPanel (
            "Selected results",
            box(
              title = "Selected", status = "primary", height =
                "auto", width = "12", solidHeader = T,
              column(
                width = 12,
                withSpinner(DTOutput(ns("selectedOut"))), style = "height:'500px'; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          tabPanel (
            'Corrected data',
            box (
              title = "Correction", status = "primary", height = 
                "auto", width = '12', solidHeader = T,
              column (
                width = 12,
                uiOutput(ns('selectTarget')),
                br(),
                withSpinner (DTOutput(ns("correctedListDf"))), style = "height:'500px'; overflow-y: scroll; overflow-x: scroll;" 
              )
            ),
            br (),
            br (),
            downloadButton (ns('downloadSourceCorrected'), 'Download')
          ),
          tabPanel (
            'Plot transformed & untransformed results',
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectTarget2'))),
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectPredictor'))),
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectResponse'))),
            box (
              title = "Model summary for available source types", status = "primary",
              height = "auto", width = "12", solidHeader = T,
              column (
                width = 12,
                DTOutput(ns('modelSummary'))
              )
            ),
            box (
              title = "Plot of transformed & untransformed sources for selected Target", status = "primary",
              height = 870L, width = "12", solidHeader = T,
              column(
                width = 12,
                plotlyOutput(ns('transformedPlot'))
              )
            )
          )
          # Possible future implementation of the model selection
          # tabPanel (
          #   'Edit models',
          #   box (
          #     title = "Linear model selected", status = "primary",
          #     height = "auto", width = "12", solidHeader = T,
          #     column (
          #       width = 12,
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectTarget3'))), #target
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectSourceType'))), #source
          #       div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput (ns('selectResponse2'))), #response
          #       DTOutput(ns('modelSelected'))
          #     )
          #   ),
          #   box (
          #     title = 'All available models', status = "primary",
          #     height = "auto", width = "12", solidHeader = T,
          #     column (
          #       width = 12,
          #       DTOutput(ns('modelAllAvailable'))
          #     )
          #   )
          # )
        )
      )
    )
  )
 )
}


correction_server <- function(input, output, session, sourceData, targetData){
  
  output$downloadSourceCorrected <- downloadHandler(
    filename = function() {
      paste(paste0("Selected_source-",input$targetID), input, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(downloadSourceCorrected(), file, row.names = F)
    }
  )
  
  resoutput <- reactiveValues(myresult = NULL)
  resoutput2 <- reactiveValues(dfList = NULL)
  
  mydt <- reactive ({
    resoutput$myresult
  })
  
  
  # radiobutton, select correct or no correct
  output$correctButton <- renderUI({
    ns <- session$ns
    radioButtons(
      ns("correctButton"), "Correct for any of the elements?",
      c(
        "Correct" = "Cor",
        "No correction" = "noCor"
      ),
      selected = "noCor"
    )
  })
  
  # keep only columns in the source that are present in the target
  mySource <- reactive({
    req(sourceData())
    req(targetData())
    sourceData()[, which(names(sourceData()) %in% names(targetData()))]
  })
  
  myTarget <- reactive({
    req (targetData)
    targetData()
  })
  
  # select columns to adjust for
  output$sourceAdjustFor <- renderUI({
    ns <- session$ns
    selectInput(ns("slcCorrect"), "Select columns with size, toc or together", choices = names(mySource())[-c(1, 2)], multiple = TRUE, width = "100%")
  })
  
  # option to pick columns that will not be used for correction
  output$sourceRemove <- renderUI({
    req(input$slcCorrect)
    ns <- session$ns
    if (!is.null(input$slcCorrect)) {
      drops <- c(names(mySource())[c(1, 2)], input$slcCorrect)
    } else {
      drops <- names(mySource())[c(1, 2)]
    }
    selectInput(ns("slcRemove"), "Select columns to remove from correction", choices = names(mySource())[which(!names(mySource()) %in% drops)], multiple = TRUE, width = "100%")
  })
  
  # select p value threshold of normality for residuals
  output$sourceShapiro <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput(ns("sourceShapiro"), "Select p-value for Shapiro Wilk test of normality for model residuals:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$pvalueDW <- renderUI ({
    req (mySource())
    ns <- session$ns
    sliderInput(ns("pvalueDW"), "Select p-value for Durbin Watson test:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$pvalueBP <- renderUI ({
    req (mySource())
    ns <- session$ns
    sliderInput(ns("pvalueBP"), "Select p-value for Breusch Pagan test:", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$sourceEq <- renderUI ({
    req (mySource())
    ns <- session$ns
    sliderInput(ns("sourceEq"), "Select p-value for F test of model significance", value = 0.05, min = 0.001, max = 1, step = 0.01)
  })
  
  output$standardDiv <- renderUI ({
    req(mySource ())
    ns <- session$ns
    sliderInput (ns("sourceStandardDiv"), "Select threshold for Variance Inflation Factor (VIF)", value = 10, min = 1, max = 30, step = 1)
  })
  
  output$performDW <- renderUI ({
    req (mySource())
    ns <- session$ns
    checkboxInput(ns("performDW"), "Use Durbin-Watson test", value = TRUE, width = NULL)
  })
  
  output$performBP <- renderUI ({
    req (mySource())
    ns <- session$ns
    checkboxInput (ns("performBP"), "Use Breush-Pagan test", value = TRUE, width = NULL)
  })
  
  output$CooksD <- renderUI ({
    req(mySource ())
    ns <- session$ns
    sliderInput (ns("CooksD"), "Select value of Cooks distance of leverage", value = 1, min = 0.01, max = 5, step = 0.01)
  })
  
  # sliderinput for corrplot R threshold
  output$sourceCorrection <- renderUI({
    req(mySource())
    ns <- session$ns
    sliderInput(ns("sourceCorrection"), "Select threshold value of R^2", value = 0.6, min = 0.1, max = 0.99, step = 0.1, animate = F)
  })
  
  # action button, apply
  output$sourceApplyCorrelation <- renderUI({
    req(mySource())
    ns <- session$ns
    actionButton(ns("sourceApplyCorrelation"), "Apply")
  })
  
  dats <- eventReactive(input$sourceApplyCorrelation, {
    
    
    
    if (input$correctButton %in% "Cor"){
      show_spinner()
      req (input$slcCorrect)
      
      print ("correction")
      #check if there any negative, 0
      if (any (mySource()[,-c(1,2)] < 0) | any (mySource()[,-c(1,2)] == 0)){
        datas <- convert (mySource(), negGlob, zeroConstant)
      } else {
        datas <- mySource()
      }
      resoutput$myresult <- correct.improved(datas,  input$slcCorrect, input$slcRemove, input$sourceStandardDiv, input$sourceCorrection, input$sourceShapiro, input$sourceEq, input$pvalueDW, input$performDW, input$pvalueBP, input$performBP, input$CooksD)
      
      print ("finished correction")
      res_glob <<- resoutput$myresult
      
      resoutput$myresultFunc <- resoutput$myresult
      resoutput$myresult
    } else {
      
    }
  })
  
  # radio button serve as indicator of user choice or default choice
  output$ui_formulas_selected <- renderUI({
    req(mySource())
    ns <- session$ns
    radioButtons(
      ns("ui_formulas_selected"), "Select data :",
      c(
        "Top rank" = "def",
        "User choice" = "sel"
      ),
      selected = "def"
    )
  })
  
  # radio button serve as indicator of user choice or default choice
  output$modelSelectedOutput <- renderUI({
    req(mySource())
    ns <- session$ns
    radioButtons(
      ns("modelSelectedInput"), "Select model transformation :",
      c(
        "Best model" = "bestM",
        "Raw model" = "rawM"
      ),
      selected = "bestM"
    )
  })
  
  
  output$resOutput <- renderDT ({
    req (moduleOutput())
    req (selectedOut())
    req (selectedOutTable ())
    resoutput$myresult[,4]<- gsub("I(", "(",resoutput$myresult[,4], fixed = T)
    resoutput$myresult
    hide_spinner()
    mydt()
  })
  
  selectedOut <- eventReactive ( input$sourceApplyCorrelation, {
    if (input$correctButton %in% "Cor"){
    runjs(code = '$("resOutput").toggleClass("recalculating");')
    
    print ("class added")
    req (dats())
    req (input$slcCorrect)
    drops <- NULL
    
    #check if there any negative, 0
    if (any (mySource()[,-c(1,2)] < 0) | any(mySource()[,-c(1,2)] == 0)){
      datas <- convert (mySource(), negGlob, zeroConstant)
    } else {
      datas <- mySource()
    }
    
    #check if there any negative, 0
    if (any (myTarget()[,-c(1,2)] < 0) | any(myTarget()[,-c(1,2)] == 0)){
      target <- convert (myTarget(), negGlob, zeroConstant)
    } else {
      target <- myTarget()
    }
    
    
    if (is.null(drops)){
      drops <- cbind ('None', 'None')
      drops <- list(NULL,drops)
    }
    print ('trying correction integrity check')
    corList <- correctionIntegrityCheck(datas, target, resoutput$myresultFunc, input$slcCorrect, drops)
    corList
    } else {
      
    }
  })
  
  selectedOutTable <- eventReactive (input$sourceApplyCorrelation,{
    if (input$correctButton %in% "Cor"){
    selectedOut ()
    dat <- selectedCorrectionTabs (selectedOut(), myTarget(), input$modelSelectedInput)
    dat } else {
      
    }
  })
  
  
  output$selectedOut <- renderDT ({
    selectedOutTable ()
    dat <- selectedOutTable()
    dat[,5]<- gsub("I(", "(",dat[,5], fixed = T)
    
    hide_spinner()
    dat
  })
  
  
  output$selectTarget <- renderUI ({
    target <- myTarget ()
    ns <- session$ns
    uniqueTargets <- unique(as.character(target[,1]))
    selectInput(ns('targetID'), 'Targets', choices = uniqueTargets)
  })
  
  observe ({
    #req (correctedListData())
    resoutput2$dfList <- correctedListData  () #changes to dfList does not trigger reactive action from the
  })
  
  correctedListData <- eventReactive ( input$sourceApplyCorrelation, {
    if (input$correctButton %in% "Cor"){
    req (selectedOut())
    
    zeroTracers <- NULL
    mixed <- NULL
    negatives <- NULL
    
    for (i in seq (3, ncol(mySource()))){
      if (any(mySource()[,i] == 0)){
        zeroTracers <- c (zeroTracers,i)
      }
      
      if (any(mySource()[,i] == 0) && any(mySource()[,i]<0)){
        mixed <- c(mixed, i)
      }
      
      if (any(mySource()[,i] >= 0)){} else {
        negatives <- c(negatives, i)
      }
    }
    
    print (zeroTracers)
    print (mixed)
    print (negatives)
    
    if (any(is.na(zeroTracers))){zeroTracers <- NULL}
    if (any(is.na(mixed))){mixed <- NULL}
    if (any(is.na(negatives))){negatives <- NULL}
    
    zeroTracers <- zeroTracers[which(!zeroTracers %in% mixed) && which (!zeroTracers %in% negatives)]
    mixed <- mixed[which(!mixed %in% zeroTracers) && which(!mixed %in% negatives)]
    negatives <- negatives[which(!negatives %in% zeroTracers) && which (!negatives %in% mixed)]
    #check if there any negative, 0
    if (any (mySource()[,-c(1,2)] < 0) | any(mySource()[,-c(1,2)] == 0)){
      datas <- convert (mySource(), negGlob, zeroConstant)
    } else {
      datas <- mySource()
    }
    
    indxl <- list ()
    for (i in seq (1, ncol(datas))){
      k <- 1
      if (any(datas[,i] == 0)){
        indxl[[k]] <- i
        k <- k +1
      }
    }
    
    if (length(indxl) > 0){
      zeroTracers <- unlist (indxl)
    } else {
      zeroTracers <- NULL
    }
    
    #check if there any negative, 0
    if (any (myTarget()[,-c(1,2)] < 0) | any(myTarget()[,-c(1,2)] == 0)){
      target <- convert (myTarget(), negGlob, zeroConstant)
    } else {
      target <- myTarget()
    }
    
    dfList <- getListCorrectedData (selectedOut(), target, datas, input$slcCorrect,input$modelSelectedInput, negGlob, zeroTracers, mixed, negatives)
    dfList
    } else {
      
    }
  })
  
  output$correctedListDf <- renderDT ({
    req (input$targetID)
    i <- which (myTarget()[,1] %in% input$targetID)
    resoutput2$dfList[[i]]
  })
  
  downloadSourceCorrected <- reactive ({
    req (input$targetID)
    i <- which (myTarget()[,1] %in% input$targetID)
    resoutput2$dfList[[i]]
  })
  
  moduleOutput <- reactive ({
    req (input$correctButton)
    drops <- NULL
    if (input$correctButton %in% "Cor"){
      listDataFramesCorrected <- resoutput2$dfList
      if (!is.null(drops)){
        for (i in seq(1, nrow(drops))){
          myindx <- which(as.character(myTarget()[,1]) %in% as.character(drops[i,1]))
          listDataFramesCorrected[[myindx]] <- listDataFramesCorrected[[myindx]][,!(names(listDataFramesCorrected[[myindx]]) %in% drops[i,2])]
        }
      }
      list (listDataFramesCorrected, NULL, NULL)
    } else {
      
      listDataFrames <- rep(list(mySource()), nrow(myTarget()))
      if (!is.null(drops)){
        for (i in seq(1, nrow(drops))){
          myindx <- which(as.character(myTarget()[,1]) %in% as.character(drops[i,1]))
          listDataFrames[[myindx]] <- listDataFrames[[myindx]][,!(names(listDataFrames[[myindx]]) %in% drops[i,2])]
        }
      }
      list (listDataFrames, NULL, NULL)
    }
  })
  
  #Plot & summary section
  output$selectTarget2 <- renderUI ({
    ns <- session$ns
    trg <- unique(as.character((selectedOutTable())[,1]))
    selectInput(ns('targetSelected'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  output$selectPredictor <- renderUI ({
    ns <- session$ns
    if (length (input$slcCorrect) ==1) {
      selectInput(ns('correctForSelected'), 'Select Predictor', choices = input$slcCorrect, selected = input$slcCorrect)
    } else {
      selectInput(ns('correctForSelected'), 'Select Predictor', choices = input$slcCorrect, selected = input$slcCorrect[1])
    }
  })
  
  output$selectSource <- renderUI ({
    ns <- session$ns
    req (input$correctForSelected)
    req (input$targetSelected)
    subSelected <- selectedOutTable()[which (selectedOutTable()[,1] %in% input$targetSelected),]
    selectInput(ns('sourceTypeSelected'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  output$selectResponse <- renderUI ({
    ns <- session$ns
    req (input$correctForSelected)
    req (input$targetSelected)
    selectedData <- selectedOutTable()
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected),]
    selectInput(ns('responseSelected'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  #new transformed plot
  output$transformedPlot <- renderPlotly({
    ns <- session$ns
    req (input$targetSelected)
    req (input$responseSelected)
    req (input$correctForSelected)
    
    #x<- inverse_negGlob(x, negGlob, mixed)
    x <- mySource() #untransformed source
    target <- myTarget()
    
    indx <- which (target[,1] %in% input$targetSelected)
    #datas <- correctedListData()[[indx]]
    datas <- resoutput2$dfList[[indx]]
    
    dat <- rbind (datas, x)
    names(dat)[2] <- 'Classes'
    mygrid <- rep ('Corrected', nrow (datas))
    mygrid <- c (mygrid, rep ('Uncorrected', nrow (x)))
    dat <- cbind (dat,mygrid)
    names(dat)[ncol(dat)] <- 'Method'
    suppressMessages(attach(dat))
    suppressWarnings(assign("Response", dat[, c(as.character(input$responseSelected))]))
    suppressWarnings(assign("Predictor", dat[, c(as.character(input$correctForSelected))]))
    p <- ggplot(dat, aes(x = Predictor, y = Response, colour = Method)) +
      geom_point()+
      geom_smooth(method = lm)+
      theme(panel.spacing = unit(2, "lines"), legend.position = "bottom")
    p <- p+facet_wrap(Classes~., ncol = 2, scales = "free") #split in horizontal direction
    ggplotly(p, height = 800, width = 1000) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  output$modelSummary <- renderDT ({
    req (input$targetSelected)
    req (input$responseSelected)
    req (input$slcCorrect)
    target <- myTarget ()
    selectedData <- selectedOutTable()
    x <- mySource()
    
    indx <- which (target[,1] %in% input$targetSelected)
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected),]
    subSelected <- subSelected[which(subSelected[,3] %in% input$responseSelected),]
    summaryList <- NULL
    for (i in seq (1, nrow(subSelected))){
      subDatas <- x[which (x[,2] %in% subSelected[i,2]),]
      if (length(input$slcCorrect)>1){
        var1 <- subDatas[,input$slcCorrect[1]]
        var2 <- subDatas[,input$slcCorrect[2]]
      } else {
        var1 <- subDatas[,input$slcCorrect]
      }
      conc <- subDatas[,input$responseSelected]
      myfit <- lm(eval(parse(text = subSelected[i,5])))
      mysummary <- tidy(summary(myfit))
      mysummary[,2:ncol(mysummary)] <- apply (mysummary[,2:ncol(mysummary)], 2, round, 5)
      mysummary <- cbind (subSelected[i,2], mysummary)
      summaryList <- rbind(summaryList, data.frame(mysummary))
    }
    summaryList[,2]<- gsub("I(", "(",summaryList[,2], fixed = T)
    names(summaryList)[1] <- 'SourceType'
    summaryList
  })
  
  #Plot & summary section
  output$selectTarget3 <- renderUI ({
    ns <- session$ns
    trg <- unique(as.character((selectedOutTable())[,1]))
    selectInput(ns('targetSelected2'), 'Select Target', choices = trg, selected = trg[1])
  })
  
  #uiSelect Source Types
  output$selectSourceType <- renderUI ({
    ns <- session$ns
    req (input$targetSelected2)
    subSelected <- selectedOutTable()[which (selectedOutTable()[,1] %in% input$targetSelected2),]
    selectInput(ns('sourceTypeSelected2'), 'Select Source Type', choices = unique(as.character(subSelected[,2])))
  })
  
  #ui select responses
  output$selectResponse2 <- renderUI ({
    ns <- session$ns
    req (input$targetSelected2)
    selectedData <- selectedOutTable()
    subSelected <- selectedData[which (selectedData[,1] %in% input$targetSelected2),]
    subSelected <- subSelected [which(subSelected[,2] %in% input$sourceTypeSelected2),]
    selectInput(ns('responseSelected2'), 'Select Response', choices = unique (as.character(subSelected[,3])), selected = unique (as.character(subSelected[,3]))[1])
  })
  
  output$modelSelectedTable <- renderDT ({
    req (input$targetSelected2)
    req (input$sourceTypeSelected2)
    req (input$responseSelected2)
    
    dat <- selectedOutTable ()
    dat <- dat[which(dat[,ncol(dat)]!=TRUE),]
    subdat <- dat[which (dat[,1] %in% input$targetSelected2),]
    subdat <- subdat [which(subdat[,2] %in% input$sourceTypeSelected2),]
    subdat <- subdat [which(subdat[,3] %in% input$responseSelected2),]
    subdat[,5]<- gsub("I(", "(",subdat[,5], fixed = T)
    subdat
  })
  
  modelAllAvailable <- reactive ({
    req (input$targetSelected2)
    req (input$sourceTypeSelected2)
    req (input$responseSelected2)
    
    indx <- which(myTarget ()[,1] %in% input$targetSelected2)
    dat <- selectedOut ()[[indx]]
    dat <- dat[which(dat[,ncol(dat)]!= TRUE),]
    dat <- dat[,-ncol(dat)]
    #subdat <- dat[which (dat[,1] %in% input$targetSelected2),]
    subdat <- dat [which(dat[,1] %in% input$sourceTypeSelected2),]
    subdat <- subdat [which(subdat[,2] %in% input$responseSelected2),]
    subdat[,4]<- gsub("I(", "(",subdat[,4], fixed = T)
    subdat
  })
  
  output$modelAllAvailable <- renderDT ({
    modelAllAvailable ()
  }, selection = 'single')
  
  observeEvent(input$modelAllAvailable_rows_selected, {
    applyformula <-modelAllAvailable()[input$modelAllAvailable_rows_selected,]
    data <- resoutput2$dfList
    target <- input$targetSelected2
    element <- input$responseSelected2
    sourceType <- input$sourceTypeSelected2
    
    targetdf <- myTarget ()
    indx <- which (targetdf[,1] %in% input$targetSelected)
    dat <- data[[indx]]
    subdat <- dat [which(dat[,2] %in% input$sourceTypeSelected2),]
    
    allformulas <- gsub("I(", "(",resoutput$myresult[,4], fixed = T)
    findx <- which (allformulas %in% applyformula[,4])
    useformula <- resoutput$myresultFunc[findx,4]
    
    if (length(input$slcCorrect)>1){
      var1 <- subdat[, which(names(subdat) %in% input$slcCorrect[1])]
      var2 <- subdat[,which(names(subdat) %in% input$slcCorrect[2])]
    } else if (length(input$slcCorrect)==1){
      var1 <- subdat[ ,which(names(subdat) %in% input$slcCorrect)]
    }
    
    x <- mySource()
    x_sub <- x[which(x[,2] %in% input$sourceTypeSelected2),]
    conc <- x_sub[,which(names(x_sub) %in% input$responseSelected2)]
    myfit <- lm(eval(parse(text = useformula)))
    
    targetSelected <- myTarget()[which(myTarget()[,1] %in% input$targetSelected),]
    myreturn <- correctOneVar (x_sub, targetSelected, input$slcCorrect, input$responseSelected2, useformula)
    
    x_sub[,input$responseSelected2] <- myreturn
    resoutput2$dfList[[indx]][which(x[,2] %in% input$sourceTypeSelected2),] <- x_sub
  })
  
  myreturn <- reactiveValues (data = NULL)
  observe({myreturn$data <- moduleOutput()})
  
  return (list(moduleOutput,myreturn))
}
#shinyApp(ui = ui, server = server)