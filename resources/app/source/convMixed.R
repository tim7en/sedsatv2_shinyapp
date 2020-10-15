library (stringr)
#function converts elements with mixed signs according to user defined options
convert <- function(dat, negGlob, zeroConst = 0.001) {
  #find index of a column with 0 value inside
  indxl <- list ()
  for (i in seq (1, ncol(dat))){
    k <- 1
    if (all(dat[,i] == 0)){
      indxl[[k]] <- i
      k <- k +1
    }
  }
  
  if (length(indxl) > 0){
    dat[,unlist(indxl)] <- dat[,unlist(indxl)] + zeroConst
  }
  
  negatives <- colnames(dat)[-c(1, 2)][sapply(dat[, -c(1, 2)], function(x) min(x)) <= 0]
  positives <- colnames(dat)[-c(1, 2)][sapply(dat[, -c(1, 2)], function(x) max(x)) >= 0]
  mixed <- negatives[which(negatives %in% positives)]
  mixed <<- mixed
  negatives <<- negatives
  mixed<-mixed[!is.na(mixed)]
  negatives <- negatives[!is.na(negatives)]
  negatives_only <- negatives[which(!negatives %in% mixed)]
  dat[,c(negatives_only)]<- (dat[,c(negatives_only)] * -1)
  if (length(mixed) == 0) {
    return (dat)
  }

  if (length(mixed) == 1) {
    x <- dat[, c(mixed)]
    dat_origin <- x
    formulas <- strsplit(negGlob, ",")
    convert <- eval(parse(text = formulas[[1]][1]))
    dat[, c(mixed)] <- convert
    return(dat)
  } else if (length(mixed) > 1) {
    if (length(unlist(strsplit(negGlob, ","))) > 2) {
      formulas <- unlist(strsplit(negGlob, ";"))
      if (length(formulas) == length(mixed)) {
        l <- sapply(formulas, strsplit, split = ",")
        flag <- 0
        for (i in seq(1, length(l))) {
          x <- dat[, c(mixed[i])]
          dat_origin <- x
          convert <- eval(parse(text = l[[i]][1]))
          dat[, c(mixed[i])] <- convert
        }
        return(dat)
      } else {
        NULL
      }
    }
  } else {
    NULL
  }
}

inverse_neg_glob <- function(dat_rv,neg_glob, mixed, zeroTracers){
  formulas <- unlist(strsplit(neg_glob, ";"))
  input <- list ()
  input$negts_par <- mixed
  if (length(input$negts_par) > 0) {
    l <- sapply(formulas, strsplit, split = ",")
    flag <- 0
    for (i in seq(1, length(input$negts_par))) {
      if (length(input$negts_par) == 1){
        x <- dat_rv[, input$negts_par]
      } else {
        x <- dat_rv[, input$negts_par[i]]
      }
      dat_origin <- x
      inverse <- eval(parse(text = str_trim(l[[i]][2])))
      inverse <- round(inverse, 3)
      dat_rv[,input$negts_par[i]] <- inverse
      if (length(zeroTracers) > 0){
        dat_rv[c(zeroTracers)] <- dat_rv[c(zeroTracers)] - zeroConstant
      }
    }
  } else {
    NULL
  }
  return (dat_rv)
}
