suppressMessages({
  suppressWarnings({
    suppressPackageStartupMessages({
      suppressForeignCheck({
mypcg <- c("factoextra","klaR",
  "shiny", "shinydashboard", "DT", "data.table", "psych",
  "htmlwidgets","parallel", "ggplot2", "rstudioapi", "boot", "dplyr",
  "shinycssloaders", "formula.tools", "corrplot",
  "grid","zCompositions", 'cluster', "car",
  "plotly", 'lmtest', 'broom'
)



# check.and.install.Package<-function(package_name){
#   if(!package_name %in% installed.packages()){
#     #print ('installing package')
#     install.packages(package_name)
#   }
# }

library (shiny)
library (shinydashboard)
# for (i in mypcg){
#   check.and.install.Package(i)
# }
#sapply (mypcg, check.and.install.Package)
#sapply (mypcg, library, character.only = TRUE)
      })
    })
  })
})

