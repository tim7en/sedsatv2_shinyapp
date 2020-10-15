splash_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("splash_screen"), img(src =  "Infinity-1s-200px.gif"),
      style = "text-align:center; padding-top:250px;")
}

splash_server <- function(input, output, session) {

  myfunc <- function (){
    mypcg <- c("factoextra","klaR", "plotly",
               "shiny", "shinydashboard", "DT", "data.table", "psych",
               "htmlwidgets","parallel", "ggplot2", "rstudioapi", "boot", "dplyr",
               "shinycssloaders", "formula.tools", "corrplot",
               "grid","zCompositions", 'cluster', "car",
               "plotly", 'lmtest', 'broom', 'stringr', 'mvnormtest', 'smwrBase', 'MVN'
    )
    suppressMessages({
      suppressWarnings({
        suppressPackageStartupMessages({
          for (i in mypcg){
            library(i, character.only = TRUE)
          }
        })
      })
    })
    
    
    candidates <- c( Sys.getenv("R_PROFILE"),
                     file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
                     Sys.getenv("R_PROFILE_USER"),
                     file.path(getwd(), ".Rprofile") )
    
    
    hide("splash_screen", anim = TRUE, animType = "fade", time = 3)
  }
  myfunc ()

}