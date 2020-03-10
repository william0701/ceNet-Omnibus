#projName = NULL
#tmpdir=NULL
#typeLimit = 10
Super <- function(run = TRUE,maxRequestSize=5*1024^2,workpath=tempdir(),projectName=NULL,typeLimit=10,...) {
  library(parallel)
  library(biomaRt)
  library(shiny)
  library(plyr)
  library(ggplot2)
  library(jsonlite)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(ggthemr)
  library(R.matlab)
  library(tibble)
  library(igraph)
  library(scales)
  library(rhandsontable)
  library(PerformanceAnalytics)
  #library(rJava)
  library(linkcomm)
  library(MCL)
  library(visNetwork)
  library(colourpicker)
  #library(ProNet)
  library(ggplotify)
  library(survival)
  library(survminer)
  library(ComplexHeatmap)
  library(circlize) 
  library(formattable)
  library(infotheo)
  tmpdir<<-normalizePath(workpath)
  projName<<-projectName
  if(!is.null(projectName))
  {
    projName <<- gsub(pattern = " ",replacement = '_',x = projectName)
  }
  typeLimit <<- typeLimit
  ggthemr('flat')
  usedcolors=swatch()
  options(shiny.maxRequestSize = maxRequestSize)
  #if(run) suppressMessages(shiny::runApp(system.file("app", package = "shinyAppDemo"),launch.browser=TRUE,...))

  runApp(appDir = "D:\\ceNet-Omnibus1\\app",...)
  # runApp(appDir = "D:\\software\\code\\R_code\\final_web_git\\ceNet-Omnibus\\app",...)
}


#Super(run = T,workpath = "D://Test/",projectName = "Single_cell_case")
Super(projectName ="Single cell case" ,typeLimit = 20)
#Super(typeLimit = 10)


