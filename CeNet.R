#' Run CeNetOmnibus App
#'
#' @param maxRequestSize Integer.The admitted file size for uploaded files. Unit MB. Default 5MB.
#' @param workpath Character. The dictionary for temp files. Default is return value of tempdir()
#' @param projectName Character. The name of this analysis. Default is session token.
#' @param typeLimit Integer. The number of valid items. Default is 10.
#' @param ... Other parameters passed to runApp()
#'
#' @return
#' @export
#'
#' @examples
#'    CeNetOmnibus()
CeNetOmnibus <- function(maxRequestSize=5,workpath=tempdir(),projectName=NULL,typeLimit=10,...) {
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
  library(tibble)
  library(igraph)
  library(scales)
  library(rhandsontable)
  library(PerformanceAnalytics)
  library(linkcomm)
  library(MCL)
  library(visNetwork)
  library(colourpicker)
  library(ggplotify)
  library(survival)
  library(survminer)
  library(ComplexHeatmap)
  library(circlize)
  library(formattable)
  library(infotheo)
  library(ProNet)
  library(gprofiler2)

  maxRequestSize=maxRequestSize*1024^2
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
  suppressMessages(shiny::runApp(system.file("app", package = "CeNetOmnibus"),launch.browser=TRUE,...))
}


#' Install dependency packages for CeNetOmnibus
#'
#' @return
#' @export
#'
#' @examples
install_dependency=function()
{
  dependency=data.frame(package=c('parallel','biomaRt','shiny','plyr','ggplot2','jsonlite','shinydashboard','shinyWidgets','DT','ggthemr','tibble','igraph','scales','rhandsontable','PerformanceAnalytics','linkcomm','MCL','visNetwork','colourpicker','ggplotify','survival','survminer','ComplexHeatmap','circlize','formattable','infotheo','ProNet','gprofiler2'),
                        repo=c('CRAN','Bioc','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','github','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','CRAN','Bioc','CRAN','CRAN','CRAN','self','CRAN'),
                        stringsAsFactors = F
  )
  rownames(dependency)=dependency$package
  installed=installed.packages()

  if(!"devtools" %in% dependency)
  {
    install.packages('devtools')
  }
  if(!"BiocManager" %in% dependency)
  {
    install.packages('BiocManager')
  }

  install=dependency[!which(dependency$package %in% installed),]

  CRAN=install$package[which(install$repo=='CRAN')]
  Bioc=install$package[which(install$repo=='Bioc')]

  if(length(CRAN)>0)
  {
    install.packages(CRAN)
  }
  if(length(Bioc)>0)
  {
    BiocManager::install(Bioc)
  }

  if("ggthemr" %in% install)
    devtools::install_github("cttobin/ggthemr")
  if("ProNet" %in% install)
    install.packages(system.file('ProNet_1.0.0.tar.gz',package = 'CeNetOmnibus'),repos = NULL,type = "source")
}



