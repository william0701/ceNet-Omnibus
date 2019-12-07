#.libPaths('D:/Program Files (x86)/R-3.5.2/library')
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
library(rJava)
library(linkcomm)
library(MCL)
library(visNetwork)
library(colourpicker)
library(ProNet)
library(ggplotify)
library(survival)
library(survminer)
library(ComplexHeatmap)
library(circlize)
# library(ggplotify)
#library(reshape2)
# library(plotly)
# library(d3heatmap)
# library(future)
# library(promises)
ggthemr('flat')
usedcolors=swatch()
source('www/R/customerUI.R')
options(shiny.maxRequestSize = 1000*1024^2)
if(!dir.exists(paste(getwd(),'www/templePlot',sep="/")))
{
  dir.create(paste(getwd(),'www/templePlot',sep="/"))
}
# basicObj=reactiveValues()
# ensemblObj=reactiveValues()
# ## basicObj:保存运算需要的变量
# basicObj$rna.exp=""
# basicObj$micro.exp=""
# basicObj$target=""
# basicObj$geneinfo=""
# ## ensemblObj：保存ensembl需要的变量
# ensemblObj$ensembl=useMart(biomart='ensembl',dataset = 'hsapiens_gene_ensembl',host='www.ensembl.org',ensemblRedirect=T)
# ensemblObj$archieves=listEnsemblArchives()
# ensemblObj$specials=reactive({listDatasets(ensemblObj$ensembl)})
# ensemblObj$filters=reactive({listFilters(ensemblObj$ensembl)})
# ensemblObj$attributions=reactive({listAttributes(ensemblObj$ensembl)})
# ensemblObj$currentSpecial="hsapiens_gene_ensembl"
# ensemblObj$currentURL="www.ensembl.org"
# 
# updateEnsembl=function(special,url,session)
# {
#   print(paste(special,url))
#   if(ensemblObj$currentSpecial!=special|ensemblObj$currentURL!=url)
#   {
#     ensemblObj$currentSpecial=special
#     ensemblObj$currentURL=url
#     session$sendCustomMessage('filter_loading',list(div='modalbody',status='ongoing'))
#     ensemblObj$ensembl=useMart(biomart='ensembl',dataset = ensemblObj$currentSpecial,host = ensemblObj$currentURL)
#     print('connection finish')
#     session$sendCustomMessage('filter_loading',list(div='modalbody',status='finish'))
#     ensemblObj$specials=reactive({listDatasets(ensemblObj$ensembl)})
#     ensemblObj$filters=reactive({listFilters(ensemblObj$ensembl)})
#     ensemblObj$attributions=reactive({listAttributes(ensemblObj$ensembl)})
#     addAttribution(session)
#   }
# }
# 
# addAttribution=function(session)
# {
#   attr=ensemblObj$attributions()
#   group=unique(attr$page)
#   result=list()
#   for(g in group)
#   {
#     tmpattr=attr[which(attr$page==g),]
#     tmpattr=data.frame(id=paste(g,tmpattr$name,sep=":"),text=paste(tmpattr$name,tmpattr$description,sep=": "),stringsAsFactors = F)
#     dd=list(text=g,children=tmpattr)
#     result=c(result,list(dd))
#   }
#   result=list(results=result)
#   session$sendCustomMessage('attribution_list',toJSON(result,auto_unbox = T))
# }