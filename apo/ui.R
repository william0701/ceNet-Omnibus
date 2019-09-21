###############################
### Google Analytics - ui.R ###
###############################
includeScript('www/js/all.js')
options(shiny.maxRequestSize = 1000*1024^2)
header=dashboardHeader(
  title='CeRNA Network Construction and Analysis',
  titleWidth=400
)
sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("1st Step: Data Input", tabName = "input", icon = icon("table",class = 'far'),badgeLabel = 3),
    menuItem("2nd Step: Data Preprocess", tabName = "process", icon = icon("cog"),badgeLabel = 3),
    menuItem("3rd Step: Network Construction", tabName = "construction", icon = icon("connectdevelop"),badgeLabel = 3),
    menuItem("4th Step: Network Visualization", tabName = "visualization", icon = icon("project-diagram"),badgeLabel = 3),
    menuItem("5th Step: Network Analysis", tabName = "analysis", icon = icon("chart-line"),badgeLabel = 3)
  ),width=400
)
input_tab= tabItem(tabName = "input",
                   h2("STEP1: Expression Input"),
                   fluidRow(
                     box(#第一部分
                         title = "Expression",collapsible = T,collapsed = F,status = 'success',solidHeader=T,footer = tags$button(id = 'express_preview', type = "button",class = "btn btn-success action-button pull-right",HTML('Preview'),width='20'), 
                         fileInput('ceRNA',label='mRNA/lncRNA/circRNA Expression'),
                         div(class='col-lg-6',style='display:inline-block;padding:0px',
                              prettyRadioButtons(inputId = 'ceRNA_seperator',label = 'Seprator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),status='success',inline=T)
                             ),
                         div(class='col-lg-6',style='display:inline-block;padding:0px',
                             textInput('ceRNA_seprator_cus',label='Custom Seprator')),
                         prettyRadioButtons(inputId = 'ceRNA_quote',label = 'Quote',choices = c("None"="",'Double Quote'='\"','Single Quote'='\''),selected='',status='success',inline=T),
                         prettyRadioButtons(inputId = 'ceRNA_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='success',inline=T),
                         prettyRadioButtons(inputId = 'ceRNA_row_col',label = 'Representation',choices = c("Rows For Genes"=T,'Columns For Genes'=F),selected=T,status='success',inline=T),
                         prettyRadioButtons(inputId = 'ceRNA_first_col',label = 'First Column For Row Name?',choices = c("Yes"=T,'No'=F),selected=F,status='success',inline=T),
                         #第二部分
                         tags$hr(),
                         fileInput('micro',label='MicroRNA Expression'),
                         div(prettyRadioButtons(inputId = 'micro_seperator',label = 'Seperator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),selected="\t",status='success',inline=T),style='display:inline-block;'),
                         div(textInput('micro_seprator_cus',label='Custom Seprator'),style='display:inline-block;padding-left:20px'),
                         prettyRadioButtons(inputId = 'micro_quote',label = 'Quote',choices = c("None"="",'Double Quote'='\"','Single Quote'='\''),selected='',status='success',inline=T),
                         prettyRadioButtons(inputId = 'micro_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='success',inline=T),
                         prettyRadioButtons(inputId = 'micro_row_col',label = 'Representation',choices = c("Rows For Genes"=T,'Columns For Genes'=F),selected=T,status='success',inline=T),
                         prettyRadioButtons(inputId = 'micro_first_col',label = 'First Column For Row Name?',choices = c("Yes"=T,'No'=F),selected=F,status='success',inline=T)
                         ),
                     box(title = "Expression Preview",collapsible = T,collapsed = F,status = 'success',solidHeader=T,
                         tabsetPanel(
                             #tabsetPanel(
                               tabPanel(title='CeRNA',
                  
                                          tableOutput('ceRNA_preview')
                                        
                               ),
                               tabPanel(title='MicroRNA',
                                        div(
                                          tableOutput('microRNA_preview')
                                        )
                               ),
                               id='Preview_panel'
                             )
                     )
                   ),
                   h2("STEP2: Target Input"),
                   fluidRow(
                     box(title = "MicroRNA Target",collapsible = T,collapsed = F,status = 'danger',solidHeader=T,footer = tags$button(id = 'target_preview', type = "button",class = "btn btn-danger action-button pull-right",HTML('Preview'),width='20'),
                         fileInput('target',label='MicroRNA Target'),
                         div(prettyRadioButtons(inputId = 'target_seperator',label = 'Seperator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),status='danger',inline=T),
                             style='display:inline-block;'),
                         div(textInput('target_seprator_cus',label='Custom Seprator'),style='display:inline-block;padding-left:20px'),
                         prettyRadioButtons(inputId = 'target_quote',label = 'Quote',choices = c("None"="",'Double Quote'='\"','Single Quote'='\''),selected='',status='danger',inline=T),
                         prettyRadioButtons(inputId = 'target_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='danger',inline=T)
                         ),
                     box(title = 'MicroRNA Target Preview',collapsible = T,collapsed = F,status = 'danger',solidHeader = T,
                         tableOutput('target_preview_panel')
                         )
                   ),
                   h2('STEP3: Gene Information Input'),
                   fluidRow(
                     box(title = "Gene Information",collapsible = T,collapsed = F,status = 'warning',solidHeader=T,footer = tags$button(id = 'geneinfo_preview', type = "button",class = "btn btn-warning action-button pull-right",HTML('Preview'),width='20'),
                         selectInput('geneinfo_source',label='Gene Information Source',choices = c('Biomart'='biomart','Custom'='custom'),selected='biomart'),
                         conditionalPanel(
                           condition='input.geneinfo_source=="biomart"',
                           div(class='col-lg-4',
                               textButtonInput(inputId = 'archieve',label = 'Ensembl Archieve',value='www.ensembl.org',buttonid = 'archieve_choose',status = 'warning',readonly = 'readonly'),
                               style='display:inline-block;padding:0px'
                           ),
                           div(class='col-lg-2'),
                           div(class='col-lg-4',
                             textButtonInput(inputId = 'database',label = 'Database(Species)',value='hsapiens_gene_ensembl',buttonid = 'database_choose',status = 'warning',readonly = 'readonly')
                           ),
                           tags$br(),
                           div(class='col-lg-4',style='padding:0px',
                             textButtonInput(inputId = 'filter',label = 'Input Gene Type',value='',buttonid='filter_choose',readonly = 'readonly',status = 'warning')                           
                           ),
                           tags$br(),
                           div(class='col-lg-12',style='padding:0px',
                                selectInput(inputId='attribution',label = 'Attribution',choices = "",selected = T,selectize = T)
                           ),
                           tags$br(),
                           div(class='col-lg-4',style='padding:0px',
                                selectInput('select_gene_source',label='Select Gene Source',choices = c('From Input File'='input','Custom'='custom'),selected='input')
                           ),
                           div(class='col-lg-2'),
                           div(class='col-lg-4',style='padding:0px',
                             conditionalPanel(condition = 'input.select_gene_source=="input"',
                                textButtonInput(inputId='gene',label = 'Select Genes',value = 'Current Selected Gene#: 0',buttonid = 'gene_choose',readonly = 'readonly',status='warning')
                             )
                           ),
                           conditionalPanel(condition = 'input.select_gene_source=="custom"',
                             div(class='col-lg-12',style="padding:0px",
                              textAreaInput(inputId = 'custom_select_gene',label = 'Custom Select Genes',resize = 'vertical',rows=8)  
                            )
                          )
                         ),
                         conditionalPanel(
                           condition='input.geneinfo_source=="custom"',
                           fileInput('geneinfo',label='Gene Information'),
                           div(prettyRadioButtons(inputId = 'geneinfo_seperator',label = 'Seperator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),status='warning',inline=T),
                               style='display:inline-block;'),
                           div(textInput('geneinfo_seprator_cus',label='Custom Seprator'),style='display:inline-block;padding-left:20px;'),
                           prettyRadioButtons(inputId = 'geneinfo_quote',label = 'Quote',choices = c("None"="",'Double Quote'='\"','Single Quote'='\''),selected='',status='warning',inline=T),
                           prettyRadioButtons(inputId = 'geneinfo_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='warning',inline=T),
                           tags$hr()
                         )
                         
                     ),
                     box(title = 'Gene Information Preview',collapsible = T,collapsed = F,status = 'warning',solidHeader = T,
                         tableOutput('geneinfo_preview_panel')
                     )
                   )
)
process_tab=tabItem(tabName = "process",
                    h2("Data Preprocess"),
                    fluidRow(
                      #box(title='Data Process',collapsible=T,collapsed=F,status='primary',solidHeader=T,width = 12,
                          value_BoxInput(value = 0,subtitle =  "Valid RNA", icon = "twitter",color = "red",width = 4,inputId="Rnaoutput" ),
                          value_BoxInput(value = 0,subtitle =  "Valid MicroRNA", icon = "twitter",color = "purple",width = 4,inputId="MicroRnaoutput" ),
                          value_BoxInput(value = 0,subtitle =  "Valid Sample", icon = "twitter",color = "yellow",width = 4,inputId="Sampleoutput" ),
                          box(title = "Info Map",status = 'success',solidHeader = F,width = 12,
                              div(class='col-lg-6',style="padding:0px",
                                  prettyRadioButtons(inputId = 'biotype_map',label = 'Which Column is Gene Biotype',choices = c('None'),selected = 'None',status='success',inline=T,shape = 'round'),
                                  #multiInput(inputId = 'valid_biotype',label = 'Select Used Biotype',choices = c('None'),selected = NULL,options = list(enable_search = T,non_selected_header = "Choose between:",selected_header = "You have selected:")),
                                  div(class="form-group",style='padding:0px',
                                       tags$label(class='control-label',HTML('Group Biotypes')),
                                       div(class='multi-wrapper',
                                           div(class='input-group margin',style='display:table;margin:0px',
                                               div(style='display:table-row',
                                                   div(class='input-group-btn',
                                                       tags$button(class='btn btn-success',type='button',id="add_group",
                                                                   HTML("<i class='fa fa-plus'></i> Add Group")
                                                                   )
                                                   ),
                                                   tags$input(class='form-control search-input',type='text',id='new_group_name')
                                               )
                                           )
                                       ),
                                       div(class='multi-wrapper',
                                           div(class='col-lg-6 non-selected-wrapper',style="height:500px;overflow-y:auto",id='group_biotype',
                                               div(class='header',HTML('Groups'))
                                           ),
                                           div(class='col-lg-6 non-selected-wrapper',style="height:500px;overflow-y:auto",id='candidate_biotype',
                                               div(class='header',HTML('Candidate Biotypes'))
                                           )
                                       )
                                   )
                            ),
                             div(class='col-lg-6',
                                 imageOutput(outputId = 'biotype_group_statics_graph',height = "100%")
                            ),
                            footer = tags$button(id = 'biotype_group_statics', type = "button",class = "btn btn-success action-button pull-right",HTML('Preview'),width='20')
                        )
                    )
)
construction_tab=tabItem(tabName = "construction",
                         h2("Network Construction")
)
visual_tab=tabItem(tabName = "visualization",
                   h2("Network Visualization")
)
analysis_tab=tabItem(tabName = "analysis",
                     h2("Network Analysis")
)

body=dashboardBody(
  tabItems(
    input_tab,
    process_tab,
    construction_tab,
    visual_tab,
    analysis_tab
  ),
  useSweetAlert()
)
dashboardPage(
  tags$head(
    tags$style(".shiny-input-container {margin-bottom: 0px} .shiny-file-input-progress {margin-bottom: 0px} .fa-spin {-webkit-animation:fa-spin 2s infinite linear;animation:fa-spin 2s infinite linear}"),
    tags$link(href = 'skins/all.css',rel="stylesheet"),tags$link(href='css/bootstrap-table.min.css',rel='stylesheet'),tags$link(href = 'css/select2.min.css',rel="stylesheet"),tags$link(href='css/select2-bootstrap-theme.css',rel="stylesheet"),
    tags$link(href = 'css/bootstrap-editable.css',rel="stylesheet"),tags$link(href='shinyWidgets/multi/multi.min.css',rel='stylesheet'),
    tags$script(src="js/all.js"),tags$script(src="js/icheck.min.js"),tags$script(src='js/bootstrap-table.min.js'),tags$script(src='js/select2.min.js'),tags$script(src='js/customerUI.js'),
    tags$script(src='js/bootstrap-editable.js'),tags$script(src="js/process.js")
    ),
  header=header,
  sidebar = sidebar,
  body=body
)