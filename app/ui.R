###############################
### Google Analytics - ui.R ###
###############################
source('www/R/input_tabUI.R')
source('www/R/construct_tabUI.R')
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
                                           div(class='col-lg-6 non-selected-wrapper',style="height:350px;overflow-y:auto",id='group_biotype',
                                               div(class='header',HTML('Groups'))
                                           ),
                                           div(class='col-lg-6 non-selected-wrapper',style="height:350px;overflow-y:auto",id='candidate_biotype',
                                               div(class='header',HTML('Candidate Biotypes'))
                                           )
                                       )
                                   )
                            ),
                             div(class='col-lg-6',
                                 imageOutput(outputId = 'biotype_group_statics_graph',height = "100%",width="100%")
                            ),
                            footer = tags$button(id = 'biotype_group_statics', type = "button",class = "btn btn-success action-button pull-right",HTML('Preview'),width='20')
                        )
                    )
)
construction_tab=tabItem(tabName = "construction",
                         h2("Step1: Choose Conditions",style='font-family:Georgia',
                            tags$button(tags$i(class='fa fa-plus-square'),HTML('Add New'),class='btn btn-success',id='add_new_condition')),
                         div(id='condition_panel')
                         
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
    tags$script(src='js/bootstrap-editable.js'),tags$script(src="js/process.js"),tags$script(src="js/construction.js")
    ),
  header=header,
  sidebar = sidebar,
  body=body
)