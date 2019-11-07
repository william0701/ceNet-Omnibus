###############################
### Google Analytics - ui.R ###
###############################
source('www/R/input_tabUI.R')
source('www/R/construct_tabUI.R')
source('www/R/analysis_tabServer.R')
includeScript('www/js/all.js')
options(shiny.maxRequestSize = 1000*1024^2)
header=dashboardHeader(
  title='CeNet Omnibus',
  titleWidth=280
)
sidebar=dashboardSidebar(
  sidebarMenu(
    menuItem("1st Step: Data Input", tabName = "input", icon = icon("table",class = 'far'),badgeLabel = 3),
    menuItem("2nd Step: Data Preprocess", tabName = "process", icon = icon("cog"),badgeLabel = 3),
    menuItem("3rd Step: Network Construction", tabName = "construction", icon = icon("connectdevelop"),badgeLabel = 3),
    menuItem("4th Step: Network Visualization", tabName = "visualization", icon = icon("project-diagram"),badgeLabel = 3),
    menuItem("5th Step: Network Analysis", tabName = "analysis", icon = icon("chart-line"),badgeLabel = 3)
  ),width=280
)

process_tab=tabItem(tabName = "process",
                    h2("Data Preprocess"),
                    div(class="row" ,id="float_banner",
                      #box(title='Data Process',collapsible=T,collapsed=F,status='primary',solidHeader=T,width = 12,
                          value_BoxInput(value = 0,subtitle =  "Valid RNA", icon = "twitter",color = "red",width = 4,inputId="Rnaoutput" ),
                          value_BoxInput(value = 0,subtitle =  "Valid MicroRNA", icon = "twitter",color = "purple",width = 4,inputId="MicroRnaoutput" ),
                          value_BoxInput(value = 0,subtitle =  "Valid Sample", icon = "twitter",color = "yellow",width = 4,inputId="Sampleoutput" )
                    ),      
                    fluidRow(      
                          box(title = "Info Map",status = 'success',solidHeader = F,width = 12,id="Info_Map_all",
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
                        ),
                        box(title = "Sample Filter",status = 'danger',solidHeader = F,width = 12,id="Sample_Filter_all"
                            
                        ),
                        box(title = "Gene Filter",status = 'danger',solidHeader = F,width = 12,id="Gene_Filter_all"
                            
                            ),
                        box(title = "Value Transform",status = 'danger',solidHeader = F,width = 12,id="Value_Transform_all",
                            div(class='btn-group',style="border:1px solid #ccc;margin:20px;",
                                h3("Logtransform Module",style="text-align:center;")
                            

                            ),
                            div(class='btn-group',style="border:1px solid #ccc;margin:20px;",
                                h3("Normalized Module",style="text-align:center;")

                            ),
                            footer = tags$button(id = 'Cancel_All_Trans', type = "button",class = "btn btn-success action-button pull-right",HTML('Cancel'),width='20')
                        )
                            
                    )
)
construction_tab=tabItem(tabName = "construction",
                         h2("Step1: Choose Conditions",style='font-family:Georgia'),
                         div(class='col-lg-12 callout callout-info',
                             tags$p(style="font-size:14px;font-family:sans-serif",
                                    HTML("Please choose conditions used for construct ceRNA network, e.g. Pearson Correlation(PCC), Shared MicroRNA Significance(MS), Liquid Association(LA).&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                                    tags$button(tags$i(class='fa fa-plus-square'),HTML('Add New'),class='btn btn-default',id='add_new_condition'))
                             
                         ),
                         fluidRow(
                                  div(id='condition_panel')
                         ),
                         h2("Step2: Condition Filter",style='font-family:Georgia'),
                         div(class='col-lg-12 callout callout-info',
                             tags$p(style="font-size:14px;font-family:sans-serif",
                                    HTML("Please choose threshold for every condition and every task.")
                             )
                         ),
                         fluidRow(
                                  div(id="condition_preview")
                         ),
                         h2("Step3: Network Evaluation",style='font-family:Georgia'),
                         div(class='col-lg-12 callout callout-info',
                             tags$p(style="font-size:14px;font-family:sans-serif",
                                    HTML("If you are constructing Human ceRNA network, you can evaluate this network with Experimental Validated ceRNA pairs by integrating LncCeRBase, LncACTdb and miRSponge!")
                             )
                         ),
                         fluidRow(
                           div(id="condition_preview")
                         )
                         
)
visual_tab=tabItem(tabName = "visualization",

                   h2("Network Visualization"),
                   div(class='row',
                     div(class='col-lg-2',
                      div( id="choose_differ_layout",class="form-group",
                        h4("choose layout",style="font-family:Georgia;font-weight:bold") 
                  
                      ),
                      div(class="input-group-btn",id="choose_differ_name",
                        h4("change gene name",style="font-family:Georgia;font-weight:bold"))
                      ),
                     div(class='col-lg-5',id='change_network_color'
                       
                      )
                   ),
                   div(id='cy')
                  
)
analysis_tab=tabItem(tabName = "analysis",
                     h2("Part1: Network Topology Properties",style='font-family:Georgia'),
                     create_property_checkboxgroup(type='node',id='node_centrality',label='Nodes Centrality',
                                                   items=c("Degree","Betweenness","Closeness",'Clustering Coefficient'),f='showNodeCentrality'),
                     create_property_checkboxgroup(type='edge',id='edge_centrality',label='Edges Centrality',items=c("Betweenness"),f='showEdgeCentrality'),
                     tags$br(),
                     div(id="network_property",class="row"),
                     h2("Part2: Network Modules",style='font-family:Georgia'),
                     div(class="box box-solid box-primary",
                         div(class='box-header',
                             h3(class="box-title",HTML("Parameters Selection")),
                             div(class="box-tools pull-right",
                                 tags$button(class='btn btn-box-tool',"data-widget"="collapse",
                                             tags$i(class='fa fa-minus')
                                 )
                             )
                         ),
                         div(class='box-body',
                             pickerInput(inputId = 'community_algorithm',label = 'Community Detection Algorithm',
                                         choices = c("NG Algorithm"="cluster_edge_betweenness",
                                                     "Modularity Optimization"="cluster_fast_greedy",
                                                     "Label Propagetion"="cluster_label_prop",
                                                     "Eigenvectors Based"="cluster_leading_eigen",
                                                     "Louvain Method"="cluster_louvain",
                                                     "Maximal Modularity"="cluster_optimal",
                                                     "Random Walk"="cluster_walktrap",
                                                     "InfoMap"="cluster_infomap",
                                                     "Cograph Community"="cluster_cograph"
                                                    ),
                                         width = "40%"
                             ),
                             
                         ),
                         div(class='box-footer',
                             conditionalPanel('input.community_algorithm=="cluster_edge_betweenness"',
                                              tags$cite(HTML("* Newman M E J, Girvan M. Finding and evaluating community structure in networks[J]. Physical review E, 2004, 69(2): 026113."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_fast_greedy"',
                                              tags$cite(HTML("* Clauset A, Newman M E J, Moore C. Finding community structure in very large networks[J]. Physical review E, 2004, 70(6): 066111."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_label_prop"',
                                              tags$cite(HTML("* Raghavan U N, Albert R, Kumara S. Near linear time algorithm to detect community structures in large-scale networks[J]. Physical review E, 2007, 76(3): 036106.",style="font-weight:bold"))
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_leading_eigen"',
                                              tags$cite(HTML("* Newman M E J. Finding community structure in networks using the eigenvectors of matrices[J]. Physical review E, 2006, 74(3): 036104."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_louvain"',
                                              tags$cite(HTML("* Blondel V D, Guillaume J L, Lambiotte R, et al. Fast unfolding of communities in large networks[J]. Journal of statistical mechanics: theory and experiment, 2008, 2008(10): P10008."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_optimal"',
                                              tags$cite(HTML("* Brandes U, Delling D, Gaertler M, et al. On modularity clustering[J]. IEEE transactions on knowledge and data engineering, 2007, 20(2): 172-188."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_walktrap"',
                                              tags$cite(HTML("* Pons P, Latapy M. Computing communities in large networks using random walks[C]//International symposium on computer and information sciences. Springer, Berlin, Heidelberg, 2005: 284-293."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_infomap"',
                                              tags$cite(HTML("* Rosvall M, Bergstrom C T. Maps of information flow reveal community structure in complex networks[J]. arXiv preprint physics.soc-ph/0707.0609, 2007."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_cograph"',
                                              tags$cite(HTML("* Jia S, Gao L, Gao Y, et al. Defining and identifying cograph communities in complex networks[J]. New Journal of Physics, 2015, 17(1): 013044."),style="font-weight:bold")
                             ),
                             tags$button(id="community_detection",class="btn btn-primary action-button pull-right shiny-bound-input",HTML("Perform"))
                         )
                     ),
                     div(id="community_table"),
                     h2("Part3: Biological Properties",style='font-family:Georgia')
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
    tags$link(href = 'css/bootstrap-editable.css',rel="stylesheet"),tags$link(href='shinyWidgets/multi/multi.min.css',rel='stylesheet'),tags$link(href='css/ion.rangeSlider.min.css',rel='stylesheet'),
    tags$script(src="js/all.js"),tags$script(src="js/icheck.min.js"),tags$script(src='js/bootstrap-table.min.js'),tags$script(src='js/select2.min.js'),tags$script(src='js/customerUI.js'),
    tags$script(src='js/bootstrap-editable.js'),tags$script(src="js/process.js"),tags$script(src="js/ion.rangeSlider.min.js"),
    tags$script(src="js/construction.js"),
    tags$script(src="js/filterProcess.js"),tags$script(src="js/samplefilterprocess.js"),
    tags$script(src="js/cytoscape.js"),tags$script(src='js/visualization.js'),
    tags$link(href ='css/network-table.css',rel="stylesheet"),
    tags$script(src="js/jscolor.js"),
    tags$script(src='js/analysis.js')
    ),
  header=header,
  sidebar = sidebar,
  body=body
)