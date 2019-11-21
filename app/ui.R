###############################
### Google Analytics - ui.R ###
###############################
source('www/R/input_tabUI.R')
source('www/R/construct_tabUI.R')
source('www/R/analysis_tabUI.R')
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
                     div(class='col-lg-3',id='change_network_color'
                       
                      ),
                     div(class='col-lg-3',id='change_network_shape'),
                     div(class='col-lg-3',id='select_network_node',
                         h4("Select node",style="font-family:Georgia;font-weight:bold"),
                         div(class='input-group margin',style="margin:0px"
                         ) 
                      )
                   ),
                   div(id='cy')
                  
)
analysis_tab=tabItem(tabName = "analysis",
                     h2("Part1: Network Topology Properties",style='font-family:Georgia'),
                     div(class="box box-solid box-primary",
                         div(class="box-header",
                             h3(class="box-title"),
                             div(class="box-tools pull-right",
                                 tags$button(class='btn btn-box-tool',"data-widget"="collapse",
                                             tags$i(class='fa fa-minus')
                                 )
                              )
                         ),
                         div(class="box-body",
                             create_property_checkboxgroup(type='node',id='node_centrality',label='Nodes Centrality',
                                                           items=c("Degree","Betweenness","Closeness",'Clustering Coefficient'),f='showNodeCentrality'),
                             create_property_checkboxgroup(type='edge',id='edge_centrality',label='Edges Centrality',items=c("Betweenness"),f='showEdgeCentrality'),
                             tags$br(),
                             div(id="network_property",class="row")
                         )
                     ),
                     h2("Part2: Network Modules",style='font-family:Georgia'),
                     div(class="box box-solid box-primary",
                         div(class='box-header',
                             h3(class="box-title"),
                             div(class="box-tools pull-right",
                                 tags$button(class='btn btn-box-tool',"data-widget"="collapse",
                                             tags$i(class='fa fa-minus')
                                 )
                             )
                         ),
                         div(class='box-body',
                                  div(class="row",
                                      div(class="col-lg-4",
                                           pickerInput(inputId = 'community_algorithm',label = 'Community Detection Algorithm',
                                                       choices = c("NG Algorithm"="cluster_edge_betweenness",
                                                                   "Modularity Optimization"="cluster_fast_greedy",
                                                                   "Label Propagetion"="cluster_label_prop",
                                                                   "Eigenvectors Based"="cluster_leading_eigen",
                                                                   "Louvain Method"="cluster_louvain",
                                                                   "Maximal Modularity"="cluster_optimal",
                                                                   "Random Walk"="cluster_walktrap",
                                                                   "InfoMap"="cluster_infomap",
                                                                   "Cograph Community"="cluster_cograph",
                                                                   "Malcov Clustering"="cluster_mcl",
                                                                   "Linkcomm"="cluster_linkcomm",
                                                                   "MCODE"="cluster_mcode"
                                                                  ),
                                                       width = "100%"
                                           )
                                        ),
                                      div(class="col-lg-2",style="padding:0;margin:0",
                                           div(class="form-group shiny-input-container",
                                                tags$label(class="control-label"),tags$br(),
                                                tags$button(id="community_detection",class="btn btn-primary btn-flat action-button shiny-bound-input",HTML("Perform"),
                                                                    onclick="run_community_detection(this)"
                                               )
                                           )
                                       )
                                   ),
                                  div(class="row",
                                      div(class="col-lg-12",
                                   conditionalPanel("input.community_algorithm=='cluster_edge_betweenness'"),
                                   conditionalPanel("input.community_algorithm=='cluster_fast_greedy'"),
                                   conditionalPanel("input.community_algorithm=='cluster_label_prop'"),
                                   conditionalPanel("input.community_algorithm=='cluster_leading_eigen'"),
                                   conditionalPanel("input.community_algorithm=='cluster_louvain'"),
                                   conditionalPanel("input.community_algorithm=='cluster_optimal'"),
                                   conditionalPanel("input.community_algorithm=='cluster_walktrap'",
                                                    div(class="col-lg-4",style="padding-left:0",
                                                        numericInput(inputId="walktrap_step",label = "Steps",value = 4,min=1,max=NA,step = 1,width = "100%")
                                                    )
                                                   ),
                                   conditionalPanel("input.community_algorithm=='cluster_infomap'",
                                                    div(class="col-lg-4",style="padding-left:0",
                                                        numericInput(inputId="infomap_nb_trails",label = "Module Counts",value = 10,min=2,max=NA,step = 1)
                                                    )
                                                   ),
                                   conditionalPanel("input.community_algorithm=='cluster_cograph'"),
                                   conditionalPanel("input.community_algorithm=='cluster_mcl'",
                                                    div(class='row',
                                                        div(class='col-lg-2 col-md-6',
                                                            numericInput(inputId="mcl_expansion",label = "Expansion",value = 2,min = 1,max = Inf,step = 0.1,width = "100%")
                                                        ),
                                                        div(class='col-lg-2 col-md-6',
                                                            numericInput(inputId="mcl_inflation",label = "Inflation",value = 2,min = 1,max = Inf,step = 0.1,width = "100%")
                                                        ),
                                                        div(class='col-lg-2 col-md-6',
                                                            numericInput(inputId="mcl_max_iter",label = "Maximal Iteration",value = 100,min = 1,max = Inf,step = 1,width = "100%")
                                                        )
                                                    )
                                                   ),
                                   conditionalPanel("input.community_algorithm=='cluster_linkcomm'",
                                                    div(class='row',
                                                        div(class='col-lg-4',
                                                            pickerInput(inputId = "linkcomm_hcmethod",label = "Hierarchical Clustering Method ",
                                                                        choices = c("average"="average",'ward'='ward','single'='single','complete'='complete','mcquitty'='mcquitty','median'='median','centroid'='centroid'),
                                                                        selected='average',width="100%"
                                                                       )
                                                        )
                                                    )
                                                   ),
                                   conditionalPanel("input.community_algorithm=='cluster_mcode'",
                                                    div(class="row",
                                                        div(class="col-lg-2",
                                                            numericInput(inputId='mcode_vwp',label = "Vertex Weight Percentage",value = 0.5,min = 0,max = 1,step = 0.1,width = '100%')
                                                        ),
                                                        div(class="col-lg-2",
                                                            div(class='form-group shiny-input-container',
                                                                tags$label(HTML("Haircut")),
                                                                switchInput(inputId='mcode_haircut',value = F,onLabel = 'True',offLabel = "False")
                                                            )
                                                        )
                                                    ),
                                                    div(class="row",
                                                        div(class="col-lg-2",
                                                            div(class='form-group shiny-input-container',
                                                                tags$label(HTML("Fluff")),
                                                                switchInput(inputId='mcode_fluff',value = F,onLabel = 'True',offLabel = "False")
                                                            )                                                  
                                                        ),
                                                        conditionalPanel("input.mcode_fluff",
                                                            div(class="col-lg-2",
                                                                numericInput(inputId='mcode_fdt',label = "Cluster Density Cutoff",value = 0.8,min = 0,max = 1,step = 0.1,width = '100%')
                                                            )
                                                        )
                                                    )
                                                   ),
                                   tags$br()
                                      )
                                  ),
                             div(class="row",
                                 div(class="col-lg-12",
                                     tags$label(HTML("Module Summary")),
                                      div(id="module_info_box")
                                 )
                             ),
                             div(class='row',
                                 div(class="col-lg-12",
                                     tags$label(HTML("Module Visualization")),
                                     div(id="module_visualization",class='row')
                                 )
                             )
                            ),
                         
                         div(class='box-footer',
                             conditionalPanel('input.community_algorithm=="cluster_edge_betweenness"',
                                              tags$cite(class="bg-orange-active",HTML("* Newman M E J, Girvan M. Finding and evaluating community structure in networks[J]. Physical review E, 2004, 69(2): 026113."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_fast_greedy"',
                                              tags$cite(class="bg-orange-active",HTML("* Clauset A, Newman M E J, Moore C. Finding community structure in very large networks[J]. Physical review E, 2004, 70(6): 066111."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_label_prop"',
                                              tags$cite(class="bg-orange-active",HTML("* Raghavan U N, Albert R, Kumara S. Near linear time algorithm to detect community structures in large-scale networks[J]. Physical review E, 2007, 76(3): 036106.",style="font-weight:bold"))
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_leading_eigen"',
                                              tags$cite(class="bg-orange-active",HTML("* Newman M E J. Finding community structure in networks using the eigenvectors of matrices[J]. Physical review E, 2006, 74(3): 036104."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_louvain"',
                                              tags$cite(class="bg-orange-active",HTML("* Blondel V D, Guillaume J L, Lambiotte R, et al. Fast unfolding of communities in large networks[J]. Journal of statistical mechanics: theory and experiment, 2008, 2008(10): P10008."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_optimal"',
                                              tags$cite(class="bg-orange-active",HTML("* Brandes U, Delling D, Gaertler M, et al. On modularity clustering[J]. IEEE transactions on knowledge and data engineering, 2007, 20(2): 172-188."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_walktrap"',
                                              tags$cite(class="bg-orange-active",HTML("* Pons P, Latapy M. Computing communities in large networks using random walks[C]//International symposium on computer and information sciences. Springer, Berlin, Heidelberg, 2005: 284-293."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_infomap"',
                                              tags$cite(class="bg-orange-active",HTML("* Rosvall M, Bergstrom C T. Maps of information flow reveal community structure in complex networks[J]. arXiv preprint physics.soc-ph/0707.0609, 2007."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_cograph"',
                                              tags$cite(class="bg-orange-active",HTML("* Jia S, Gao L, Gao Y, et al. Defining and identifying cograph communities in complex networks[J]. New Journal of Physics, 2015, 17(1): 013044."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_mcl"',
                                              tags$cite(class="bg-orange-active",HTML("* Stijn van Dongen, Graph Clustering by Flow Simulation. PhD thesis, University of Utrecht, May 2000."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_linkcomm"',
                                              tags$cite(class="bg-orange-active",HTML("* Ahn Y Y, Bagrow J P, Lehmann S. Link communities reveal multiscale complexity in networks[J]. nature, 2010, 466(7307): 761."),style="font-weight:bold")
                             ),
                             conditionalPanel('input.community_algorithm=="cluster_mcode"',
                                              tags$cite(class="bg-orange-active",HTML("* Bader G D, Hogue C W V. An automated method for finding molecular complexes in large protein interaction networks[J]. BMC bioinformatics, 2003, 4(1): 2."),style="font-weight:bold")
                             )
                         )
                     ),


                     div(id="community_table"),
                     # h2("Part3: Biological Properties",style='font-family:Georgia'),
                     
                     # h2("Part3: Biological Properties",style='font-family:Georgia'),
                     h2("Part3: Enrichment Analysis",style='font-family:Georgia'),
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
                             div(class='row',
                                 div(class='col-lg-3',
                                     pickerInput(inputId = 'community_algorithm',label = 'Community Detection Algorithm',
                                                 choices = c("gProfile(Online)"="...",
                                                             "Cluster Profiler(Offline)"="..."
                                                 ),
                                                 width = "100%"
                                     )
                                 )
                             ),
                             div(class='row',
                                 div(class='col-lg-3',
                                     prettyRadioButtons(
                                       inputId = "Id036",
                                       label = "Choose:", 
                                       choices = c("Module analysis"="Module_analysis", "Custom input"="Custom_input"),
                                       inline = TRUE
                                     ),
                                     conditionalPanel("input.Id036=='Module_analysis'",
                                                      pickerInput(inputId = 'Module_analysis1',label = 'Module analysis',
                                                                  choices = c("NG Algorithm"="cluster_edge_betweenness",
                                                                              "Modularity Optimization"="cluster_fast_greedy"
                                                                  ),
                                                                  multiple = TRUE,
                                                                  width = "100%"
                                                      )
                                     ),
                                     conditionalPanel( "input.Id036=='Custom_input'",
                                                       textAreaInput("Custom_input1", "Custom input", "gene", width = "500px",height = "200px")
                                     )
                                 )
                             ),
                             
                             pickerInput(inputId = 'Significance_threshold',label = 'Significance threshold',
                                         choices = c("g:SCS threshold"="g:SCS_threshold",
                                                     "Bonferroni correction"="Bonferroni_correction",
                                                     "Benjamini-Hochberg FDR"="Benjamini-Hochberg_FDR"
                                         ),
                                         
                                         width = "50%"
                             ),
                             
                             textInput("User_threshold", "User threshold", "0.05",width="400px"),
                             
                             pickerInput(inputId = 'Numeric_IDs_treated_as',label = 'Numeric IDs treated as',
                                         
                                         choices =c(),
                                         width = "50%"
                             ),
                             
                             pickerInput(inputId = 'Data_Sources',label = 'Data Sources',
                                         choices = c("NG Algorithm"="cluster_edge_betweenness",
                                                     "Modularity Optimization"="cluster_fast_greedy"
                                                     
                                         ),
                                         multiple = TRUE,
                                         width = "50%"
                             )
                             
                         )
                     ),
                     h2("Part4: Survival Analysis",style='font-family:Georgia'),
                     div(class="box box-solid box-primary",
                         div(class="box-header",
                           h3(class="box-title"),
                           div(class="box-tools pull-right",
                               tags$button(class="btn btn-box-tool","data-widget"="collapse",
                                           tags$i(class="fa fa-minus")
                               )
                           )
                         ),
                         div(class="box-body",
                             tabsetPanel(type="tabs",
                                         tabPanel(title="Parameter",
                                                  div(class="col-lg-4",style="padding:0px",
                                                      tags$fieldset(tags$legend(HTML("Clinical Information")),
                                                                    div(class="row",
                                                                        div(class="col-lg-12",
                                                                            fileInput(inputId="clinical_file",label = "Clinical Data")
                                                                        ),
                                                                        div(class='col-lg-12',
                                                                            prettyRadioButtons(inputId = 'clinical_seperator',label = 'Seprator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),status='primary',inline=T)
                                                                        ),
                                                                        div(class="col-lg-12",
                                                                            prettyRadioButtons(inputId = 'clinical_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='primary',inline=T)
                                                                        )
                                                                    ),
                                                                    div(class="row",
                                                                        div(class="col-lg-12",
                                                                            selectInput(inputId = "survival_model",label = "Survival Model",
                                                                                        choices = c("Log Rank"="log.rank","Cox Model"="cox.model","Random Forest"="random.forest"))
                                                                        )
                                                                    )
                                                      )
                                                  ),
                                                  div(class="col-lg-5",
                                                      tags$fieldset(tags$legend(HTML("Expression Data")),
                                                                    div(class="row",
                                                                        div(class="col-lg-4",
                                                                            div(class="form-group shiny-input-container",
                                                                                tags$label(HTML("Use Current Expression?")),
                                                                                switchInput(inputId = "survival_exp_con",value = T,onLabel = "Yes",offLabel = "No")
                                                                            )
                                                                        ),
                                                                        conditionalPanel("!input.survival_exp_con",
                                                                          div(class="col-lg-6",
                                                                              fileInput(inputId="survival_exp_data",label = "Expression Data")
                                                                          )
                                                                        )
                                                                    ),
                                                                    conditionalPanel("!input.survival_exp_con",
                                                                        div(class="row",
                                                                            div(class='col-lg-12',
                                                                                prettyRadioButtons(inputId = 'survival_exp_seperator',label = 'Seprator',choices = c("Tab"="\t",'Common'=',','Space'=' ','Semicolon'=';'),status='primary',inline=T)
                                                                            ),
                                                                            div(class="col-lg-12",
                                                                                prettyRadioButtons(inputId = 'survival_exp_header',label = 'Header',choices = c("With header"=T,'Without header'=F),selected=T,status='primary',inline=T)
                                                                            )
                                                                        ),
                                                                        div(class="row",
                                                                            div(class="col-lg-12",
                                                                                selectInput(inputId = "survival_model",label = "Survival Model",
                                                                                            choices = c("Log Rank"="log.rank","Cox Model"="cox.model","Random Forest"="random.forest"))
                                                                            )
                                                                        )
                                                                    )
                                                      )
                                                  )
                                         ),
                                         tabPanel(title="Clinical Data Preview",div(id="clinical_data_preview",style="margin:10px")),
                                         tabPanel(title="Expression Data Preview",div(id="survival_exp_preview",style="margin:10px"))
                                        )
                         )

                     )
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