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