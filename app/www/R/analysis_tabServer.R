node_property=c()
edge_property=c()
modules=list()
module.configure=list()
moduleinfo=""
nodeNewInfo=""
default.configure=list(color="red",
                       color.attr="All",
                       layout="layout_nicely",
                       shape="circle",
                       shape.attr="All",
                       size=5,
                       label="")
clinical_data=""
survival_exp=""


create_property_box=function(type,id)
{
  id=sub(pattern = " ",replacement = "_",x = id)
  ui=div(class="col-lg-4",id=paste(type,"_",id,sep=""),
      div(class="box box-success",
          div(class="box-header with-border",
              h3(class="box-title",HTML(paste(toupper(type)," ",sub(pattern = "_",replacement = " ",x = toupper(id)),sep=""))),
              div(class="box-tools pull-right",
                  tags$button(class="btn btn-box-tool",type="button","data-widget"="collapse",
                              tags$i(class="fa fa-minus")
                  )
              )
          ),
          div(class="box-body",style="display:block;",
              imageOutput(outputId = paste(type,"_",id,"_plot",sep=""),width = "100%",height = "100%"),
              #tableOutput(outputId = paste(type,"_",id,"_table",sep=""))
              tags$button(class="btn bg-maroon btn-block btn-flat",type='button',HTML("Details"),
                          onclick=paste(ifelse(type=='node','node','edge'),"Details(this)",sep="")
                         )
          )
      )    
  )
  return(ui)
}




cluster_mcl=function(graph,expansion=2,inflation=2,allow1=F,max.iter=100)
{
  community=mcl(as.matrix(as_adjacency_matrix(graph,type='both')),addLoops = T,expansion = expansion,inflation = inflation,allow1 = allow1,max.iter = max.iter)
  result=community$Cluster
  names(result)=rownames(as_adjacency_matrix(graph,type='both'))
  return(result)
}

cluster_linkcomm=function(edgeinfo,hcmethod)
{
  community=getLinkCommunities(network = edgeinfo[,c("N1","N2")],hcmethod = hcmethod,directed = F,plot = F)
  result=community$nodeclusters
  result[,1]=as.character(result[,1])
  result[,2]=as.character(result[,2])
  colnames(result)=c('node','cluster')
  return(result)
}

cluster_mcode=function(graph,vwp,haircut,fluff,fdt)
{
  community=cluster(graph = graph,method = 'MCODE',vwp = vwp,haircut = haircut,fluff = fluff,fdt = fdt,plot = F)
  result=as.data.frame(community,stringsAsFactors=F)
  result=data.frame(node=rownames(result),cluster=result$community,stringsAsFactors = F)
  return(result)
}

cluster_cograph=function(netpath,outpath)
{
  browser()
  .jinit()
  .jaddClassPath(path = "Program/Cograph.jar")
  cograph=.jnew(class = 'com/xidian/Cograph/CographMining',normalizePath(netpath),normalizePath(outpath))
  .jcall(obj = cograph,returnSig = 'V',method = 'run')
}

create_module_info=function()
{
  moduleinfo<<-data.frame()
  for(community in names(modules))
  {
    module_genes=modules[[community]]
    subgraph=subgraph(graph = net_igraph,v = module_genes)
    node_count=length(module_genes)
    edge_count=gsize(subgraph)
    density=edge_count/(node_count*(node_count-1)/2)
   
    node_type_count=data.frame(count=rep(0,times=length(unique(after_slice_geneinfo$.group))),row.names = unique(after_slice_geneinfo$.group))
    nodetype=table(after_slice_geneinfo[module_genes,'.group'])
    node_type_count[names(nodetype),'count']=nodetype
    node_type_count=t(node_type_count)
    colnames(node_type_count)=paste(colnames(node_type_count),".count",sep="")
    rownames(node_type_count)=NULL
    
    tmpmicro=as.matrix(target[module_genes,rownames(after_slice_micro.exp)])
    subnet=as.matrix(as_adjacency_matrix(subgraph))
    subnet=subnet[module_genes,module_genes]
    path=tmpmicro%*%t(tmpmicro)*subnet
    ave.micro=sum(path)/sum(subnet)

    scores=list()
    for(con in condition$abbr[which(condition$used)])
    {
      module_edges=edgeinfo[edgeinfo$N1%in%module_genes&edgeinfo$N2%in%module_genes,]
      scores=c(scores,mean(module_edges[,con]))
    }
    names(scores)=paste0("Average.",condition$abbr[which(condition$used)])
    
    nodeDetails=paste("<a onclick=communityDetail('",community,"')>Details</a>",sep="")
    edgeDetails=paste("<a onclick=communityEdgeDetail('",community,"')>Details</a>",sep="")
    display=paste("<a href='#",paste("module_",community,sep=""),"' onclick=displayCommunity('",community,"')>Display</a>",sep="")
    moduleinfo<<-rbind(moduleinfo,data.frame("ModuleID"=community,"Node Count"=node_count,"Edge Count"=edge_count,node_type_count,
                                   Density=density,Average.MicroRNA=ave.micro,scores,
                                   Nodes=nodeDetails,Edges=edgeDetails,
                                   Visualization=display,stringsAsFactors = F))
  }
}

create_module_visualization=function(id)
{
  ui=div(class="col-lg-6",id=paste("module_",id,sep=""),
         div(class="box box-danger",
             div(class="box-header",
                 h4(id),
                 tags$button(id=paste(id,'_setting',sep=""),class="btn btn-default action-button btn-circle-sm dropdown-toggle shiny-bound-input",onclick="module_setting(this)",
                             tags$span(tags$i(class="fa fa-gear"))
                            )
             ),
             div(class="box-body",visNetworkOutput(outputId = paste(id,"_plot",sep=""),width = "100%",height = "500px"))
         )
     )
  return(ui)
}

create_modal_setting=function(id)
{
  valid_label_column=c("All")
  candidate_column=colnames(after_slice_geneinfo)
  label_column=candidate_column
  names(label_column)=label_column
  shapes=list("Label In"=c("ellipse","circle",'database',"box","text"),
              "Label Out"=c("circularImage","diamond","dot","star","triangle","triangleDown","square")
  )
  names(shapes[[1]])=shapes[[1]]
  names(shapes[[2]])=shapes[[2]]
  colorcandidate=list(
    conditionalPanel(condition="input.module_color=='All'",
                     div(class="row",
                         div(class="col-lg-2",colourInput(inputId = "All_color",label = "All",value = ifelse(module.configure[[id]]$color.attr=='All',module.configure[[id]]$color,'red')))
                     )
                    )
  )
  shapecandidate=list(
    conditionalPanel(condition="input.module_shape=='All'",
                     div(class="row",
                         div(class="col-lg-2",
                             selectInput(inputId = "All_shape",
                                         label = "All",choices = shapes,
                                         selected = ifelse(module.configure[[id]]$shape.attr=='All',module.configure[[id]]$shape,"circle")
                                         )
                         )
                     )
    )
 )
  
  
  
  
  for(column in candidate_column)
  {
    items=as.character(unique(after_slice_geneinfo[,column]))
    if(length(items)>100)
    {
      next
    }
    valid_label_column=c(valid_label_column,column)
    coloritems=list()
    shapeitems=list()
    if(column==module.configure[[id]]$color.attr)
    {
      for(item in items)
      {
        coloritems=c(coloritems,list(div(class="col-lg-2",
                                         colourInput(inputId = paste(item,"_color",sep=""),
                                                                      label = item,
                                                                      value = module.configure[[id]]$color[[item]]))))
      }
    }
    else
    {
      for(item in items)
      {
        coloritems=c(coloritems,list(div(class="col-lg-2",colourInput(inputId = paste(item,"_color",sep=""),label = item,value = "red"))))
      }
    }
    
    if(column==module.configure[[id]]$shape.attr)
    {
      for(item in items)
      {
        shapeitems=c(shapeitems,list(div(class="col-lg-2",
                                         selectInput(inputId = paste(item,"_shape",sep=""),label = item,
                                                     choices = shapes,
                                                     selected = module.configure[[id]]$shape[[item]]))))
      }
      
    }
    else
    {
      for(item in items)
      {
        shapeitems=c(shapeitems,list(div(class="col-lg-2",selectInput(inputId = paste(item,"_shape",sep=""),label = item,choices = shapes,selected = "circle"))))
      }
    }
    
    
    colorui=conditionalPanel(condition = paste("input.module_color=='",column,"'",sep=""),div(class="row",coloritems))
    shapeui=conditionalPanel(condition = paste("input.module_shape=='",column,"'",sep=""),div(class="row",shapeitems))
    
    colorcandidate=c(colorcandidate,list(colorui))
    shapecandidate=c(shapecandidate,list(shapeui))
  }
  names(valid_label_column)=valid_label_column
  ui=div(div(class="row",
             div(class="col-lg-4",
                 selectInput(inputId = "module_layout",label = "Layout",
                             choices =  c("Star"="layout_as_star","Tree"="layout_as_tree","Circle"="layout_in_circle","Nicely"="layout_nicely","Grid"="layout_on_grid","Sphere"="layout_on_sphere","Random"="layout_randomly","D.H. Algorithm"="layout_with_dh","Force Directed"="layout_with_fr","GEM"="layout_with_gem","Graphopt"="layout_with_graphopt","Kamada-Kawai"="layout_with_kk","Large Graph Layout"="layout_with_lgl","Multidimensional Scaling"="layout_with_mds","Sugiyama"="layout_with_sugiyama"),
                             selected = module.configure[[id]]$layout
                 )
                ),
             div(class="col-lg-4",
                 selectInput(inputId = "module_label",label = "Labels",choices = label_column,selected = module.configure[[id]]$label)
                )
        ),
        div(class="row",
            div(class="col-lg-4",
                selectInput(inputId = "module_color",label = "Color Map",choices = valid_label_column,selected = module.configure[[id]]$color.attr)
            ),
            div(class="col-lg-12",style="background-color:#F8F9F9",
                colorcandidate
            )
        ),
        div(class="row",
           div(class="col-lg-4",
               selectInput(inputId = "module_shape",label = "Shape Map",choices = valid_label_column,selected=module.configure[[id]]$shape.attr)
           ),
           div(class="col-lg-12",style="background-color:#F8F9F9",
               shapecandidate
           )
        )
  )
  return(ui)
}

create_progress=function(msg)
{
  ui=div(class='progress active',
         div(class='progress-bar progress-bar-primary progress-bar-striped',"aria-valuenow"="100","aria-valuemin"="0","aria-valuemax"="100",style="width:100%",
             tags$span(msg)
            )
        )
}

create_alert_box=function(header,msg,class){
  ui=div(class=paste("alert alert-info alert-dismissible",class),
         h4(tags$i(class="icon fa fa-info"),HTML(header)),
         HTML(msg)
         )
  return(ui)
}