node_property=c()
edge_property=c()
modules=list()
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

create_property_checkboxgroup=function(type,id,label,items,f)
{
  selection=list()
  for(i in items)
  {
    s=div(class="btn-group btn-goup-toggle",
        tags$button(class="btn checkbtn btn-primary",type=type,
                    tags$span(class="check-btn-icon-yes",tags$i(class='glyphicon glyphicon-ok')),
                    tags$span(class="check-btn-icon-no"),
                    tags$input(type='checkbox',autocomplete="off",name=paste(type,"_centrality",sep=""),value=i,HTML(i),onchange=paste(f,"(this)",sep=""))
        )
    )
    selection=c(selection,list(s))
  }
  ui=div(class='form-group shiny-input-container shiny-input-container-inline',
         tags$label(class='control-label',HTML(label)),
         tags$br(),
         div(id=id,class='checkboxGroupButtons shiny-bound-input',
             div(class='btn-group btn-group-container-sw',"data-toggle"="buttons","aria-label"="...",
                 selection
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