node_property=c()
edge_property=c()
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