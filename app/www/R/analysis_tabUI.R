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