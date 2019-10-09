condition=data.frame(abbr=c('PCC','LA','MS'),
                     used=F,
                     description=c('Pearson Correlation Coefficient','Liquid Association','MicroRNA Significance'),
                     core=0,
                     task="",
                     stringsAsFactors = F
                    )
rownames(condition)=condition$abbr
validcore=detectCores()
condition.values=list()

filter_box=function(type,tasks)
{
  title=h4(type)
  icon=tags$button(class="btn btn-box-tool",type="button","data-widget"="collapse",
                   tags$i(class="fa fa-minus")
                   )  

  tool=div(class="box-tools pull-right",icon)
  header=div(class="box-header with-border",title,tool)
  plot_panel=list()
  for(task in tasks)
  {
    panel=div(class="col-lg-4",id=paste("density_plot",type,task,sep="_"),type=type,task=task,style="border:2px solid #f4f4f4;")
    plot_panel=c(plot_panel,list(panel))
  }
  body=div(class="box-body",plot_panel)
  box=div(class="box box-primary",id=paste("density_plot_",type,sep=""),header,body)
  all=div(class="col-lg-12",box)
  return(all)
}

condition_density_plot=function(basepath,type,task,value,direction="<")
{
  #data
  data=condition.values[[type]][[task]]
  if(all(rownames(data)==colnames(data)))
  {
    data=as.vector(data[upper.tri(data)])
  }
  else
  {
    data=as.vector(data)
  }
  #operation
  direction=get(direction)
  
  valid=which(direction(data,value))
  text1=paste("Thresh:",value)
  text2=paste("Remain: ",round(length(valid)/length(data)*100,digits = 2),"%",sep="")
  density=density(x = data,from = min(data,na.rm = T),to = max(data,na.rm = T),na.rm = T)
  density=data.frame(x=density$x,y=density$y)
  density$area="false"
  density$area[which(direction(density$x,value))]='true'
  text=data.frame(label=c(text1,text2),x=max(density$x)*0.8,y=c(max(density$y),max(density$y)*0.93),stringsAsFactors = F)
  
  
  figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
  print(figurepath)
  svg(figurepath)
  p=ggplot(data = density)+geom_line(mapping = aes(x = x,y = y),size=1.5)+
    geom_area(mapping = aes(x = x,y=y,fill=area),alpha=0.8)+
    geom_vline(xintercept = value,size=1.2,colour=usedcolors[5],linetype="dashed")+
    scale_fill_manual(values = c("true"=usedcolors[6],"false"="#FFFFFF"))+
    labs(title = paste(type,task))+
    geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')+
    theme(legend.position = 'none',panel.background = element_rect(fill = NA))
  print(p)
  dev.off()
}
draw_density=function(basepath,output,session,type,tasks)
{
  for(task in tasks)
  {
    condition_density_plot(basepath = basepath,type = type,task = task,value=0)
    removeUI(selector = paste("#density_plot",type,task,"image",sep="_"),immediate = T)
    insertUI(selector = paste("#density_plot",type,task,sep="_"),where = "beforeEnd",
             ui = filter_bar(type,task),immediate = T)
    insertUI(selector = paste("#density_plot",type,task,sep="_"),
             where = 'beforeEnd',
             ui = imageOutput(outputId = paste("density_plot",type,task,"image",sep="_"),width = "100%",height = "100%"),
             immediate = T,session = session
    )
    Map(function(task){
      figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
      output[[paste("density_plot",type,task,"image",sep="_")]]<- renderImage({
        list(src=figurepath,width="100%",height="100%")
      },deleteFile = F)
    },tasks)
  }
}

filter_bar=function(type,task)
{
  direction=selectInput(inputId = paste("direction",type,task,sep="_"),label = "Direction",choices = list("<"="<",">"=">","<="="<=",">="=">=","=="="=="),multiple = F)
  thresh=div(class="input-group",id=paste("thresh",type,task,sep="_"),
             div(class="input-group-btn",
                 tags$button(class="btn btn-default",type="button",HTML("<i class='fa fa-minus'></i>"),onclick="step_change(this)")#paste("thresh_change('",type,"','",task,"')",sep=""))
                ),
             tags$input(class="form-control",type="text",value=0,style="text-align:center",onchange="thresh_change(this)"),
             div(class="input-group-btn",
                 tags$button(class="btn btn-default",type="button",HTML("<i class='fa fa-plus'></i>"),onclick="step_change(this)")#paste("thresh_change('",type,"','",task,"')",sep=""))
             )
            )
  step=tags$input(class="form-control",type="text",value=0.01,style="text-align:center")
  result=div(class="row",
             div(class="col-lg-3",direction),
             div(class="col-lg-3",tags$label(class="control-label",HTML("Step")),step),
             div(class="col-lg-6",tags$label(class="control-label",HTML("Thresh")),thresh)
             )
  return(result)
}