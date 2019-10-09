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
    panel=div(class="col-lg-4",id=paste("density_plot",type,task,sep="_"))
    plot_panel=c(plot_panel,list(panel))
  }
  body=div(class="box-body",plot_panel)
  box=div(class="box box-primary",id=paste("density_plot_",type,sep=""),header,body)
  all=div(class="col-lg-12",box)
  return(all)
}

draw_density=function(basepath,output,session,type,tasks)
{
  browser()
  for(task in tasks)
  {
    data=condition.values[[type]][[task]]
    if(all(rownames(data)==colnames(data)))
    {
      data=as.vector(data[upper.tri(data)])
    }
    else
    {
      data=as.vector(data)
    }
    figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
    print(figurepath)
    svg(figurepath)
    p=ggplot(data = data.frame(x=data))+geom_line(aes(x = x),stat='density')+geom_area(mapping = aes(x=x,y=stat(count)),stat='density')+
      labs(title = paste(type,task))
    print(p)
    dev.off()
    # #figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
    # output[[paste("#density_plot_",type,task,"image",sep="_")]]<- renderImage({
    #   list(src=figurepath,width="100%",height="100%")
    # },deleteFile = F)
    # #invalidateLater(100,)
  }
}