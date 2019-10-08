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

draw_density=function(basepath,output,session,type,tasks)
{
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
    insertUI(selector = paste("#density_plot",type,task,sep="_"),
             where = 'beforeEnd',
             ui = imageOutput(outputId = paste("#density_plot_",type,task,"image",sep="_"),width = "100%",height = "100%"),
             immediate = T,session = session
            )
    
    figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
    print(figurepath)
    svg(figurepath)
    p=ggplot(data = data.frame(x=data))+geom_density(aes(x = x))+labs(title = paste(type,task))
    print(p)
    dev.off()
  }
}