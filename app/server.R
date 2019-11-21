####################################
#### Google Analytics - server.R ###
####################################

print(options('shiny.maxRequestSize'))

shinyServer(function(input,output,session) {
  source('www/R/input_tabServer.R')
  source('www/R/construct_tabServer.R')
  source('www/R/analysis_tabServer.R')
  source('www/R/process_tabServer.R')
  ##创建临时文件???
  
  tmpdir=tempdir()
  basepath = paste(tmpdir,'/session_tmp_file',session$token,sep="");
  dir.create(path = basepath)
  print(paste("Session:",session$token,'is started!'))
  dir.create(paste(basepath,'Plot',sep="/"))
  dir.create(paste(basepath,'code',sep="/"))
  dir.create(paste(basepath,'data',sep="/"))
  dir.create(paste(basepath,'log',sep="/"))

  visual_layout=""
  
  load('testdata/ph1.RData',envir = globalenv())
  # rna.exp<<-rna.exp
  # geneinfo<<-geneinfo
  # micro.exp<<-micro.exp
  # target<<-target
  # select.gene<<-select.gene
  ############Input Page Action##########
  observeEvent(input$onclick,{
    isolate({msg=fromJSON(input$onclick)})
    if(msg$id=='express_preview')
    {
      ### ceRNA_preview
      session$sendCustomMessage('reading',list(div='ceRNA_preview',status='ongoing'))
      isolate({
        sep_cus=input$ceRNA_seprator_cus;
        sep=input$ceRNA_seperator;
        filepath=input$ceRNA$datapath;
        header=as.logical(input$ceRNA_header);
        quote=input$ceRNA_quote
        row=as.logical(input$ceRNA_row_col)
        rowname=as.logical(input$ceRNA_first_col)
      })
      if(sep_cus!="")
      {
        sep=sep_cus
      }
      if(is.null(filepath))
      {
        rna.exp<<-'No Data'
      }else
      {
        rna.exp<<-read.table(file = filepath,header = header,sep = sep,quote = quote,nrow=-1,stringsAsFactors = F)
      }
      if(!row)
      {
        rna.exp<<-t(rna.exp)
      }
      if(rowname)
      {
        rownames(rna.exp)<<-rna.exp[,1];
        rna.exp<<-rna.exp[,-1]
      }
      select.gene<<-rownames(rna.exp)
      Sys.sleep(2)
      session$sendCustomMessage('reading',list(div='ceRNA_preview',status='finish'))
      output$ceRNA_preview=renderTable({
        return(head(rna.exp,n = 20))
      },escape = F,hover=T,width='100%',bordered = T,striped=T,rownames=T,colnames=T,align='c')
      ### micro_preview
      session$sendCustomMessage('reading',list(div='microRNA_preview',status='ongoing'))
      isolate({
        sep_cus=input$micro_seprator_cus;
        sep=input$micro_seperator;
        filepath=input$micro$datapath;
        header=as.logical(input$micro_header);
        quote=input$micro_quote
        row=as.logical(input$micro_row_col)
        rowname=as.logical(input$micro_first_col)
      })
      if(sep_cus!="")
      {
        sep=sep_cus
      }
      if(is.null(filepath))
      {
        micro.ex<<-'No Data'
      }else
      {
        micro.exp<<-read.table(file = filepath,header = header,sep = sep,quote = quote,nrow=-1,stringsAsFactors = F)
      }
      if(!row)
      {
        micro.exp<<-t(micro.exp)
      }
      if(rowname)
      {
        rownames(micro.exp)<<-micro.exp[,1]
        micro.exp<<-micro.exp[,-1]
      }
      Sys.sleep(2)
      session$sendCustomMessage('reading',list(div='microRNA_preview',status='finish'))
      output$microRNA_preview=renderTable({
        return(head(micro.exp,n = 20))
      },escape = F,hover=T,width='100%',bordered = T,striped=T,rownames=T,colnames=T,align='c')
    }
    else if(msg$id=='target_preview')
    {
      session$sendCustomMessage('reading',list(div='target_preview_panel',status='ongoing'))
      isolate({
        sep_cus=input$target_seprator_cus;
        sep=input$target_seperator;
        filepath=input$target$datapath;
        header=as.logical(input$target_header);
        quote=input$target_quote
      })
      if(sep_cus!="")
      {
        sep=sep_cus
      }
      if(is.null(filepath))
      {
        target<<-'No Data'
      }else
      {
        target<<-read.table(file = filepath,header = header,sep = sep,quote = quote,stringsAsFactors = F)
      }
      Sys.sleep(2)
      session$sendCustomMessage('reading',list(div='target_preview_panel',status='finish'))
      output$target_preview_panel=renderTable({
        return(head(target,n = 20))
      },escape = F,hover=T,width='100%',bordered = T,striped=T,rownames=T,colnames=T,align='c')
    }
    else if(msg$id=='geneinfo_preview')
    {
      session$sendCustomMessage('reading',list(div='geneinfo_preview_panel',status='ongoing'))
      isolate({
        sep_cus=input$geneinfo_seprator_cus;
        sep=input$geneinfo_seperator;
        filepath=input$geneinfo$datapath;
        header=as.logical(input$geneinfo_header);
        quote=input$geneinfo_quote
      })
      if(sep_cus!="")
      {
        sep=sep_cus
      }
      if(is.null(filepath))
      {
        geneinfo<<-'No Data'
      }else
      {
        geneinfo<<-read.table(file = filepath,header = header,sep = sep,quote = quote,nrow=-1,stringsAsFactors = F)
      }
      Sys.sleep(2);
      session$sendCustomMessage('reading',list(div='geneinfo_preview_panel',status='finish'))
      output$geneinfo_preview_panel=renderTable({
        return(head(geneinfo,n = 20))
      },escape = F,hover=T,width='100%',bordered = T,striped=T,rownames=T,colnames=T,align='c')
    }
  })
  observeEvent(input$ensembl_info,{
    isolate({
      msg=fromJSON(input$ensembl_info)
    })
    print(msg)
    if(msg$id=='database_choose')
    {
      session$sendCustomMessage('ensembl_database_info',list(title='Database',body=toJSON(specials)))
    }
    else if(msg$id=='archieve_choose')
    {
      session$sendCustomMessage('ensembl_archieve_info',list(title='Archieve',body=toJSON(archieves)))
    }
    else if(msg$id=='filter_choose')
    {
      session$sendCustomMessage('ensembl_filter_info',list(title='Input Type',body=toJSON(filters)))
    }
    else if(msg$id=='gene_choose')
    {
      session$sendCustomMessage('select_gene',list(title='Gene',body=toJSON(list(all=data.frame(gene=rownames(rna.exp),stringsAsFactors = F),select=select.gene))))
    }
  })
  observeEvent(input$Update_Ensembl,{
    isolate({
      special=input$database
      url=input$archieve
    })
    print(paste(special,url))
    Sys.sleep(1)
    updateEnsembl(special,url,session)
  })
  observeEvent(input$attribution_update,{
    print('attribution_update')
    addAttribution(session)
  })
  observeEvent(input$Update_Select_Gene,{
    isolate({
      msg=input$Update_Select_Gene
    })
    select.gene<<-unlist(msg$select_gene)
  })
  observeEvent(input$sweetAlert,{
    isolate({
      msg=input$sweetAlert
    })
    sendSweetAlert(session = session,title = msg$title,text = msg$text,type = msg$type)
  })
  observeEvent(input$ensembl_gene_info,{
    isolate({
      msg=input$ensembl_gene_info
    })
    filter=msg$filter
    attr=unlist(msg$attr)
    attr=attr[which(attr!="")]
    attr=unlist(strsplit(x = attr,split = ":"))
    attr=attr[seq(2,length(attr),2)]
    gene=unlist(msg$gene)
    select.gene<<-gene
    attr=unique(c(filter,attr))
    session$sendCustomMessage('reading',list(div='geneinfo_preview_panel',status='ongoing'))
    feature=getBM(attributes = attr,filters = filter,values = select.gene,mart = ensembl)
    rownames(feature)=feature[,filter]
    session$sendCustomMessage('reading',list(div='geneinfo_preview_panel',status='finish'))
    geneinfo<<-feature
    output$geneinfo_preview_panel=renderTable({
      return(geneinfo)
    },escape = F,hover=T,width='100%',bordered = T,striped=T,rownames=T,colnames=T,align='c')
    # session$sendCustomMessage('geneinfo',toJSON(geneinfo))
  })
  #########Process Page Action########
  observeEvent(input$interclick,{
    if(is.character(rna.exp)){
      sendSweetAlert(session = session,title = "Error..",text = "ceRNA Not Exist!",type = "error");
      return();
    }
    else if(is.character(micro.exp)){
      sendSweetAlert(session = session,title = "Error..",text = "microRNA Not Exist!",type = "error");
      return();
    }
    else if(is.character(target)){
      sendSweetAlert(session = session,title = "Error..",text = "Target Not Exist!",type = "error");
      return();
    }
    else if(is.character(geneinfo)){
      sendSweetAlert(session = session,title = "Error..",text = "Geneinfo Not Exist!",type = "error");
      return();
    }
    else{
      sect_gene=intersect(rownames(rna.exp),rownames(target));
      sect_gene=intersect(sect_gene,rownames(geneinfo));
      sect_micro=intersect(rownames(micro.exp),names(target));
      sect_sample=intersect(names(rna.exp),names(micro.exp));
      sect_output_rna.exp<<-rna.exp[sect_gene,sect_sample];
      sect_output_micro.exp<<-micro.exp[sect_micro,sect_sample]
      sect_output_target<<-target[sect_gene,sect_micro];
      sect_output_geneinfo<<-geneinfo[sect_gene,]
      #sect_output_geneinfo$.group<<-NA
      after_slice_micro.exp<<-sect_output_micro.exp
      after_slice_rna.exp<<-sect_output_rna.exp
      after_slice_geneinfo<<-sect_output_geneinfo
      validNum1 = length(sect_gene);
      validNum2 = length(sect_micro);
      validNum3 = length(sect_sample);
      ValidNum = data.frame(rnaNum = validNum1,microNum = validNum2,sampleNum = validNum3,stringsAsFactors = F);
      session$sendCustomMessage('Valid-Num',ValidNum);
    }
  })
  observeEvent(input$process_showdetails,{
    isolate({
      msg=input$process_showdetails;
    })
    if(msg$id=='Rnaoutput'){
        obj=list(title='Valid ceRNA',details=toJSON(data.frame(detail= rownames(after_slice_rna.exp),stringsAsFactors =F)));
        session$sendCustomMessage('outdetails',obj);
    }
    if(msg$id=='MicroRnaoutput'){
       obj=list(title='Valid microRNA',details=toJSON(data.frame(detail= rownames(after_slice_micro.exp),stringsAsFactors =F)));
       session$sendCustomMessage('outdetails',obj);
    }
    if(msg$id=='Sampleoutput'){
    
       obj=list(title='Valid Sample',details=toJSON(data.frame(detail= intersect(names(after_slice_micro.exp),names(after_slice_rna.exp)),stringsAsFactors =F)));
       session$sendCustomMessage('outdetails',obj);
    }
  })
  observeEvent(input$Update_Biotype_Map,{
    choice=colnames(sect_output_geneinfo)
    choicenum=lapply(X = sect_output_geneinfo,FUN = unique)
    choicenum=lapply(X = choicenum,FUN = length)
    names(choicenum)=choice
    choicenum=unlist(choicenum)
    invalidchoice=names(choicenum)[which(choicenum>100)]
    if(biotype_map=="None")
    {
      updatePrettyRadioButtons(session = session,inputId = 'biotype_map',label = 'Which Column is Gene Biotype?',choices = choice,selected = names(sort(choicenum))[1],inline=T,prettyOptions=list(shape='round',status='success'))
    }
    else
    {
      updatePrettyRadioButtons(session = session,inputId = 'biotype_map',label = 'Which Column is Gene Biotype?',choices = choice,selected = biotype_map,inline=T,prettyOptions=list(shape='round',status='success'))
    }
    session$sendCustomMessage('invalidColumn',data.frame(choice=invalidchoice,stringsAsFactors = F))
  })
  observe({
    biotype_map<<-input$biotype_map
    if(biotype_map!='None')
    {
     
      choice=unique(geneinfo[,biotype_map])
      if(length(choice)>100)
      {
        sendSweetAlert(session = session,title = 'Warning...',text = 'Too Many Biotypes, Choose Carefully!',type = 'warning')
        #return()
      }
      session$sendCustomMessage('update_candidate_biotype',sort(choice))
    }
  })
  observeEvent(input$show_biotype_group,{
    isolate({
      biotype=input$biotype_map
      msg=input$show_biotype_group
      data=msg$data
    })
    sect_output_geneinfo$.group<<-NA
    for(group in names(data))
    {
      subset=unlist(data[[group]])
      sect_output_geneinfo[sect_output_geneinfo[,biotype] %in% subset,'.group']<<-group
    }
    output$biotype_group_statics_graph=renderImage({
      p=ggplot(data =sect_output_geneinfo)+geom_bar(mapping = aes_string(x = '.group',fill=biotype))+
        labs(title='Group Genes Statistics',x='Group',y='Gene Count')+
        theme(legend.position = 'bottom')
      # ggplotly(p) %>%
      #   layout(title = list(text="Gene Counts Statistics in Groups",font=list(family='serif')),
      #          legend = list(orientation = "h",font=list(family='Georgia')),
      #          autosize=T)
      svg(filename = paste(basepath,"Plot",'ph1.svg',sep="/"),family = 'serif')
      print(p)
      dev.off()
      print(normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")))
      list(src=normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")),height="100%",width="100%")    
    },deleteFile=F)
    after_slice_geneinfo <<- sect_output_geneinfo[which(!is.na(sect_output_geneinfo$.group)),]
    after_slice_rna.exp <<- sect_output_rna.exp[rownames(after_slice_geneinfo),]
    ValidNum = data.frame(rnaNum = length(rownames(after_slice_rna.exp)),stringsAsFactors = F);
    session$sendCustomMessage('Valid_valuebox_rna',ValidNum);
    session$sendCustomMessage('clear_construction_task',"")
    
  })
  
  observeEvent(input$Sample_Filter,{
    isolate({
      msg=input$Sample_Filter
      sep=msg$sep
      group=msg$group
      exist=msg$exist
      value=msg$value
      
    })
    
    if(group=="micro_invalid_name"){
    len_sep<-length(sep)

    if(len_sep==1){
      myfunc<-function(x){
        if(is.character(x)){
          x<-toupper(x)
        }
        # x<-as.character(x)
        sum(x!=sep[[1]])
      }
      expressgene_num<<-apply(sect_output_micro.exp, 2, myfunc)
      # expressgene_num<<-colSums(sect_output_micro.exp!=0) 
    }
    else if(len_sep==2){
      myfunc<-function(x){
        if(is.character(x)){
          x<-toupper(x)
        }
        # x<-as.character(x)
        sum(x!=sep[[1]]&x!=sep[[2]])
      }  
      expressgene_num<<-apply(sect_output_micro.exp, 2, myfunc)
     
    }
    else if(len_sep==3){
      myfunc<-function(x){
        if(is.character(x)){
          x<-toupper(x)
        }
        # x<-as.character(x)
        sum(x!=sep[[1]]&x!=sep[[2]]&x!=sep[[3]])
      }
      expressgene_num<<-apply(sect_output_micro.exp, 2, myfunc)
    }
    
    else if(len_sep==4){
      myfunc<-function(x){
        if(is.character(x)){
          x<-toupper(x)
        }
        # x<-as.character(x)
        sum(x!=sep[[1]]&x!=sep[[2]]&x!=sep[[3]]&x!=sep[[4]])
      }
      expressgene_num<<-apply(sect_output_micro.exp, 2, myfunc)
    }
    else{
      sendSweetAlert(session = session,title = "Warning..",text = 'Please choose valid value',type = 'warning')
      expressgene_num<<-rep(dim(sect_output_micro.exp)[1],time=dim(sect_output_micro.exp)[2])
    }
    
    expressgene_num<<-expressgene_num/(dim(sect_output_micro.exp)[1])
    process_sample<-data.frame(
      x=expressgene_num
    )
    
    value=as.numeric(value)
    draw_x<-(max(expressgene_num)+min(expressgene_num))/2  

    x2<-quantile(expressgene_num,value,type=3) 
    draw_x2<-round(x2,3)
    process_sample<-data.frame(
      x=expressgene_num,
      color=as.character(c(expressgene_num>x2)),stringsAsFactors = F
    )
    svg(filename = paste(basepath,"Plot","microSampleFilter.svg",sep = "/"),family = 'serif')

    liuxiasum<-length(colnames(sect_output_rna.exp[,which(expressgene_num>x2)]))
    
    text1=paste("Thresh:",draw_x2)
    text2=paste("Remain:",liuxiasum)
    
    p=ggplot(process_sample,aes(x=x,fill=..x..>x2))+
      geom_histogram(color="black",bins = 100) +
      scale_fill_manual(values=c('FALSE'="white",'TRUE'= "#9b59b6"))+
      geom_vline(aes(xintercept=x2), colour="#990000", linetype="dashed")
    pp=ggplot_build(p)
    axis_y<-get(x = "range",envir = pp$layout$panel_scales_y[[1]]$range)
    axis_x<-get(x = "range",envir = pp$layout$panel_scales_x[[1]]$range)
    x_pianyi=(axis_x[2]-axis_x[1])*0.2
    
    if(var(process_sample$x)!=0){
      if(skewness(process_sample$x)<0){
        text=data.frame(label=c(text1,text2),x=axis_x[1]+x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
      }
      else{
        text=data.frame(label=c(text1,text2),x=axis_x[2]-x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
      }
    }
    else{
      text=data.frame(label=c(text1,text2),x=axis_x[1]+x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
    }
    

    # p_test1=p+xlim(axis_x[1], axis_x[2])+
    #   geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')
    # p_test2=ggplot_build(p_test1)
    print(p+xlim(axis_x[1]-x_pianyi*5/150, axis_x[2]+x_pianyi*5/150)+
          geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')
            )

    
    # print(p+
    #         geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')
    # )
    dev.off()
    file.copy(from = paste(basepath,"Plot","microSampleFilter.svg",sep = "/"),
              to = paste('www/templePlot/microSampleFilter',session$token,'.svg',sep = ""))
    if(exist=="F"){
      print(paste("#","sample_Group_",group,'_panel',sep=""))
      insertUI(
        selector = paste("#","sample_Group_",group,'_panel',sep=""),
        where='beforeEnd',
        ui=imageOutput(outputId = paste(group,'_plot',sep=""),height = "100%"),
        immediate = T
      )
      
       insertUI(
         selector = paste("#","sample_Group_",group,'_panel',sep=""),
         where='beforeEnd',
         ui=div(class="box-footer",
                tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",
                            onclick=paste("slice('#","sample_Group_",group,"_panel')",sep=""),
                            style="margin:5px",height = "100%",HTML("Filter"))),
         immediate = T
       )
    }
    output[[paste(group,'_plot',sep="")]]=renderImage({
      list(src=paste('www/templePlot/microSampleFilter',session$token,'.svg',sep = ""),width="100%",height="100%")
    })
    }
    
    else if(group=="ce_invalid_name"){
      len_sep<-length(sep)
      # for(n in 1:len_sep){
      #   
      # }
      if(len_sep==1){
        myfunc<-function(x){
          if(is.character(x)){
            x<-toupper(x)
          }
          # x<-as.character(x)
          sum(x!=sep[[1]])
        }
        expressgene_num2<<-apply(sect_output_rna.exp, 2, myfunc)
        # expressgene_num2<<-colSums(sect_output_rna.exp!=0)

      }
      else if(len_sep==2){
        myfunc<-function(x){
          if(is.character(x)){
            x<-toupper(x)
          }
          # x<-as.character(x)
          sum(x!=sep[[1]]&x!=sep[[2]])
        }
        expressgene_num2<<-apply(sect_output_rna.exp, 2, myfunc)

      }
      else if(len_sep==3){
        myfunc<-function(x){
          if(is.character(x)){
            x<-toupper(x)
          }
          # x<-as.character(x)
          sum(x!=sep[[1]]&x!=sep[[2]]&x!=sep[[3]])
        }
        expressgene_num2<<-apply(sect_output_rna.exp, 2, myfunc)
      }

      else if(len_sep==4){
        myfunc<-function(x){
          if(is.character(x)){
            x<-toupper(x)
          }
          # x<-as.character(x)
          sum(x!=sep[[1]]&x!=sep[[2]]&x!=sep[[3]]&x!=sep[[4]])
        }
        expressgene_num2<<-apply(sect_output_rna.exp, 2, myfunc)
      }
      else{
        sendSweetAlert(session = session,title = "Warning..",text = 'Please choose valid value',type = 'warning')
        expressgene_num2<<-rep(dim(sect_output_rna.exp)[1],time=dim(sect_output_rna.exp)[2])
        
        process_sample<-data.frame(#also will crash the page
          x=expressgene_num2,stringsAsFactors = F
        )
      }
  
      expressgene_num2<<-expressgene_num2/(dim(sect_output_rna.exp)[1])
      value=as.numeric(value)
      draw_x<-(max(expressgene_num2)+min(expressgene_num2))/2  
      
      x2<-quantile(expressgene_num2,value,type=3)
      process_sample<-data.frame(
        x=expressgene_num2,
        color=as.character(c(expressgene_num2>x2)),stringsAsFactors = F
      )
      draw_x2<-round(x2,3)
      liuxiasum<-length(colnames(sect_output_rna.exp[,which(expressgene_num2>x2)]))
      svg(filename = paste(basepath,"Plot","RNASampleFilter.svg",sep = "/"),family = 'serif')
      
      text1=paste("Thresh:",draw_x2)
      text2=paste("Remain:",liuxiasum)
      
      p=ggplot(process_sample,aes(x=x,fill=..x..>x2))+
        geom_histogram(color="black",bins = 100) +
        scale_fill_manual(values=c('FALSE'="white",'TRUE'= "#9b59b6"))+
        geom_vline(aes(xintercept=x2), colour="#990000", linetype="dashed")
      pp=ggplot_build(p)
      axis_y<-get(x = "range",envir = pp$layout$panel_scales_y[[1]]$range)
      axis_x<-get(x = "range",envir = pp$layout$panel_scales_x[[1]]$range)
      x_pianyi=(axis_x[2]-axis_x[1])*0.2
      
      if(var(process_sample$x)!=0){
        if(skewness(process_sample$x)<0){
          text=data.frame(label=c(text1,text2),x=axis_x[1]+x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
        }
        else{
          text=data.frame(label=c(text1,text2),x=axis_x[2]-x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
        }
      }
      else{
        text=data.frame(label=c(text1,text2),x=axis_x[1]+x_pianyi,y=c(axis_y[2],axis_y[2]*0.95),stringsAsFactors = F)
      }
      # print(p+xlim(axis_x[1], axis_x[2])+
      #         geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')
      # )
      
      print(p+xlim(axis_x[1]-x_pianyi*5/150, axis_x[2]+x_pianyi*5/150)+
              geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif')
      )
      
      dev.off()
      file.copy(from = paste(basepath,"Plot","RNASampleFilter.svg",sep = "/"),to = paste('www/templePlot/RNASampleFilter',session$token,'.svg',sep = ""))
      if(exist=="F"){
        print(paste("#","sample_Group_",group,'_panel',sep=""))
        insertUI(
          selector = paste("#","sample_Group_",group,'_panel',sep=""),
          where='beforeEnd',
          ui=imageOutput(outputId = paste(group,'_plot',sep=""),height = "100%"),
          immediate = T
        )
        
        insertUI(
          selector = paste("#","sample_Group_",group,'_panel',sep=""),
          where='beforeEnd',
          ui=div(class="box-footer",
                 tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",
                             onclick=paste("slice('#","sample_Group_",group,"_panel')",sep=""),
                             style="margin:5px",height = "100%",HTML("Filter"))),
          immediate = T
        )
      }
      output[[paste(group,'_plot',sep="")]]=renderImage({
        list(src=paste('www/templePlot/RNASampleFilter',session$token,'.svg',sep = ""),width="100%",height="100%")
      })
    }
  
  })
  
  observeEvent(input$Sample_Slice_Signal,{
    isolate({
      msg=input$Sample_Slice_Signal
      group=msg$group
      line=msg$line
      line=as.numeric(line)
    })
    
    if(group=="sample_Group_micro_invalid_name_panel"){
      x2<-quantile(expressgene_num,line,type=3) 
      
      liuxiasum<-length(colnames(sect_output_micro.exp[,which(expressgene_num>x2)]))
      liuxiabaifenbi<-liuxiasum/length(colnames(sect_output_micro.exp))
      if(abs((1-line)-liuxiabaifenbi)<=0.05){
        after_slice_micro.exp<<-sect_output_micro.exp[,which(expressgene_num>x2)]
        intersect_sample_num<-length(intersect(colnames(after_slice_micro.exp),colnames(after_slice_rna.exp))) 
        sendSweetAlert(session = session,title = "Success..",text =paste0("Filter Ok! Sample Remain: ",intersect_sample_num) ,type = 'success')
        ValidNum = data.frame(sampleNum = length(colnames(after_slice_micro.exp)),stringsAsFactors = F);
        session$sendCustomMessage('Valid_valuebox_sample',ValidNum);
      }
      else{
        # print("tanchutishi") #tanchutishi..
        sendSweetAlert(session = session,title = "Warning..",text = 'Invlid value! Please choose again.',type = 'warning')
        # after_slice_micro.exp<<-sect_output_micro.exp
      }

    }
    else{
      x2<-quantile(expressgene_num2,line,type=3) 
      liuxiasum<-length(colnames(sect_output_rna.exp[,which(expressgene_num2>x2)]))
      liuxiabaifenbi<-liuxiasum/length(colnames(sect_output_rna.exp))
      if(abs((1-line)-liuxiabaifenbi)<=0.05){
        after_slice_rna.exp<<-sect_output_rna.exp[,which(expressgene_num2>x2)]
        intersect_sample_num<-length(intersect(colnames(after_slice_micro.exp),colnames(after_slice_rna.exp))) 
        sendSweetAlert(session = session,title = "Success..",text =paste0("Filter Ok! Sample Remain: ",intersect_sample_num) ,type = 'success')
        ValidNum = data.frame(sampleNum = length(colnames(after_slice_rna.exp)),stringsAsFactors = F);
        session$sendCustomMessage('Valid_valuebox_sample',ValidNum);
      }
      else{
        sendSweetAlert(session = session,title = "Warning..",text = 'Invlid value please choose again',type = 'warning')
        # after_slice_rna.exp<<-sect_output_rna.exp  
      }
    }
  })
  observeEvent(input$creatFilter_request,{
  
    level=unique(sect_output_geneinfo$.group)
    level=level[!is.na(level)]
    session$sendCustomMessage('gene_type_infomation',data.frame(group=level))
  
  })
  
  observeEvent(input$Gene_Filter_Signal,{
    isolate({
      msg=input$Gene_Filter_Signal
      group=msg$group
      group=sub(pattern = "gene_slice_value_",replacement = "",x = group)
      type=msg$type
      number=as.numeric(msg$number)
      exist=msg$exist
      line=msg$line
    })
    after_slice_micro.exp<<- sect_output_micro.exp[,colnames(after_slice_micro.exp)]
    after_slice_rna.exp<<- sect_output_rna.exp[,colnames(after_slice_rna.exp)]
    #paint picture
    if(type=="micro"){  
      validGene=rownames(after_slice_micro.exp)
      validSample = rowSums(after_slice_micro.exp>=number)
      ratio=as.numeric(line)
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_micro.exp)),stringsAsFactors = F)
      ypoint=length(which(xdata$SampleRatio<=ratio))/length(validGene)
      temp.data=data.frame(x=c(0,ratio),xend=c(ratio,ratio),y=c(ypoint,0),yend=c(ypoint,ypoint),stringsAsFactors = F)
      draw_x<-(max(xdata$SampleRatio)+min(xdata$SampleRatio))/2
      number_ori=length(validGene)
      number_after=(1-ypoint)*number_ori
      x1 = min(xdata$SampleRatio)+0.2*max(xdata$SampleRatio)+min(xdata$SampleRatio)
      text_to_plot=data.frame(x=c(x1,x1),y=c(0.95,0.90),col=c("blue","blue"),text=c(paste("Original genes:",number_ori),paste("After seg:",number_after)))
      ypoint =round(ypoint,2)
      
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_micro.exp)),
                         color=as.character(c(xdata$SampleRatio>ratio)),
                         stringsAsFactors = F
                         )
      
      svg(filename = paste(basepath,"Plot","microStatistic.svg",sep = "/"),family = 'serif')
      # print(ggplot(xdata, aes(x = SampleRatio))+stat_ecdf()+
      #         geom_hline(aes(yintercept=ypoint), colour="#990000", linetype="dashed")+
      #         geom_vline(aes(xintercept=ratio), colour="#990000", linetype="dashed")+
      #         geom_point(x=ratio,y=ypoint)+geom_text(label=paste0("(",ratio,",",ypoint,")"),x=draw_x ,y=0,colour = "red",family="serif",size=5)+
      #         geom_text(data = text_to_plot,aes(x = text_to_plot$x,y = text_to_plot$y,label =text_to_plot$text),size =8,colour="blue")
      #       )

      text1=paste("Thresh:",ratio)
      text2=paste("Remain:",number_after)
  
      p=ggplot(xdata,aes(x=SampleRatio,fill=..x..>ratio))+
              geom_histogram(color="black",bins = 100) +
              scale_fill_manual(values=c('FALSE'="white",'TRUE'= "#9b59b6"))+
              geom_vline(aes(xintercept=ratio), colour="#990000", linetype="dashed")
      pp=ggplot_build(p)
      draw_y<-get(x = "range",envir = pp$layout$panel_scales_y[[1]]$range)
      draw_x<-get(x = "range",envir = pp$layout$panel_scales_x[[1]]$range)
      x_pianyi=(draw_x[2]-draw_x[1])*0.2
      if(var(xdata$SampleRatio)!=0){
        if(skewness(xdata$SampleRatio)<0){
          text=data.frame(label=c(text1,text2),x=draw_x[1]+x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
        }
        else{
          text=data.frame(label=c(text1,text2),x=draw_x[2]-x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
        }
      }
      else{
        text=data.frame(label=c(text1,text2),x=draw_x[1]+x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
      }
      
      print(p+xlim(draw_x[1]-x_pianyi*5/150, draw_x[2]+x_pianyi*5/150)+
              geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif'))
      
      dev.off()
      file.copy(from = paste(basepath,"Plot","microStatistic.svg",sep = "/"),to = paste('www/templePlot/microStatistic',session$token,'.svg',sep = ""))

      if(exist=="F"){
        print(paste("#","gene_Group_",group,'_panel',sep=""))
        insertUI(
        selector = paste("#","gene_Group_",group,'_panel',sep=""),
        where='beforeEnd',
        ui=imageOutput(outputId = paste(group,'_plot',sep=""),height = "100%"),
        immediate = T
        )
        insertUI(
        selector = paste("#","gene_Group_",group,'_panel',sep=""),
        where='beforeEnd',
        ui=div(class="box-footer",
               tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",onclick=paste("slice_gene('#","gene_Group_",group,"_panel')",sep=""),style="margin:5px",height = "100%",HTML("Filter"))),
        immediate = T
      )
      }
      output[[paste(group,'_plot',sep="")]]=renderImage({
        list(src=paste('www/templePlot/microStatistic',session$token,'.svg',sep = ""),width="100%",height="100%")
      })
      #qiefen
      
    }
    else{
      validGene=rownames(sect_output_geneinfo[which(sect_output_geneinfo$.group==group),])
      validSample = rowSums(after_slice_rna.exp[validGene,]>=number)
      ratio=as.numeric(line)
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_rna.exp)),stringsAsFactors = F)
      ypoint=length(which(xdata$SampleRatio<=ratio))/length(validGene)
      temp.data=data.frame(x=c(0,ratio),xend=c(ratio,ratio),y=c(ypoint,0),yend=c(ypoint,ypoint),stringsAsFactors = F)
      draw_x<-(max(xdata$SampleRatio)+min(xdata$SampleRatio))/2
      number_ori=length(validGene)
      number_after=(1-ypoint)*number_ori
      ypoint =round(ypoint,2)
      svg(filename = paste(basepath,"/Plot/",group,"Statistic.svg",sep = ""),family = 'serif')
      x1 = min(xdata$SampleRatio)+0.2*max(xdata$SampleRatio)+min(xdata$SampleRatio)
      text_to_plot=data.frame(x=c(x1,x1),y=c(0.95,0.90),col=c("blue","blue"),text=c(paste("Original genes:",number_ori),paste("After seg:",number_after)))
      # print(ggplot(xdata, aes(x = SampleRatio))+stat_ecdf()+
      #         geom_hline(aes(yintercept=ypoint), colour="#990000", linetype="dashed")+
      #         geom_vline(aes(xintercept=ratio), colour="#990000", linetype="dashed")+
      #         geom_point(x=ratio,y=ypoint)+geom_text(label=paste0("(",ratio,",",ypoint,")"),x=draw_x ,y=0,colour = "red",family="serif",size=5)+
      #         geom_text(data = text_to_plot,aes(x = text_to_plot$x,y = text_to_plot$y,label =text_to_plot$text),size =8,colour="blue")
      #       )
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_micro.exp)),
                         color=as.character(c(xdata$SampleRatio>ratio)),
                         stringsAsFactors = F
      )
      
      text1=paste("Thresh:",ratio)
      text2=paste("Remain:",number_after)
      
      p=ggplot(xdata,aes(x=SampleRatio,fill=..x..>ratio))+
        geom_histogram(color="black",bins = 100) +
        scale_fill_manual(values=c('FALSE'="white",'TRUE'= "#9b59b6"))+
        geom_vline(aes(xintercept=ratio), colour="#990000", linetype="dashed")
      pp=ggplot_build(p)
      draw_y<-get(x = "range",envir = pp$layout$panel_scales_y[[1]]$range)
      draw_x<-get(x = "range",envir = pp$layout$panel_scales_x[[1]]$range)
      x_pianyi=(draw_x[2]-draw_x[1])*0.2
      
      if(var(xdata$SampleRatio)!=0){
        if(skewness(xdata$SampleRatio)<0){
          text=data.frame(label=c(text1,text2),x=draw_x[1]+x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
        }
        else{
          text=data.frame(label=c(text1,text2),x=draw_x[2]-x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
        }
      }
      else{
        text=data.frame(label=c(text1,text2),x=draw_x[1]+x_pianyi,y=c(draw_y[2],draw_y[2]*0.95),stringsAsFactors = F)
      }
      
      print(p+xlim(draw_x[1]-x_pianyi*5/150, draw_x[2]+x_pianyi*5/150)+
              geom_text(mapping = aes(x = x,y = y,label=label),data=text,size=6,family='serif'))
      
      
      dev.off()
      file.copy(from = paste(basepath,"/Plot/",group,"Statistic.svg",sep = ""),to = paste('www/templePlot/',group,'Statistic',session$token,'.svg',sep = ""))
      
      if(exist=="F"){
        print(paste("#","gene_Group_",group,'_panel',sep=""))
        insertUI(
          selector = paste("#","gene_Group_",group,'_panel',sep=""),
          where='beforeEnd',
          ui=imageOutput(outputId = paste(group,'_plot',sep=""),height = "100%"),
          immediate = T
        )
        insertUI(
          selector = paste("#","gene_Group_",group,'_panel',sep=""),
          where='beforeEnd',
          ui=div(class="box-footer",
                 tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",onclick=paste("slice_gene('#","gene_Group_",group,"_panel')",sep=""),style="margin:5px",height = "100%",HTML("Filter"))),
          immediate = T
        )
      }
      output[[paste(group,'_plot',sep="")]]=renderImage({
        list(src= paste('www/templePlot/',group,'Statistic',session$token,'.svg',sep = ""),width="100%",height="100%")
      })
    }
  })
  observeEvent(input$Gene_Slice_Signal,{
    isolate({
      msg = input$Gene_Slice_Signal
      group = msg$group
      group=sub(pattern = "gene_Group_",replacement = "",x = group)
      group=sub(pattern = "_panel",replacement = "",x = group)
      number=as.numeric(msg$number)
      type = msg$type
      line = msg$line
      line = as.numeric(line)
      first = msg$first
    })

    if(type=="micro"){
      validGene=rownames(after_slice_micro.exp)
      validSample = rowSums(after_slice_micro.exp>=number)
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_micro.exp)),stringsAsFactors = F)
      intersect_name = rownames(xdata)[which(xdata$SampleRatio>line)]
      ratio = sum(xdata$SampleRatio==line)/length(validGene)
      if(ratio<0.05){
        after_slice_micro.exp<<- after_slice_micro.exp[intersect_name,]
        num1 = length(rownames(after_slice_micro.exp))
        sendSweetAlert(session = session,title = "Success..",text = paste("Filter Success! Valid microRNA Remain:",num1),type = 'success')
        ValidNum = data.frame(microNum = length(intersect_name),stringsAsFactors = F);
        session$sendCustomMessage('Valid_valuebox_micro',ValidNum);
      }
      else{
        sendSweetAlert(session = session,title = "Warning..",text = 'Invlid value please choose again',type = 'warning')
      }
    }
    else{
      #append group gene to after_slice_rna.exp
      validGene=rownames(sect_output_geneinfo[which(sect_output_geneinfo$.group==group),])
      validSample = rowSums(after_slice_rna.exp[validGene,]>=number)
      xdata = data.frame(SampleRatio=validSample/length(colnames(after_slice_rna.exp)),stringsAsFactors = F)
      intersect_name = rownames(xdata)[which(xdata$SampleRatio>line)]
      ratio = sum(xdata$SampleRatio==line)/length(validGene)
      if(ratio<0.05){
        delete = setdiff(rownames(after_slice_rna.exp),validGene)
        remain = after_slice_rna.exp[intersect_name,]
        after_slice_rna.exp<<-after_slice_rna.exp[delete,]
        after_slice_rna.exp<<-rbind(after_slice_rna.exp,remain)
        after_slice_geneinfo<<-after_slice_geneinfo[rownames(after_slice_rna.exp),]
        num1 = length(rownames(after_slice_rna.exp))
        ValidNum = data.frame(rnaNum = length(rownames(after_slice_rna.exp)),stringsAsFactors = F);
        sendSweetAlert(session = session,title = "Success..",text = paste("Filter Success! Valid ceRNA Remain:",num1),type = 'success')
        session$sendCustomMessage('Valid_valuebox_rna',ValidNum);
      }
      else{
        sendSweetAlert(session = session,title = "Warning..",text = 'Invlid value please choose again',type = 'warning')
      }
    }
    #   list(src=normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")),height="100%",width="100%")    
    # },deleteFile=F)
    session$sendCustomMessage('clear_construction_task',"")
  })

  observeEvent(input$Value_Transform_Signal,{
    isolate({
      msg = input$Value_Transform_Signal
      opera = msg$opera
    })
    inter_sample = intersect(colnames(after_slice_micro.exp),colnames(after_slice_rna.exp))
    after_slice_micro.exp<<-after_slice_micro.exp[,inter_sample]
    after_slice_rna.exp<<-after_slice_rna.exp[,inter_sample]
    if(opera=="log2"){
      after_slice_rna.exp = log2(after_slice_rna.exp)
      after_slice_micro.exp = log2(after_slice_micro.exp)
      sendSweetAlert(session = session,title = "Success..",text = "Successful Log2 Operation",type = 'success')
    }
    else if(opera=="log"){
      after_slice_rna.exp = log(after_slice_rna.exp)
      after_slice_micro.exp = log(after_slice_micro.exp)
      sendSweetAlert(session = session,title = "Success..",text = "Successful Loge Operation",type = 'success')
    }
    else if(opera=="log10"){
      after_slice_rna.exp = log10(after_slice_rna.exp)
      after_slice_micro.exp = log10(after_slice_micro.exp)
      sendSweetAlert(session = session,title = "Success..",text = "Successful Log10 Operation",type = 'success')
    }
  })
  observeEvent(input$Normalized_Signal,{
    isolate({
      msg=input$Normalized_Signal
      opera=msg$opera
    })
    if(opera=="Min_Max_scaling"){
      action=function(x){
        (x-min(x))/(max(x)-min(x))
      }
      after_slice_rna.exp <<- t(apply(after_slice_rna.exp, 1, action))
      after_slice_micro.exp <<- t(apply(after_slice_micro.exp, 1, action))
      sendSweetAlert(session = session,title = "Success..",text = "Successful Min_Max_scaling Operation",type = 'success')
    }
    else if(opera=="Zero_Mean_normalization"){
      action=function(x){
        (x-mean(x))/sd(x)
      }
      after_slice_rna.exp <<- t(apply(after_slice_rna.exp, 1, action))
      after_slice_micro.exp <<- t(apply(after_slice_micro.exp, 1, action))
      sendSweetAlert(session = session,title = "Success..",text = "Successful Zero_Mean Operation",type = 'success')
    }
  })
  observeEvent(input$Cancel_All_Trans,{
    colnamerna = colnames(after_slice_rna.exp)
    rownamerna = rownames(after_slice_rna.exp)
    colnamemicro = colnames(after_slice_micro.exp)
    rownamemicro = rownames(after_slice_micro.exp)
    after_slice_rna.exp<<-sect_output_rna.exp[rownamerna,colnamerna]
    after_slice_micro.exp<<-sect_output_micro.exp[rownamemicro,colnamemicro]
    sendSweetAlert(session = session,title = "Success..",text = "Successful Cancel Transform Opera",type = 'success')
  })
  #Construction Page Action

  #########Construction Page Action########
  observeEvent(input$construction_data_confirm,{
    samples=intersect(colnames(after_slice_rna.exp),colnames(after_slice_micro.exp))
    gene=intersect(rownames(after_slice_rna.exp),rownames(after_slice_geneinfo))
    after_slice_geneinfo<<-after_slice_geneinfo[gene,]
    after_slice_rna.exp<<-after_slice_rna.exp[gene,samples]
    after_slice_micro.exp<<-after_slice_micro.exp[,samples]
  })

  observeEvent(input$add_new_condition,{
    isolate({
      msg=input$add_new_condition
      core=input$use_core
    })
    choice=c(condition[which(!condition$used),'abbr'],'custom')
    if(length(choice)>1)
      names(choice)=c(paste(condition[which(!condition$used),'description'],'(',condition[which(!condition$used),'abbr'],')',sep=""),'Custom')
    else
      names(choice)='Custom'
    
    if(all(is.na(after_slice_geneinfo$.group)))
    {
      after_slice_geneinfo$.group<<-'Default'
      sendSweetAlert(session = session,title = "Warning",text = 'Group All Genes in Defaut',type = 'warning')
    }
    groupstaistic=as.data.frame(table(after_slice_geneinfo$.group))
    rownames(groupstaistic)=groupstaistic$Var1
    pairs=data.frame(v1=rep(groupstaistic$Var1,times=dim(groupstaistic)[1]),v2=rep(groupstaistic$Var1,each=dim(groupstaistic)[1]),stringsAsFactors = F)
    pairs=unique(t(apply(X = pairs,MARGIN = 1,FUN = sort)))
    show=paste(pairs[,1],'(',groupstaistic[pairs[,1],'Freq'],') vs ',pairs[,2],'(',groupstaistic[pairs[,2],'Freq'],')',sep="")
    values=paste(pairs[,1],"---",pairs[,2],sep="")
    show=c('All',show)
    values=c('all',values)
    cores=seq(0,validcore-sum(condition$core))
    
    if(length(msg)>1)
    {
      choice=msg$type
      cores=seq(0,validcore-sum(condition$core)+condition[msg$type,'core'])
      type=msg$type
      tasks=msg$tasks
    }
    else
    {
      type=choice[1]
      core=cores[1]
      tasks='all'
    }
    
    removeUI(selector = '#modalbody>',multiple = T,immediate = T)
    insertUI(selector = '#modalbody',where = 'beforeEnd',immediate = T,
             ui=div(
               div(class='row',
                   div(class='col-lg-6',
                       selectInput(inputId = 'condition_type',label = 'Choose New Condition',choices = choice,multiple = F,selected = type)
                   ),
                   div(class='col-lg-6',
                       selectInput(inputId = 'use_core',label = 'Choose Parallel Cores',choices = cores ,multiple = F,selected = as.character(core))
                   )
               ),
               div(class='row',
                   div(class="col-lg-12",
                       multiInput(inputId = 'group_pairs',label = 'Group Pairs',choiceNames = show,choiceValues = values,selected = tasks,width = "100%")
                   )
               ),
               conditionalPanel(condition = 'input.condition_type=="custom"',
                                hr(),
                                div(class='row',
                                    div(class='col-lg-3 col-xs-12',
                                        textInput(inputId = 'custom_condition_description',label = 'New Condition Full Name')
                                    ),
                                    div(class='col-lg-3 col-xs-12',
                                        textInput(inputId = 'custom_condition_abbr',label = 'New Condition Abbreviation')
                                    ),
                                    # ),
                                    # div(class="row",
                                    div(class='col-lg-6 col-xs-12',
                                        div(class='form-group',
                                            tags$label(HTML('Available Variables')),
                                            tags$ul(class='form-control',style="border-color:#fff;padding:0px",
                                                    tags$li(tags$i(class='fa fa-tag text-light-blue'),HTML('rna.exp'),style="display:inline-block;padding-left:0px;padding-right:5px"),
                                                    tags$li(tags$i(class='fa fa-tag text-light-blue'),HTML('micro.exp'),style="display:inline-block;padding-left:0px;padding-right:5px"),
                                                    tags$li(tags$i(class='fa fa-tag text-light-blue'),HTML('target'),style="display:inline-block;padding-left:0px;padding-right:5px")
                                            )
                                        )
                                    )
                                ),
                                div(class='row',
                                    div(class='col-lg-12',
                                        textAreaInput(inputId = 'custom_condition_code',label = 'New Condition Function',rows = 20,placeholder = 'Please paste the calculate function of the new condition...',width='100%',resize='both')
                                    )
                                )
               )
             )
    )
    session$sendCustomMessage('conditions',condition)
  })
  observeEvent(input$choose_new_condition,{
    isolate({
      msg=input$choose_new_condition
      core=as.numeric(msg$core)
      tasks=msg$tasks
      tasks=paste(unlist(tasks),collapse = ";")
      type=msg$type
      description=input$custom_condition_description
      abbr=input$custom_condition_abbr
      code=input$custom_condition_code
    })
    if(type=='custom')
    {
      condition<<-rbind(condition,data.frame(description=description,abbr=abbr,used=T,core=core,task=tasks,stringsAsFactors = F))
      rownames(condition)<<-condition$abbr
      write(x = code,file = paste(basepath,"/code/",abbr,'.R',sep=""))
      #thresh<<-rbind(thresh,data.frame(type=condition$abbr,task=tasks,direction="<",thresh=0,stringsAsFactors = F))
    }
    else
    {
      condition[type,'used']<<-T
      condition[type,'core']<<-core
      condition[type,'task']<<-tasks
      #thresh<<-rbind(thresh,data.frame(type=condition$abbr,task=tasks,direction="<",thresh=0,stringsAsFactors = F))
    }
  })
  observeEvent(input$remove_condition,{
    isolate({
      msg=input$remove_condition
    })
    condition[msg$type,'used']<<-F
    condition[msg$type,'core']<<-0
    thresh<<-thresh[thresh$type!=msg$type,]
    removeUI(selector = paste("div.col-lg-12 > #density_plot_",msg$type,sep=""),immediate = T)
  })
  observeEvent(input$compute_condition,{
    isolate({
      type=input$compute_condition$type
    })
    core=condition[type,'core']
    
    tasks=condition[type,'task']
    logpath=paste(basepath,'/log/',type,'.txt',sep="")
    if(file.exists(logpath))
    {
      file.remove(logpath)
    }
    if(type=="PCC")
    {
      if(dir.exists(paths = paste(basepath,'/log/')))
      {
        dir.create(paths = paste(basepath,'/log/'),recursive = T)
      }
      print('start')
      session$sendCustomMessage('calculation_eta',list(type=type,task="all",msg="Data Prepare",status='run'))
      filepath=paste(basepath,"/data/rna.exp.mat",sep="")
      writeMat(con=filepath,x=as.matrix(after_slice_rna.exp))
      system(paste("www/Program/COR.exe",filepath,basepath,"all",sep=" "),wait = F)
    }
    else
    {
      if(dir.exists(paths = paste(basepath,'/log/')))
      {
        dir.create(paths = paste(basepath,'/log/'),recursive = T)
      }
      file.create(logpath)
      print('start')
      session$sendCustomMessage('calculation_eta',list(type=type,task="all",msg="Data Prepare",status='run'))
      datapath=paste(basepath,"/data/tmpdatas.RData",sep="")
      scriptpath="www/Program/ComputeCondition.R"
      codepath=""
      resultpath=paste(basepath,'/',type,'.RData',sep="")
      if(file.exists(paste(basepath,'/code/',type,'.R',sep="")))
      {
        codepath=paste(basepath,'/code/',type,'.R',sep="")
      }
      else if(file.exists(paste('www/Program/',type,'.R',sep="")))
      {
        codepath=paste('www/Program/',type,'.R',sep="")
      }
      else
      {
        sendSweetAlert(session = session,title = "Error..",text = "No Code",type = 'error')
      }
      
      rna.exp=after_slice_rna.exp
      micro.exp=after_slice_micro.exp
      target=sect_output_target[rownames(rna.exp),rownames(micro.exp)]
      geneinfo=after_slice_geneinfo
      save(rna.exp,micro.exp,target,geneinfo,file = datapath)
      print(paste("Rscript",scriptpath,datapath,codepath,type,core,logpath,tasks))
      system(paste("Rscript",scriptpath,datapath,codepath,type,core,logpath,tasks,resultpath),wait = F)
    }
  })
  observeEvent(input$compute_status,{
    isolate({
      msg=input$compute_status
      type=msg$type
    })
    time=function(s)
    {
      s=floor(s)
      out=""
      if(s>=86400)
      {
        out=paste0(out,floor(s/86400),'d')
        s=s%%86400
      }
      if(s>=3600)
      {
        out=paste0(out,floor(s/3600),'h')
        s=s%%3600
      }
      if(s>=60)
      {
        out=paste0(out,floor(s/60),'m')
        s=s%%60
      }
      out=paste0(out,s,'s')
      return(out)
    }
    print("check status")
    logpath=paste(basepath,'/log/',type,'.txt',sep="")
    if(file.exists(logpath))
    {
      tasks=unlist(strsplit(x = condition[type,'task'],split = ";"))
      if(type=="PCC")
      {
        content=readLines(logpath)
        lastline=content[length(content)]
        progress=min(length(content),3)/3*100
        
        if(grepl(pattern = "^All Finish.$",x = lastline))
        {
          session$sendCustomMessage('calculation_eta',
                                    list(type=type,msg=lastline,status='stop',progress=paste(progress,"%",sep=""),complete=paste(length(tasks),"/",length(tasks),sep="")))
        }
        else
        {
          session$sendCustomMessage('calculation_eta',list(type=type,msg=lastline,status='run',progress=paste(progress,"%",sep=""),complete=paste(0,"/",length(tasks),sep="")))
        }
      }
      else
      {
        content=readLines(logpath)
        indexes=which(grepl(pattern = "^\\[\\{\"task",x = content))
        if(length(indexes)>0)
        {
          endtime=as.numeric(Sys.time())
          index=max(indexes)
          info=fromJSON(content[index])
          complete=nchar(content[index+1])#完成???
          if(is.na(complete))
            complete=1
          eta=(endtime-info$time)/complete*(info$total-complete)#预计时间
          finish.task=length(which(grepl(pattern = "^Finish",x = content)))#总完成任务数
          status="run"
          msg=paste("Running:",info$task,"&nbsp;&nbsp;&nbsp;&nbsp;ETA:",time(eta))
          progress=format(x = complete/info$total*100,nsmall=2)
          if(length(which(grepl(pattern = "^All Finish.$",x = content)))>0)
          {
            msg="All Finish."
            status='stop'
          }
          session$sendCustomMessage('calculation_eta',
                                    list(type=type,msg=msg,progress=paste(progress,"%",sep=""),status=status,complete=paste(finish.task,"/",length(tasks),sep="")))
        }
      }
    }
    else
    {
      session$sendCustomMessage('calculation_eta',list(type=type,msg="",status='run'))
    }
  })
  observeEvent(input$condition_filter_response,{
    isolate({
      type=input$condition_filter_response$type
      tasks=input$condition_filter_response$tasks
    })
    removeUI(selector = paste("div.col-lg-12 > #density_plot_",type,sep=""),immediate = T)
    insertUI(selector = "#condition_preview",where = 'beforeEnd',
             ui =filter_box(type,tasks),
             immediate = T
    )
  })
  observeEvent(input$condition_finish,{
    isolate({
      type=input$condition_finish$type
    })
    tasks=condition[type,'task']
    tasks=unlist(strsplit(x = tasks,split = ";"))
    if(type=="PCC")
    {
      result=readMat(paste(basepath,'/all.cor.mat',sep=""))
      cor=result$cor
      pvalue=result$pvalue
      print(dim(after_slice_geneinfo))
      gene=rownames(after_slice_rna.exp)
      print(dim(cor))
      rownames(cor)=gene
      colnames(cor)=gene
      rownames(pvalue)=gene
      colnames(pvalue)=gene
      if(length(which(tasks=='all'))==1)
      {
        cor=list(cor)
        names(cor)='all'
        corlist=list(cor)
        names(corlist)='PCC'
        
        pvalue=list(pvalue)
        names(pvalue)="all"
        pvaluelist=list(pvalue)
        names(pvaluelist)='PCC.pvalue'
      }
      else
      {
        corlist=list()
        pvaluelist=list()
        for(task in tasks)
        {
          groups=unlist(strsplit(x = task,split = "---"))
          group1=rownames(after_slice_geneinfo)[which(after_slice_geneinfo$.group==groups[1])]
          group2=rownames(after_slice_geneinfo)[which(after_slice_geneinfo$.group==groups[2])]
          
          tmp=list(cor[group1,group2])
          names(tmp)=task
          corlist=c(corlist,tmp)
          tmp=list(pvalue[group1,group2])
          names(tmp)=task
          pvaluelist=c(pvaluelist,tmp)
        }
        corlist=list(corlist)
        names(corlist)="PCC"
        
        pvaluelist=list(pvaluelist)
        names(pvaluelist)="PCC.pvalue"
        #condition.values<<-c(condition.values,corlist,pvaluelist)
      }
      if(is.null(condition.values[['PCC']]))
      {
        condition.values<<-c(condition.values,corlist)
      }
      else
      {
        condition.values['PCC']<<-corlist
      }
      if(is.null(condition.values[['PCC.pvalue']]))
      {
        condition.values<<-c(condition.values,pvaluelist)
      }
      else
      {
        condition.values['PCC.pvalue']<<-pvaluelist
      }
    }
    else
    {
      result=readRDS(paste(basepath,'/',type,'.RData',sep=""))
      result=list(result)
      names(result)=type
      if(is.null(condition.values[[type]]))
      {
        condition.values<<-c(condition.values,result)
      }
      else
      {
        condition.values[type]<<-result
      }    
    }
    draw_density(basepath,output,session,type,tasks)
  })
  observeEvent(input$update_condition_thresh,{
    isolate({
      msg=input$update_condition_thresh
      direction=input[[paste("direction",msg$type,msg$task,sep="_")]]
    })
    condition_density_plot(basepath = basepath,type = msg$type,task = msg$task,value = msg$value,direction = direction)
    output[[paste("density_plot",msg$type,msg$task,"image",sep="_")]]=renderImage({
      figurepath=paste(basepath,'/Plot/density_plot_',msg$type,"_",msg$task,".svg",sep="")
      list(src=figurepath,width="100%",height="100%")
    },deleteFile = F)
  })
  observeEvent(input$construct_network,{
    isolate({
      msg=input$construct_network
      newthresh=msg$thresh
      type=msg$type
    })
    thresh<<-thresh[thresh$type!=type,]
    for(name in names(newthresh))
    {
      thresh<<-rbind(thresh,data.frame(type=type,task=name,direction=newthresh[[name]][['direction']],thresh=as.numeric(newthresh[[name]][['thresh']]),stringsAsFactors = F))
    }
    network_construnction(after_slice_geneinfo)
  })

  ##########Visualization Page Action#########
  observeEvent(input$network,{
    isolate({
      msg=input$network
      do_what =msg$do_what
    })
    if(do_what=="layout"){
      type=msg$type
      visual_layout<<-type
      edge=as.data.frame(which(network==1,arr.ind = T))
      edge[,1]=rownames(network)[edge[,1]]
      edge[,2]=colnames(network)[edge[,2]]
      nodes=unique(c(edge[,1],edge[,2]))
      colnames(edge)=c('source','target')
      num=which(colnames(after_slice_geneinfo)==".group")
      new_after_geneinfo = after_slice_geneinfo
      colnames(new_after_geneinfo)[num]="group"
      nodes=data.frame(id=nodes,new_after_geneinfo[nodes,],stringsAsFactors = F)
      node=tibble(group="nodes",data=apply(X = nodes,MARGIN = 1,as.list))
      edge=tibble(group="edges",data=apply(X = edge,MARGIN = 1,FUN = as.list))
      session$sendCustomMessage('network',toJSON(list(nodes=node,edge=edge,type=type,do_what=do_what),auto_unbox = T))
    }
  })
  observeEvent(input$change_network_name,{
    
    session$sendCustomMessage('Gene_info_name_change',colnames(after_slice_geneinfo))
  
  })
  observeEvent(input$net_color_shape,{
    isolate({
      msg=input$net_color_shape
      type =msg$type
      func = msg$func;
    })
    if(func=="color"){
      if(type=="group"){
        vec = data.frame(type=after_slice_geneinfo[".group"])
        vec = vec[[1]]
        index = duplicated(vec)
        vec = vec[!index]
        session$sendCustomMessage('Gene_network_color_change',data.frame(type=vec,stringsAsFactors = F))
      }
      else{
        vec = data.frame(type=after_slice_geneinfo[type])
        vec = vec[[1]]
        index = duplicated(vec)
        vec = vec[!index]
        session$sendCustomMessage('Gene_network_color_change',data.frame(type=vec,stringsAsFactors = F))
      }
    }
    if(func=="shape"){
      if(type=="group"){
        vec = data.frame(type=after_slice_geneinfo[".group"])
        vec = vec[[1]]
        index = duplicated(vec)
        vec = vec[!index]
        session$sendCustomMessage('Gene_network_shape_change',data.frame(type=vec,stringsAsFactors = F))
      }
      else{
        vec = data.frame(type=after_slice_geneinfo[type])
        vec = vec[[1]]
        index = duplicated(vec)
        vec = vec[!index]
        session$sendCustomMessage('Gene_network_shape_change',data.frame(type=vec,stringsAsFactors = F))
      }
    }
  })
  
  ##########Analysis Page Action###############
  observeEvent(input$nodeCentrality,{
    isolate({
      msg=input$nodeCentrality
      centrality=unlist(msg$value)
    })
    print(centrality)
    deleted=setdiff(node_property,centrality)
    deleted=sub(pattern = " ",replacement = "_",deleted)
    removeUI(selector = paste("#node_",deleted,sep=""),multiple = T,immediate = T)
    newadd=setdiff(centrality,node_property)
    node_property<<-centrality
    if(nodeNewInfo=="")
    {
      nodeNewInfo<<-data.frame(id=rownames(after_slice_geneinfo),stringsAsFactors = F,row.names = rownames(after_slice_geneinfo))
    }
      
    for(id in newadd)
    {
        ui=create_property_box('node',id)
        insertUI(selector = "#network_property",where = 'beforeEnd',ui = ui,
                 immediate = T)
        if(id=="Degree")
        {
          degree=as.data.frame(degree(net_igraph))
          nodeNewInfo[rownames(degree),'Degree']<<-degree[,1]
          data=as.data.frame(table(degree[,1]),stringsAsFactors = F)
          data$Var1=as.numeric(data$Var1)
          data=data[which(data$Var1!=0),]
          p=ggplot(data = data,aes(x = Var1,ymax=Freq,ymin=0,y=Freq))+
          geom_linerange(linetype='dashed')+
          geom_point(size=3)+scale_x_log10()+scale_y_log10()+geom_smooth(method = lm)
          svg(filename = paste(basepath,"Plot",'node_degree.svg',sep="/"),family = 'serif')
          print(p)
          dev.off()
          output$node_Degree_plot=renderImage({
            list(src=normalizePath(paste(basepath,"Plot",'node_degree.svg',sep="/")),height="100%",width="100%")
          },deleteFile=F)
        }
        else if(id=="Betweenness")
        {
          betweenness=betweenness(net_igraph,directed = F)
          nodeNewInfo[names(betweenness),'Betweenness']<<-betweenness
          density=density(x = betweenness,from = min(betweenness,na.rm = T),to = max(betweenness,na.rm = T),na.rm = T)
          density=data.frame(x=density$x,y=density$y)
          p=ggplot(data = density)+geom_line(mapping = aes(x = x,y = y),size=1.5)
          svg(filename = paste(basepath,"Plot",'node_betweenness.svg',sep="/"),family = 'serif')
          print(p)
          dev.off()
          output$node_Betweenness_plot=renderImage({
            list(src=normalizePath(paste(basepath,"Plot",'node_betweenness.svg',sep="/")),height="100%",width="100%")
          },deleteFile=F)
        }
        else if(id=="Closeness")
        {
          closeness=closeness(net_igraph,mode = 'all')
          nodeNewInfo[names(closeness),'Closeness']<<-closeness
          density=density(x = closeness,from = min(closeness,na.rm = T),to = max(closeness,na.rm = T),na.rm = T)
          density=data.frame(x=density$x,y=density$y)
          p=ggplot(data = density)+geom_line(mapping = aes(x = x,y = y),size=1.5)
          svg(filename = paste(basepath,"Plot",'node_closeness.svg',sep="/"),family = 'serif')
          print(p)
          dev.off()
          output$node_Closeness_plot=renderImage({
            list(src=normalizePath(paste(basepath,"Plot",'node_closeness.svg',sep="/")),height="100%",width="100%")
          },deleteFile=F)
        }
        else if(id=="Clustering Coefficient")
        {
          cc=transitivity(net_igraph,type='local',isolates = 'zero')
          nodeNewInfo[V(net_igraph)$name,'Clustering Coefficient']<<-cc
          density=density(x = cc,from = min(cc,na.rm = T),to = max(cc,na.rm = T),na.rm = T)
          density=data.frame(x=density$x,y=density$y)
          p=ggplot(data = density)+geom_line(mapping = aes(x = x,y = y),size=1.5)
          svg(filename = paste(basepath,"Plot",'node_clustering_coefficient.svg',sep="/"),family = 'serif')
          print(p)
          dev.off()
          output$node_Clustering_Coefficient_plot=renderImage({
            list(src=normalizePath(paste(basepath,"Plot",'node_clustering_coefficient.svg',sep="/")),height="100%",width="100%")
          },deleteFile=F)
        }
    }
  })
  observeEvent(input$edgeCentrality,{
    isolate({
      msg=input$edgeCentrality
      centrality=msg$value
    })
    print(centrality)
    deleted=setdiff(edge_property,centrality)
    removeUI(selector = paste("#edge_",deleted,sep=""),multiple = T,immediate = T)
    newadd=setdiff(centrality,edge_property)
    edge_property<<-centrality
    for(id in newadd)
    {
      ui=create_property_box('edge',id)
      insertUI(selector = "#network_property",where = 'beforeEnd',ui = ui,
               immediate = T)
      if(id=="Betweenness")
      {
        betweenness=edge_betweenness(net_igraph,directed = F)
        
        edgelist=t(apply(X = as_edgelist(net_igraph),MARGIN = 1,FUN = sort))
        edgename=t(apply(X = edgeinfo[,c('N1','N2')],MARGIN = 1,FUN = sort))
        rownames(edgeinfo)<<-paste(edgename[,1],edgename[,2],sep="|")
        edgeinfo[paste(edgename[,1],edgename[,2],sep="|"),'Betweenness']<<-betweenness
        rownames(edgeinfo)<<-NULL
        
        density=density(x = betweenness,from = min(betweenness,na.rm = T),to = max(betweenness,na.rm = T),na.rm = T)
        density=data.frame(x=density$x,y=density$y)
        p=ggplot(data = density)+geom_line(mapping = aes(x = x,y = y),size=1.5)
        svg(filename = paste(basepath,"Plot",'edge_betweenness.svg',sep="/"),family = 'serif')
        print(p)
        dev.off()
        output$edge_Betweenness_plot=renderImage({
          list(src=normalizePath(paste(basepath,"Plot",'edge_betweenness.svg',sep="/")),height="100%",width="100%")
        },deleteFile=F)
      }
    }
  })
  observeEvent(input$nodeDetails,{
    removeUI(selector = "#modalbody>",immediate = T)
    insertUI(selector = "#modalbody",where = 'beforeEnd',ui = rHandsontableOutput(outputId = "nodeDetailsTable"),immediate = T)
    output$nodeDetailsTable=renderRHandsontable({
      showtable=data.frame(after_slice_geneinfo,nodeNewInfo[,-1],stringsAsFactors = F,check.rows = T,check.names = T)
      doubleColumn=which(unlist(lapply(X = showtable,FUN = typeof))=='double')
      rhandsontable(showtable, width = "100%", height = "500",rowHeaders = NULL,search = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = T,manualColumnMove = T,manualColumnResize = F) %>%
        hot_col(col = seq(1:dim(showtable)[2]),halign='htCenter',readOnly = T,copyable = T)%>%
        hot_col(col = doubleColumn,format = '0.000e-0')
      
    })
  })
  observeEvent(input$edgeDetails,{
    removeUI(selector = "#modalbody>",immediate = T)
    insertUI(selector = "#modalbody",where = 'beforeEnd',ui = rHandsontableOutput(outputId = "edgeDetailsTable"),immediate = T)
    output$edgeDetailsTable=renderRHandsontable({
      doubleColumn=which(unlist(lapply(X = edgeinfo,FUN = typeof))=='double')
      rhandsontable(edgeinfo, width = "100%", height = "500",rowHeaders = NULL,readOnly = F) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = T,manualColumnMove = T,manualColumnResize = T) %>%
        hot_col(col = doubleColumn,format='0.000e+0')%>%
        hot_col(col = seq(1:dim(edgeinfo)[2]),halign='htCenter',readOnly = T,copyable = T)
    })
  })
  observeEvent(input$community_detection,{
    isolate({
      algorithm=input$community_algorithm
    })
    removeUI(selector = "#module_info_box>",multiple = T,immediate = T)
    removeUI(selector = "#module_visualization>",multiple = T,immediate = T)
    insertUI(selector = "#module_info_box",where = 'beforeEnd',ui = create_progress(paste0("Running ",algorithm,"...")),immediate = T)
    moduleinfo<<-""
    module.configure<<-list()
    gc()
    
    if(algorithm=='cluster_edge_betweenness')
    {
      community=get(algorithm)(net_igraph)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_fast_greedy')
    {
      community=get(algorithm)(net_igraph)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
      
    }
    else if(algorithm=='cluster_label_prop')
    {
      community=get(algorithm)(net_igraph)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_leading_eigen')
    {
      
    }
    else if(algorithm=='cluster_louvain')
    {
      community=get(algorithm)(net_igraph)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_optimal')
    {
      community=get(algorithm)(net_igraph)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_walktrap')
    {
      isolate({
        step=floor(input$walktrap_step)
      })
      community=get(algorithm)(net_igraph,step=step)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_infomap')
    {
      isolate({
        nb.trials=floor(input$infomap_nb_trails)
      })
      community=get(algorithm)(net_igraph,nb.trials=nb.trials)
      communitySize=sizes(community)
      singleNodeCommunity=as.numeric(names(communitySize[which(communitySize==1)]))
      membership=membership(community)
      membership[which(membership%in%singleNodeCommunity)]=0
      after_slice_geneinfo[names(membership),'module']<<-paste("Module",membership,sep="")
      
      community_list=list()
      for(id in unique(membership))
      {
        module_gene=names(membership)[which(membership==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(membership),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_cograph')
    {
      netpath=paste(basepath,"/data/net_edge.txt",sep="")
      write.table(x = edgeinfo[,c("N1","N2")],file = netpath,quote = F,sep = "\t",row.names = F,col.names = F)
      outpath=paste(basepath,"/data/",sep="")
      cluster_cograph(netpath = netpath,outpath = outpath)
    }
    else if(algorithm=='cluster_mcl')
    {
      isolate({
        expansion=input$mcl_expansion
        inflation=input$mcl_inflation
        max.iter=floor(input$mcl_max_iter)
      })
      community=get(algorithm)(net_igraph,expansion=expansion,inflation=inflation,max.iter=max.iter)
      after_slice_geneinfo[names(community),'module']<<-community
      community_list=list()
      for(id in unique(community))
      {
        module_gene=names(community)[which(community==id)]
        community_list=c(community_list,list(module_gene))
      }
      names(community_list)=paste("Module",unique(community),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_linkcomm')
    {
      isolate({
        hcmethod=input$linkcomm_hcmethod
      })
      community=get(algorithm)(edgeinfo,hcmethod=hcmethod)
      after_slice_geneinfo[,'module']<<-''
      community_list=list()
      for(id in unique(community$cluster))
      {
        module_gene=community$node[which(community$cluster==id)]
        community_list=c(community_list,list(module_gene))
        after_slice_geneinfo[module_gene,'module']<<-paste(after_slice_geneinfo[module_gene,'module'],',Module',id,sep="")
      }
      after_slice_geneinfo$module[which(after_slice_geneinfo$module=="")]<<-"Module0"
      after_slice_geneinfo$module<<-sub(pattern = "^,",replacement = "",x = after_slice_geneinfo$module)
      names(community_list)=paste("Module",unique(community$cluster),sep="")
      modules<<-community_list
    }
    else if(algorithm=='cluster_mcode')
    {
     isolate({
       vwp=input$mcode_vwp
       haircut=input$mcode_haircut
       fluff=input$mcode_fluff
       fdt=input$mcode_fdt
     })
     community=get(algorithm)(net_igraph,vwp=vwp,haircut=haircut,fluff=fluff,fdt=fdt)
     
     after_slice_geneinfo[,'module']<<-''
     community_list=list()
     for(id in unique(community$cluster))
     {
       module_gene=community$node[which(community$cluster==id)]
       community_list=c(community_list,list(module_gene))
       after_slice_geneinfo[module_gene,'module']<<-paste(after_slice_geneinfo[module_gene,'module'],',Module',id,sep="")
     }
     after_slice_geneinfo$module[which(after_slice_geneinfo$module=="")]<<-"Module0"
     after_slice_geneinfo$module<<-sub(pattern = "^,",replacement = "",x = after_slice_geneinfo$module)
     names(community_list)=paste("Module",unique(community$cluster),sep="")
     modules<<-community_list
    }
    #Show Communities
    create_module_info()
    removeUI(selector = "#module_info_box>",immediate = T,multiple = T)
    insertUI(selector = "#module_info_box",where = 'beforeEnd',ui = create_alert_box(header="Tips",msg="The <i>Module0</i> is consisted of all isolated nodes",class="col-lg-4"),immediate = T)
    insertUI(selector = "#module_info_box",where = 'beforeEnd',ui = rHandsontableOutput(outputId = "moduleInfoTable"),immediate = T)
    output$moduleInfoTable=renderRHandsontable({
      rhandsontable(moduleinfo)%>%
        hot_cols(columnSorting = T)%>%
        hot_table(contextMenu = F)%>%
        hot_col(col = seq(1:dim(moduleinfo)[2]),halign='htCenter',readOnly = T,copyable=T)%>%
        hot_col(col = "Nodes",halign = 'htCenter',renderer=htmlwidgets::JS("safeHtmlRenderer"))%>%
        hot_col(col = "Edges",halign = 'htCenter',renderer=htmlwidgets::JS("safeHtmlRenderer"))%>%
        hot_col(col = "Visualization",halign = 'htCenter',renderer=htmlwidgets::JS("safeHtmlRenderer"))
    })
  })
  observeEvent(input$communityDetals,{
    isolate({
      msg=input$communityDetals
      id=msg$moduleid
    })
    modulegene=modules[[id]]
    removeUI(selector = "#modalbody>",multiple = T,immediate = T)
    insertUI(selector = "#modalbody",where = "beforeEnd",ui = rHandsontableOutput(outputId = "nodesDetailsTable"),immediate = T)
    output$nodesDetailsTable=renderRHandsontable({
      showtable=data.frame(after_slice_geneinfo,nodeNewInfo[,-1],stringsAsFactors = F,check.rows = T,check.names = T)
      doubleColumn=which(unlist(lapply(X = showtable,FUN = typeof))=='double')
      rhandsontable(showtable[modulegene,], width = "100%", height = "500",rowHeaders = NULL,search = T) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = T,manualColumnMove = T,manualColumnResize = F) %>%
        hot_col(col = seq(1:dim(showtable)[2]),halign='htCenter',readOnly = T,copyable = T)%>%
        hot_col(col = doubleColumn,format = '0.000e-0')
    })
  })
  observeEvent(input$communityEdgeDetals,{
    isolate({
      msg=input$communityEdgeDetals
      id=msg$moduleid
    })
    modulegene=modules[[id]]
    index=which(edgeinfo$N1%in%modulegene&edgeinfo$N2%in%modulegene)
    edges=edgeinfo[index,]
    removeUI(selector = "#modalbody>",multiple = T,immediate = T)
    insertUI(selector = "#modalbody",where = "beforeEnd",ui = rHandsontableOutput(outputId = "nodesDetailsTable"),immediate = T)
    output$nodesDetailsTable=renderRHandsontable({
      doubleColumn=which(unlist(lapply(X = edges,FUN = typeof))=='double')
      rhandsontable(edges, width = "100%", height = "500",rowHeaders = NULL,readOnly = F) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = T,manualColumnMove = T,manualColumnResize = T) %>%
        hot_col(col = doubleColumn,format='0.000e+0')%>%
        hot_col(col = seq(1:dim(edges)[2]),halign='htCenter',readOnly = T,copyable = T)
    })
  })
  observeEvent(input$displayCommunity,{
    isolate({
      msg=input$displayCommunity
      id=msg$moduleid
    })
    
    if(is.null(module.configure[[id]]))
    {
      tmp.configure=list(default.configure)
      names(tmp.configure)=id
      module.configure<<-c(module.configure,tmp.configure)
    }
    
    removeUI(selector = paste("#module_",id,sep=""),multiple = T,immediate = T)
    ui=create_module_visualization(id)
    insertUI(selector = "#module_visualization",where = 'beforeEnd',ui = ui,immediate = T)
    output[[paste(id,"_plot",sep="")]]=renderVisNetwork({
      module.gene=modules[[id]]
      node=data.frame(id=module.gene,label=module.gene,
                      group=after_slice_geneinfo[module.gene,'.group'],color='red')
      edgeindex=which(edgeinfo$N1%in%module.gene&edgeinfo$N2%in%module.gene)
      edge=edgeinfo[edgeindex,c("N1","N2")]
      colnames(edge)=c("from",'to')
      visNetwork(nodes = node,edges = edge,width = "100%",height = "100%")%>%
        visPhysics(stabilization = FALSE)%>%
        visEdges(smooth = FALSE)%>% 
        visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout()%>% 
        visOptions(highlightNearest = TRUE)
    })
  })
  observeEvent(input$module_setting,{
    isolate({
      msg=input$module_setting
      id=msg$id
    })
    removeUI(selector = "#modalbody>",multiple = T,immediate = T)
    ui=create_modal_setting(id)
    insertUI(selector = "#modalbody",where = "beforeEnd",ui = ui,multiple = F,immediate = T)
  })
  
  observeEvent(input$Update_community_style,{
    isolate({
      msg=input$Update_community_style
      id=msg$id
      layout=input$module_layout
      label=input$module_label
      color_map=input$module_color
      shape_map=input$module_shape
    })
    
    module.gene=modules[[id]]
    node=data.frame(id=module.gene,
                    label=after_slice_geneinfo[module.gene,label],
                    color='',
                    shpe='',stringsAsFactors = F
    )
    rownames(node)=node$id
    edgeindex=which(edgeinfo$N1%in%module.gene&edgeinfo$N2%in%module.gene)
    edge=edgeinfo[edgeindex,c("N1","N2")]
    colnames(edge)=c("from",'to')
    if(color_map=="All")
    {
      isolate({
        color=input[["All_color"]]
      })
      node$color=color
      module.configure[[id]]$color<<-color
    }
    else
    {
      module.configure[[id]]$color<<-list()
      items=as.character(unique(after_slice_geneinfo[module.gene,color_map]))
      for(item in items)
      {
        isolate({
          color=input[[paste0(item,"_color")]]
        })
        node[module.gene[which(after_slice_geneinfo[module.gene,color_map]==item)],'color']=color
        
        module.configure[[id]]$color<<-c(module.configure[[id]]$color,list(color))
      }
      names(module.configure[[id]]$color)<<-items
    }
    if(shape_map=="All")
    {
      isolate({
        shape=input[["All_shape"]]
      })
      node$shape=shape
      module.configure[[id]]$shape<<-shape
    }
    else
    {
      module.configure[[id]]$shape<<-list()
      items=as.character(unique(after_slice_geneinfo[module.gene,shape_map]))
      for(item in items)
      {
        isolate({
          shape=input[[paste0(item,"_shape")]]
        })
        node[module.gene[which(after_slice_geneinfo[module.gene,shape_map]==item)],'shape']=shape
        
        module.configure[[id]]$shape<<-c(module.configure[[id]]$shape,list(shape))
      }
      names(module.configure[[id]]$shape)<<-items
    }
    output[[paste(id,"_plot",sep="")]]=renderVisNetwork({
      visNetwork(nodes = node,edges = edge,width = "100%",height = "100%")%>%
        visPhysics(stabilization = FALSE)%>%
        visEdges(smooth = FALSE)%>% 
        visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout(layout = layout)%>%
        visOptions(highlightNearest = TRUE) 
    })
    
    module.configure[[id]]$layout<<-layout
    module.configure[[id]]$label<<-label
    module.configure[[id]]$color.attr<<-color_map
    module.configure[[id]]$shape.attr<<-shape_map
  })
  
  observeEvent(list(input$clinical_file,input$clinical_seperator,input$clinical_header),{
    isolate({
      file=input$clinical_file
      seperator=input$clinical_seperator
      header=as.logical(input$clinical_header)
    })
    if(!is.null(file))
    {
      removeUI(selector = "#clinical_data_preview>",multiple = T,immediate = T)
      insertUI(selector = "#clinical_data_preview",where = 'beforeEnd',ui = div(class="overlay",id="icon",tags$i(class="fa fa-spinner fa-spin",style="font-size:50px")))
      insertUI(selector = "#clinical_data_preview",where = 'beforeEnd',ui = rHandsontableOutput(outputId = "clinical_data_table"))
      clinical_data<<-read.table(file = file$datapath,header = header,sep = seperator,stringsAsFactors = F)
      output[['clinical_data_table']]=renderRHandsontable({
        rhandsontable(head(clinical_data))
      })
      removeUI(selector = "#icon")
    }
  })
})


