####################################
#### Google Analytics - server.R ###
####################################

print(options('shiny.maxRequestSize'))
shinyServer(function(input,output,session) {
  source('www/R/input_tabServer.R')
  dir.create(path = paste('session_tmp_file',session$token))
  print(paste("Session:",session$token,'is started!'))
  basepath = paste('session_tmp_file',session$token);
  dir.create(paste(basepath,'Plot',sep="/"))
  sect_output_rna.exp=""
  sect_output_micro.exp=""
  sect_output_target=""
  sect_output_geneinfo=""
  after_slice_micro.exp=""
  after_slice_rna.exp=""
  expressgene_num=""
  expressgene_num2=""
  load('testdata/ph1.RData')
  #Input Page Action
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
  #Process Page Action
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
      obj=list(title='Valid ceRNA',details=toJSON(data.frame(detail= rownames(sect_output_rna.exp),stringsAsFactors =F)));
      session$sendCustomMessage('outdetails',obj);
    }
    if(msg$id=='MicroRnaoutput'){
      obj=list(title='Valid microRNA',details=toJSON(data.frame(detail= rownames(sect_output_micro.exp),stringsAsFactors =F)));
      session$sendCustomMessage('outdetails',obj);
    }
    if(msg$id=='Sampleoutput'){
      obj=list(title='Valid Sample',details=toJSON(data.frame(detail= names(sect_output_rna.exp),stringsAsFactors =F)));
      session$sendCustomMessage('outdetails',obj);
    }
  })
  observeEvent(input$Update_Biotype_Map,{
    choice=colnames(geneinfo)
    updatePrettyRadioButtons(session = session,inputId = 'biotype_map',label = 'Which Column is Gene Biotype?',choices = choice,selected = NULL,inline=T,prettyOptions=list(shape='round',status='success'))
  })
  observe({
    biotype=input$biotype_map
    if(biotype!='None')
    {
      choice=unique(geneinfo[,biotype])
      if(length(choice)>200)
      {
        sendSweetAlert(session = session,title = 'Warning...',text = 'Too Many Biotypes, Choose Carefully!',type = 'warning')
        return()
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
    sect_output_geneinfo$.group<<-NULL
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
      dev.off()
      print(normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")))
      list(src=normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")))    
    })
    
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
    #这需要判断传进来的是字符串还是数字。。和字符串的比较好像不能直接！= 然后还要统一大小写，全转换为大写toupper(states)
    browser()
    if(len_sep==1){
      myfunc<-function(x){
        if(is.character(x)){
          x<-toupper(x)
        }
        # x<-as.character(x)
        sum(x!=sep[[1]])
      }
      # expressgene_num<<-apply(sect_output_micro.exp, 2, myfunc)
      expressgene_num<<-colSums(sect_output_micro.exp!=0) 
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
    # expressgene_num<<-expressgene_num/(dim(sect_output_micro.exp)[1])
    # browser()
    process_sample<-data.frame(
      x=expressgene_num
    )
    
    value=as.numeric(value)
    draw_x<-(max(expressgene_num)+min(expressgene_num))/2  
    #通过quantile函数找  
    x2<-quantile(expressgene_num,value,type=3) 
    svg(filename = paste(basepath,"Plot","microSampleFilter.svg",sep = "/"),family = 'serif')
    print(ggplot(process_sample, aes(x = x))+stat_ecdf()+
      geom_hline(aes(yintercept=value), colour="#990000", linetype="dashed")+
      geom_vline(aes(xintercept=x2), colour="#990000", linetype="dashed")+
      geom_point(x=x2,y=value)+geom_text(label=paste0("(",x2,",",value,")"),x=draw_x ,y=0,colour = "red",family="serif",size=5))
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
      #这需要判断传进来的是字符串还是数字。。和字符串的比较好像不能直接！= 然后还要统一大小写，全转换为大写toupper(states)
      if(len_sep==1){
        myfunc<-function(x){
          if(is.character(x)){
            x<-toupper(x)
          }
          # x<-as.character(x)
          sum(x!=sep[[1]])
        }
        # expressgene_num2<<-apply(sect_output_rna.exp, 2, myfunc)
        expressgene_num2<<-colSums(sect_output_rna.exp!=0) 
       
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
      expressgene_num2<<-expressgene_num2/(dim(sect_output_rna.exp)[1])
      process_sample<-data.frame(
        x=c(expressgene_num2)
      )
      
      value=as.numeric(value)
      draw_x<-(max(expressgene_num2)+min(expressgene_num2))/2  
      #通过quantile函数找  
      x2<-quantile(expressgene_num2,value,type=3) 
      svg(filename = paste(basepath,"Plot","RNASampleFilter.svg",sep = "/"),family = 'serif')
      print(ggplot(process_sample, aes(x = x))+stat_ecdf()+
              geom_hline(aes(yintercept=value), colour="#990000", linetype="dashed")+
              geom_vline(aes(xintercept=x2), colour="#990000", linetype="dashed")+
              geom_point(x=x2,y=value)+geom_text(label=paste0("(",x2,",",value,")"),x=draw_x ,y=0,colour = "red",family="serif",size=5))
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
      #这里需要讨论确定删选比例的意思.1:样本基因表达数除以总基因数2：学长说的按照分布函数？
      x2<-quantile(expressgene_num,line,type=3) 
      which(expressgene_num>=x2)
      after_slice_micro.exp<-sect_output_micro.exp[,which(expressgene_num>=x2)]
      browser()
    }
    else{
      x2<-quantile(expressgene_num2,line,type=3) 
      which(expressgene_num2>=x2)
      after_slice_rna.exp<-sect_output_rna.exp[,which(expressgene_num2>=x2)]
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
    
    #paint picture
    if(type=="micro"){  
      validGene=rownames(sect_output_micro.exp)
      validSample = rowSums(sect_output_micro.exp>=number)
      ratio=as.numeric(line)
      xdata = data.frame(SampleRatio=validSample/length(colnames(sect_output_micro.exp)),stringsAsFactors = F)
      ypoint=length(which(xdata$SampleRatio<=ratio))/length(validGene)
      temp.data=data.frame(x=c(0,ratio),xend=c(ratio,ratio),y=c(ypoint,0),yend=c(ypoint,ypoint),stringsAsFactors = F)
      
      svg(filename = paste(basepath,"Plot","microStatistic.svg",sep = "/"),family = 'serif')
      print(ggplot(xdata,aes(x=SampleRatio,))+stat_ecdf()+geom_segment(aes(x=x,xend=xend,y=y,yend=yend),data=temp.data))
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
               tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",onclick=paste("slice('#","gene_Group_",group,"_panel')",sep=""),style="margin:5px",height = "100%",HTML("Filter"))),
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
      validSample = rowSums(sect_output_rna.exp[validGene,]>=number)
      ratio=as.numeric(line)
      xdata = data.frame(SampleRatio=validSample/length(colnames(sect_output_rna.exp)),stringsAsFactors = F)
      ypoint=length(which(xdata$SampleRatio<=ratio))/length(validGene)
      temp.data=data.frame(x=c(0,ratio),xend=c(ratio,ratio),y=c(ypoint,0),yend=c(ypoint,ypoint),stringsAsFactors = F)
      
      svg(filename = paste(basepath,"/Plot/",group,"Statistic.svg",sep = ""),family = 'serif')
      print(ggplot(xdata,aes(x=SampleRatio,))+stat_ecdf()+geom_segment(aes(x=x,xend=xend,y=y,yend=yend),data=temp.data))
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
                 tags$button(class = "btn btn-success action-button pull-right shiny-bound-input",onclick=paste("slice('#","gene_Group_",group,"_panel')",sep=""),style="margin:5px",height = "100%",HTML("Filter"))),
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
      group=sub(pattern = "gene_slice_value_",replacement = "",x = group)
      number=as.numeric(msg$number)
      type = msg$type
      line = msg$line
      line = as.numeric(line)
      first = msg$first
    })

    if(type=="micro"){
      validGene=rownames(sect_output_micro.exp)
      validSample = rowSums(sect_output_micro.exp>=number)
      xdata = data.frame(SampleRatio=validSample/length(colnames(sect_output_micro.exp)),stringsAsFactors = F)
      intersect_name = rownames(xdata)[which(xdata$SampleRatio>=line)]
      after_slice_micro.exp<<- sect_output_micro.exp[intersect_name,]
      
    }
    else{
      if(first=="T"){
        validGene=rownames(sect_output_geneinfo[which(sect_output_geneinfo$.group==group),])
        validSample = rowSums(sect_output_rna.exp[validGene,]>=number)
        xdata = data.frame(SampleRatio=validSample/length(colnames(sect_output_rna.exp)),stringsAsFactors = F)
        intersect_name = rownames(xdata)[which(xdata$SampleRatio>=line)]
        after_slice_rna.exp<<- sect_output_rna.exp[intersect_name,]
      }
      #qingkong xiangying rna
      after_slice_rna.exp<<-after_slice_rna.exp[!(after_slice_rna.exp$.group==group),]
      #append group gene to after_slice_rna.exp
      validGene=rownames(sect_output_geneinfo[which(sect_output_geneinfo$.group==group),])
      validSample = rowSums(sect_output_rna.exp[validGene,]>=number)
      xdata = data.frame(SampleRatio=validSample/length(colnames(sect_output_rna.exp)),stringsAsFactors = F)
      intersect_name = rownames(xdata)[which(xdata$SampleRatio>=line)]
      rbind(after_slice_rna.exp,sect_output_rna.exp[intersect_name,])
    }
  })
})
