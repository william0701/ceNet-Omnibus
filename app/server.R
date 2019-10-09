####################################
#### Google Analytics - server.R ###
####################################

print(options('shiny.maxRequestSize'))
shinyServer(function(input,output,session) {
  source('www/R/input_tabServer.R')
  source('www/R/construct_tabServer.R')
  ##创建临时文件夹
  tmpdir=tempdir()
  basepath = paste(tmpdir,'/session_tmp_file',session$token,sep="");
  dir.create(path = basepath)
  print(paste("Session:",session$token,'is started!'))
  dir.create(paste(basepath,'Plot',sep="/"))
  dir.create(paste(basepath,'code',sep="/"))
  dir.create(paste(basepath,'data',sep="/"))
  dir.create(paste(basepath,'log',sep="/"))
  ##
  sect_output_rna.exp=""
  sect_output_micro.exp=""
  sect_output_target=""
  sect_output_geneinfo=""
  biotype_map=""
  
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
      print(p)
      dev.off()
      print(normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")))
      list(src=normalizePath(paste(basepath,"Plot",'ph1.svg',sep="/")),height="100%",width="100%")    
    },deleteFile=F)
    session$sendCustomMessage('clear_construction_task',"")
  })
  #Construction Page Action
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
    
    if(is.null(sect_output_geneinfo$.group))
    {
      sect_output_geneinfo$.group<<-'Default'
      sendSweetAlert(session = session,title = "Warning",text = 'Group All Genes in Defaut',type = 'warning')
    }
    groupstaistic=as.data.frame(table(sect_output_geneinfo$.group))
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
    
    removeUI(selector = '#modalbody>',immediate = T)
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
      type=msg$type
      description=input$custom_condition_description
      abbr=input$custom_condition_abbr
      code=input$custom_condition_code
    })
    if(type=='custom')
    {
      condition<<-rbind(condition,data.frame(description=description,abbr=abbr,used=T,core=core,task=msg$tasks,stringsAsFactors = F))
      rownames(condition)<<-condition$abbr
      write(x = code,file = paste(basepath,"/code/",abbr,'.R',sep=""))
    }
    else
    {
      condition[type,'used']<<-T
      condition[type,'core']<<-core
      condition[type,'task']<<-paste(unlist(tasks),collapse = ";")
    }
  })
  observeEvent(input$remove_condition,{
    isolate({
      msg=input$remove_condition
    })
    condition[msg$type,'used']<<-F
    condition[msg$type,'core']<<-0
  })
  observeEvent(input$compute_condition,{
    isolate({
      type=input$compute_condition$type
    })
    core=condition[type,'core']
   
    tasks=condition[type,'task']
    logpath=paste(basepath,'/log/',type,'.txt',sep="")
    
    if(type=="PCC")
    {
      if(dir.exists(paths = paste(basepath,'/log/')))
      {
        dir.create(paths = paste(basepath,'/log/'),recursive = T)
      }
      print('start')
      session$sendCustomMessage('calculation_eta',list(type=type,task="all",msg="Data Prepare",status='run'))
      filepath=paste(basepath,"/data/rna.exp.mat",sep="")
      writeMat(con=filepath,x=as.matrix(sect_output_rna.exp))
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
      
      rna.exp=sect_output_rna.exp
      micro.exp=sect_output_micro.exp
      target=sect_output_target
      geneinfo=sect_output_geneinfo
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
          complete=nchar(content[index+1])#完成数
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
      gene=rownames(sect_output_rna.exp)
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
          group1=rownames(sect_output_geneinfo)[which(sect_output_geneinfo$.group==groups[1])]
          group2=rownames(sect_output_geneinfo)[which(sect_output_geneinfo$.group==groups[2])]
          
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
    Map(function(task){
            removeUI(selector = paste("#density_plot",type,task,"image",sep="_"),immediate = T)
            insertUI(selector = paste("#density_plot",type,task,sep="_"),
                     where = 'beforeEnd',
                     ui = imageOutput(outputId = paste("density_plot",type,task,"image",sep="_"),width = "100%",height = "100%"),
                     immediate = T,session = session
            )
            figurepath=paste(basepath,'/Plot/density_plot_',type,"_",task,".svg",sep="")
            output[[paste("density_plot",type,task,"image",sep="_")]]<- renderImage({
                    list(src=figurepath,width="100%",height="100%")
                  },deleteFile = F)
            },tasks)
  })
})
