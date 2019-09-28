var conditons
$(document).ready(function(){
  $("#add_new_condition").on('click',function(e){
    var obj={}
    obj['stamp']=Math.random()
    Shiny.setInputValue('add_new_condition',obj)
    $('#modalSubmit').off('click').on('click',function(e){
      var description=$('#custom_condition_description').val()
      var abbr=$('#custom_condition_abbr').val()
      var code=$('#custom_condition_code').val()
      var core=$('#use_core').val()
      var tasks=$('#group_pairs').val()
      var type=$('#condition_type').val()
      if(tasks==null)
      {
        sweetAlert('warning','Warning..','Select Group Pairs!')
        return
      }
      check=function(id,candidate)
      {
        $('#'+id).parent().children('i').remove()
        if($('#'+id).val()=="")
        {
          var $error=$('<i class= "fa fa-close text-red">Empty!</i>')
          $('#'+id).parent().append($error)
        }
        else if(candidate!=null&&candidate.indexOf($('#'+id).val())>=0)
        {
          var $error=$('<i class= "fa fa-close text-red">Existed!</i>')
          $('#'+id).parent().append($error)
        }
        else
        {
          var ok=$('<i class= "fa fa-check text-green">OK</i>')
          $('#'+id).parent().append(ok)
        }
      }
      check('custom_condition_abbr',conditions.abbr)
      check('custom_condition_description',conditions.description)
      check('custom_condition_code',null)
      //自定义检查
      if($('#condition_type').val()=='custom')
      {
        if($('#modalbody .text-red').length>0)
        {
          sweetAlert('warning','Warning..','Invalid Input')
          return
        }
        else
        {
          var obj={}
          obj['stamp']=Math.random()          
          obj['type']='custom'
          obj['description']=description
          obj['abbr']=abbr
          obj['code']=code
        }
      }
      else
      {
        var obj={}
          obj['stamp']=Math.random()          
          obj['type']=$('#condition_type').val()
      }
      
      if(tasks.indexOf('all')>=0)
      {
        tasks='all'
      }
      if($('#body_'+type).length>0)
      {
        $('#core_'+type).text("Core:"+core)
        if(tasks instanceof Array)
        {
          $('#task_'+type).text("Tasks:0/"+tasks.length)
          $('#body_'+type).attr("tasks",tasks.join(";"))
        }
        else
        {
          $('#task_'+type).text("Tasks:0/1")
          $('#body_'+type).attr("tasks",tasks)
        }
      }
      else
      {
        if($('#condition_type').val()=='custom')
        {
          var $box=create_condition($('#custom_condition_abbr').val(),tasks,core)
        }
        else
        {
          var $box=create_condition($('#condition_type').val(),tasks,core)
        }
        $('#condition_panel').append($("<div class='col-lg-4'></div>").append($box))
      }
      
      obj['core']=core
      obj['tasks']=tasks
      
      $('#infolist').modal('hide');
      Shiny.setInputValue('choose_new_condition',obj)
    })
    if(!$('#infolist').hasClass('in'))
    {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
    else
    {
      $('#infolist').modal('hide');
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
  })
})
create_condition=function(name,tasks,core)
{
  var $box=$('<div class="info-box bg-red" id="body_'+name+'"></div>')
  if(tasks instanceof Array)
  {
    $box.attr('tasks',tasks.join(";"))
  }
  else
  {
     $box.attr('tasks',tasks)
  }
  var $left=$('<span class="info-box-icon" id="icon_'+name+'"><a href="#"><i class="fa fa-play" style="color:#fff"></i></a></span>')
  var $right=$('<div class="info-box-content"></div>')
  var $title=$('<span class="info-box-number">'+name+'</span>')
  var $remove=$('<div style="float:right"><a href="#"><i class="fa fa-times" style="color:#fff"></i></a></div>')
  var $taskpanel=$('<span class="info-box-text" style="text-transform:none;"></span>')
  var $task=$('<a href="#" style="color:#fff;padding-right:5px"><i class="fa fa-tasks" id="task_'+name+'" style="text-decoration:underline;"> Tasks:0\/'+ $box.attr('tasks').split(';').length+'</i></a>')
  var $core=$('<a href="#" style="color:#fff;padding-left:5px"><i class="fas fa-microchip" id="core_'+name+'" style="text-decoration:underline;"> Cores:'+core+"</a>")
  $taskpanel.append($task).append($core)
  var $progress=$('<div class="progress"><div class="progress-bar" id="progress_'+name+'" style="width:0%"></div></div>')
  var $eta=$('<span class="progress-description" id="eta_'+name+'">ETA:</span>')
  $task.on('click',function(e){
    var $current=$(e.currentTarget)
    var type=$current.parent().prev().text()
    var tasks=$current.parent().parent().parent().attr("tasks").split(";")
    var core=$current.next().children().text()
    var obj={}
    obj['stamp']=Math.random()
    obj['type']=type
    obj['tasks']=tasks
    obj['core']=core
    Shiny.setInputValue('add_new_condition',obj)
    
    if(!$('#infolist').hasClass('in'))
    {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
    else
    {
      $('#infolist').modal('hide');
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
  })
  $core.on('click',function(e){
    var $current=$(e.currentTarget)
    var type=$current.parent().prev().text()
    var tasks=$current.parent().parent().parent().attr("tasks").split(";")
    var core=$current.next().children().text()
    var obj={}
    obj['stamp']=Math.random()
    obj['type']=type
    obj['tasks']=tasks
    obj['core']=core
    Shiny.setInputValue('add_new_condition',obj)
    
    if(!$('#infolist').hasClass('in'))
    {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
    else
    {
      $('#infolist').modal('hide');
      $('#infolist').modal({backdrop: 'static', keyboard: false});
    }
  })
  $left.children('a').on('click',function(e){
    alert(123)
  })
  $remove.children('a').on('click',function(e){
    var id=$(e.currentTarget).parent().parent().parent().parent().attr('id')
    var type=id.substr(id.indexOf('_')+1)
    var obj={}
    obj['stamp']=Math.random()
    obj['type']=type
    Shiny.setInputValue('remove_condition',obj)
    $(e.currentTarget).parent().parent().parent().parent().parent().remove()
  })
  $box.append($left).append($right)//.append($go)
  $right.append($title).append($taskpanel).append($progress).append($eta)
  $title.append($remove)
  return($box)
}

Shiny.addCustomMessageHandler("clear_construction_task",function(msg){
  var $select_conditions=$('#condition_panel').children()
  for(var i=0;i<$select_conditions.length;++i)
  {
    var $cur=$($select_conditions.get(i)).children("div")
    $cur.attr('tasks',"")
    $("#"+$cur.attr("id").replace(/^body/,"task")).text('Tasks:0/0')
  }
  
})

Shiny.addCustomMessageHandler('conditions',function(msg){
  conditions=msg
})