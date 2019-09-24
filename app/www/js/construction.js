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
          Shiny.setInputValue('choose_new_condition',obj)
        }
      }
      else
      {
        var obj={}
          obj['stamp']=Math.random()          
          obj['type']=$('#condition_type').val()
          Shiny.setInputValue('choose_new_condition',obj)

      }
      var $box=create_condition($('#condition_type').val(),1000,core)
      $('#condition_panel').append($("<div class='col-lg-4'></div>").append($box))
      $('#infolist').modal('hide')
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
  var $left=$('<span class="info-box-icon" id="icon_'+name+'"><a href="#"><i class="fa fa-play" style="color:#fff"></i></a></span>')
  var $right=$('<div class="info-box-content"></div>')
  var $title=$('<span class="info-box-number">'+name+'</span>')
  var $remove=$('<div style="float:right"><a href="#"><i class="fa fa-times" style="color:#fff"></i></a></div>')
  var $task=$('<span class="info-box-text" style="text-transform:none">Tasks: '+tasks+'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Cores: '+core+'</span>')
  var $progress=$('<div class="progress"><div class="progress-bar" id="progress_'+name+'" style="width:0%"></div></div>')
  var $eta=$('<span class="progress-description" id="eta_'+name+'">ETA:</span>')
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
  $right.append($title).append($task).append($progress).append($eta)
  $title.append($remove)
  return($box)
}

Shiny.addCustomMessageHandler('conditions',function(msg){
  conditions=msg
})