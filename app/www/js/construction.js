$(document).ready(function(){
  $("#add_new_condition").on('click',function(e){
    var obj={}
    obj['stamp']=Math.random()
    Shiny.setInputValue('add_new_condition',obj)
    $('#modalsubmit').on('click',function(e){
      var description=$('#custom_condition_description').val()
      var abbr=$('#custom_condition_abbr').val()
      var code=$('#custom_condition_code').val()
      if(description==""||abbr==""||code=="")
      {
        sweetAlert('warning','Warning...','Input Empty!')
      }
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
create_condition=function(name)
{
  
}
Shiny.addCustomMessageHandler('redundent_condition',function(msg){
  
  if(msg.type=='error')
  {
    var $error=$('<i class="fa fa-close text-red">Redundent</i>')
  }
  else
  {
    var $error=$('<i class="fa fa-check text-green">Ok</i>')
  }
  $('#'+msg.id).parent().children('i').remove()
  $('#'+msg.id).parent().append($error)
})