$(document).ready(function(){
  $("#add_new_condition").on('click',function(e){
    var obj={}
    obj['stamp']=Math.random()
    Shiny.setInputValue('add_new_condition',obj)
    $('#modalsubmit').on('click',function(e){
      
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