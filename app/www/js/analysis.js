$(document).ready(function(){
  $("#add_new_topological_property").on('click',function(e){
    //$("#modalbody").empty()
    var obj={}
    obj['stamp']=Math.random()
    obj['select']=$("#network_property").children("div").attr("id")
    Shiny.setInputValue('add_network_property',obj)
    $('#modalSubmit').off('click').on("click",function(e){
      var obj={}
      obj['stamp']=Math.random()
      obj['select']=$("#network_property").children("div").attr("id")
      Shiny.setInputValue("select_network_property",obj)
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