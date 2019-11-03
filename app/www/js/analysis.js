$(document).ready(function(){
   check123()
  /*$("#add_new_topological_property").on('click',function(e){
   
    //$("#modalbody").empty()
    var obj={}
    var select=[]
    $("#network_property").children("div").each(function(i,item){
      select.push($(item).attr("id"))
    })
    obj['stamp']=Math.random()
    obj['select']=select
    Shiny.setInputValue('add_network_property',obj)
    $('#modalSubmit').off('click').on("click",function(e){
      var obj={}
      var select=[]
      $("#network_property").children("div").each(function(i,item){
        select.push($(item).attr("id"))
      })
      obj['select']=select
      obj['stamp']=Math.random()
      Shiny.setInputValue("select_network_property",obj)
      $('#infolist').modal('hide');
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
  })*/
})

function showNodeCentrality(box)
{
  var values=[]
  $("input[name='"+$(box).attr("name")+"']:checked").each(function(i,ele){
    values.push($(ele).val())
  })
  var obj={}
  obj['stamp']=Math.random()
  obj['value']=values
  Shiny.setInputValue("nodeCentrality",obj)
}

function showEdgeCentrality(box)
{
  var values=[]
  $("input[name='"+$(box).attr("name")+"']:checked").each(function(i,ele){
    values.push($(ele).val())
  })
  var obj={}
  obj['stamp']=Math.random()
  obj['value']=values
  Shiny.setInputValue("edgeCentrality",obj)
}