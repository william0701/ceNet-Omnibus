$(document).ready(function(){
  $("a[href='#shiny-tab-analysis']").on("click",function(e){
    Shiny.setInputValue("initialization_enrichment",Math.random());
    
  });
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

function nodeDetails(btn)
{
   if(!$('#infolist').hasClass('in'))
   {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   else
   {
    $('#infolist').modal('hide');
    $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   $("#modalSubmit").off("click").on('click',function(e){
      $('#infolist').modal('hide');
   })
  Shiny.setInputValue("nodeDetails",Math.random())
  Shiny.addCustomMessageHandler("nodeDetails",function(e){
   
  })
}
function edgeDetails(btn)
{
   if(!$('#infolist').hasClass('in'))
   {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   else
   {
    $('#infolist').modal('hide');
    $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   $("#modalSubmit").off("click").on('click',function(e){
      $('#infolist').modal('hide');
   })
  Shiny.setInputValue("edgeDetails",Math.random())
}

function run_community_detection(obj)
{
  Shiny.setInputValue("community_detection",Math.random())
}

function run_enrichment_finish(obj)
{
  Shiny.setInputValue("enrichment_finish",Math.random())
}

function communityDetail(id)
{
  var obj={};
  obj['stamp']=Math.random();
  obj['moduleid']=id
  Shiny.setInputValue("communityDetals",obj)
   if(!$('#infolist').hasClass('in'))
   {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   else
   {
    $('#infolist').modal('hide');
    $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   $("#modalSubmit").off("click").on('click',function(e){
      $('#infolist').modal('hide');
   })
}
function communityEdgeDetail(id)
{
  var obj={};
  obj['stamp']=Math.random();
  obj['moduleid']=id
  Shiny.setInputValue("communityEdgeDetals",obj)
   if(!$('#infolist').hasClass('in'))
   {
      $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   else
   {
    $('#infolist').modal('hide');
    $('#infolist').modal({backdrop: 'static', keyboard: false});
   }
   $("#modalSubmit").off("click").on('click',function(e){
      $('#infolist').modal('hide');
   })
}

function displayCommunity(id)
{
  var obj={};
  obj['stamp']=Math.random();
  obj['moduleid']=id
  Shiny.setInputValue("displayCommunity",obj)
}

function module_setting(btn)
{
  var id=$(btn).attr("id")
  id=id.replace(/_setting$/,"")
  var obj={}
  obj['stamp']=Math.random()
  obj['id']=id
  Shiny.setInputValue("module_setting",obj)
  if(!$('#infolist').hasClass('in'))
  {
    $('#infolist').modal({backdrop: 'static', keyboard: false});
  }
  else
  {
    $('#infolist').modal('hide');
    $('#infolist').modal({backdrop: 'static', keyboard: false});
  }
  $("#modalSubmit").off('click').on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['id']=id
    Shiny.setInputValue("Update_community_style",obj)
    $("#infolist").modal("hide")
  })
}

function survival(obj)
{
  Shiny.setInputValue("execute_survival",Math.random())
}