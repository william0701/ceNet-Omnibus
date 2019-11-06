var cy
var network_color = "red"
var network_shape = "star"
$(document).ready(function(){
   $("a[href='#shiny-tab-visualization']").on("click",function(e){
     var obj=Math.random()
     Shiny.setInputValue("change_network_name",obj)
     $("#cy").children("div").css("height","1000px")
   })
   $('#cy').attr("style","width:100%;height:100%;position:relative;z-index:0;left: 0;top: 0;")
   cy=cytoscape({
     container:$("#cy"),
     elements:[],
     style: [ // the stylesheet for the graph
              {
                selector: 'node',
                style: {
                  'background-color': '#ad1a66',
                  'label': 'data(id)'
                }
              },
              {
                selector: 'edge',
                style: {
                  'width': 0.5,
                  /*'line-color': '#ad1a66'*/
                  'curve-style': 'haystack'
                }
              }
    ],

    layout: {
        name: 'circle',
        animate: false
    }
  })
/*  $("#cy").children("div").css("height","1000px")*/
  
  var $button_change_layout=$('<div class="input-group-btn"><button type="button" class="btn btn-success dropdown-toggle" data-toggle="dropdown" aria-expanded="false">Which label to choose as the layout<span class="fa fa-caret-down"></span></button><ul class="dropdown-menu"></ul></div>')
  var $button_nameChoose=$('<button type="button" class="btn btn-danger dropdown-toggle" data-toggle="dropdown" aria-expanded="false">Which Column as Gene Name<span class="fa fa-caret-down"></span></button>')
  var $ul_nameChoose=$('<ul class="dropdown-menu"></ul>')
  var $button_change_color=$('<div class="input-group-btn"><button type="button" class="btn btn-success dropdown-toggle" data-toggle="dropdown" aria-expanded="false">Which label to choose as the gene type<span class="fa fa-caret-down"></span></button><ul class="dropdown-menu"></ul></div>')
  var $nerwork_p = $('<div class="form-group"><h4 style="font-family:Georgia;font-weight:bold">choose network color and shape</h4></div>')
  $("#choose_differ_layout").append($button_change_layout)
  var layout_name=new Array("circle","random","grid","concentric","breadthfirst","cose")
  for(var i=0;i<layout_name.length;i++){
    create_net_layout(layout_name[i])
  }
  $("#change_network_color").append($nerwork_p)
  $("#choose_differ_name").append($button_nameChoose).append($ul_nameChoose)
  $nerwork_p.append($button_change_color)
  var $table_color = $('<div class="col-lg-6" style="padding:0px"><div class="table" style="margin-top:5px"></div></div>')
   $("#change_network_color").children('div.form-group').append($table_color)
        
     
})
create_net_layout = function(name){
    var $li =$('<li></li>')
    var $a =$('<a>'+name+'</a>')
    $('#choose_differ_layout').children('div').children('ul').append($li)
    $li.append($a)
    $li.on("click",function(){
      var obj={}
      obj['stamp']=Math.random()
      obj['type']=name
      obj['do_what']="layout"
      Shiny.setInputValue("network",obj)
    })
}
creat_changeName = function(name){
  var $li =$('<li></li>')
  var $a =$('<a>'+name+'</a>')
  $('#choose_differ_name').children('ul').append($li)
  $li.append($a)
  $li.on("click",function(e){
    cy.style().selector('node').style('label', 'data('+name+')').update()
  })
}
create_net_change_module_pre = function(name){
  var $li =$('<li></li>')
  var $a =$('<a>'+name+'</a>')
  $('#change_network_color').children('div.form-group').children('div').children('ul').append($li)
  $li.append($a)
  $li.on("click",function(e){
     var obj={}
     obj['stamp']=Math.random()
     obj['type']=name
     Shiny.setInputValue("net_color_shape",obj)
  })
}
create_net_change_module = function(type){
  $table_tr = $('<div class="table-tr"></div>')
  $table_tdname = $('<div class="table-tdr"><small class="bg-green" style="border-radius:10px;padding:3px 7px">'+type+'</small></div>')
  $table_tdcolor = $('<div class="table-tdl"> <input class="jscolor" style="border-radius:15px;text-align:center" value="ab2567"></div>')
  $("#change_network_color").children('div.form-group').children('div.col-lg-6').children('div.table').append($table_tr)
  $table_tr.append($table_tdname).append($table_tdcolor)
/*  $divcolor.children('p').append($buttoncolor)
  var $select = $('<select class="form-control" style="width:100px;height:30px; text-align: center;"></select>')
  $option1 = $('<option>star</option>')
  $option2 = $('<option>triangle</option>')
  $option3 = $('<option>round-triangle</option>')
  $option4 = $('<option>rectangle</option>')
  $option5 = $('<option>diamond</option>')
  $divouter.append($select)
  $select.append($option1).append($option2).append($option3).append($option4).append($option5)*/
}
/*creat_changeColor = function(name){
  var $li =$('<li></li>')
  var $a =$('<a>'+name+'</a>')
  $('#change_network_color').children('ul').append($li)
  $li.append($a)
  $li.on("click",function(e){
    cy.style().selector('node[group="'+name+'"]').style('background-color',''+network_color).style('shape',''+network_shape).update()
  })
}
Shiny.addCustomMessageHandler("Gene_network_color_change",function(msg){
  $("#change_network_color").children('ul').empty();
  for(var i=0;i<msg.type.length;i++){
    creat_changeColor(msg.type[i])
  }
})*/
Shiny.addCustomMessageHandler("network",function(msg){
  if(msg.do_what=="layout"){
     var collection = cy.elements('node');
     cy.remove( collection );
     collection = cy.elements('edge');
     cy.remove( collection );
     cy.add(msg.nodes)
     cy.add(msg.edge)
     
  /*var width=$("#cy").children("div").css("width")
  var height=$("#cy").children("div").css("height")
  width=parseInt(width.replace(/px/,""))
  height=parseInt(height.replace(/px/,""))*/
     cy.center()
     cy.fit()
     var layout=cy.layout({
       name:msg.type,
       fit: true, // whether to fit the viewport to the graph
       padding: 30,
       animate: true,
       animationDuration: 2000// padding used on fit
     })
     layout.run()
  }
})
Shiny.addCustomMessageHandler("Gene_info_name_change",function(msg){
  $("#choose_differ_name").children('ul').empty();
  for(var i=0;i<msg.length;i++){
    if(msg[i]==".group"){
      msg[i]="group"
    }
    creat_changeName(msg[i])
    create_net_change_module_pre(msg[i])
  }
})
Shiny.addCustomMessageHandler("Gene_network_color_change",function(msg){
  $("#change_network_color").children('div.form-group').children('div.col-lg-6').children('div.table').empty();
  $table_head=$('<div class="table-tr"><div class="table-th">Item</div><div class="table-th">Color</div></div>')
  $("#change_network_color").children('div.form-group').children('div.col-lg-6').children('div.table').append($table_head)
  for(var i=0;i<msg.type.length;i++){
    create_net_change_module(msg.type[i])
  }
  window.jscolor();
})