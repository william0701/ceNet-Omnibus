var cy
$(document).ready(function(){
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
  $("#cy").children("div").css("height","1000px")
  $("#show_net_button_circle").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="circle"
    Shiny.setInputValue("network",obj)
  })
  $("#show_net_button_random").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="random"
    Shiny.setInputValue("network",obj)
  })
   $("#show_net_button_grid").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="grid"
    Shiny.setInputValue("network",obj)
  })
   $("#show_net_button_concentric").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="concentric"
    Shiny.setInputValue("network",obj)
  })
   $("#show_net_button_breadthfirst").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="breadthfirst"
    Shiny.setInputValue("network",obj)
  })
   $("#show_net_button_cose").on("click",function(e){
    var obj={}
    obj['stamp']=Math.random()
    obj['type']="cose"
    Shiny.setInputValue("network",obj)
  })
})

Shiny.addCustomMessageHandler("network",function(msg){
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
    padding: 30, // padding used on fit
  })
  layout.run()
})