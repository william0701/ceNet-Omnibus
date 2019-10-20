var cy
$(document).ready(function(){
  $('#cy').attr("style","width:100%;height:100%;position:relative;")
  cy=cytoscape({
    container:$("#cy"),
    elements:[],
    style: [ // the stylesheet for the graph
              {
                selector: 'node',
                style: {
                  'background-color': '#666',
                  'label': 'data(id)'
                }
              },
              {
                selector: 'edge',
                style: {
                  'width': 3,
                  'line-color': '#ccc',
                  'target-arrow-color': '#ccc',
                  'target-arrow-shape': 'triangle'
                }
              }
  ],

  layout: {
    name: 'grid',
    rows: 1
  }
  })
  $("a[href='#shiny-tab-visualization']").on('click',function(e){
    Shiny.setInputValue("network",Math.random())
  })
})

Shiny.addCustomMessageHandler("network",function(msg){
  var t=[
  { group: 'nodes', data: { id: 'n0' }, position: { x: 100, y: 100 } },
  { group: 'nodes', data: { id: 'n1' }, position: { x: 200, y: 200 } },
  { group: 'edges', data: { id: 'e0', source: 'n0', target: 'n1' } }
]
  cy.add(msg.nodes)
  cy.add(msg.edge)
  var layout=cy.layout({
    name:'grid',
    fit: true, // whether to fit the viewport to the graph
    padding: 30, // padding used on fit
  })
  layout.run()
})