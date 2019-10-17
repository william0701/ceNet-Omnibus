
//creat filter modal
creat_geneFilter = function(title,inputId,type)
{ 
  var $modal = $('<div class="gene_filter construct col-lg-4" style="border:1px solid #ccc" id="gene_Group_'+inputId+'_panel"></div>');
  var $title = $('<h4>'+title+'</h4><hr style="margin-top:0px;margin-bottom:0px">');
  var $body = $('<div class="form-group col-lg-6" style="padding:0px"></div>')
  var $label = $('<label class="control-label">Minimal expression value</label> ')
  var $inputModal = $('<div class="input-group "></div>');
  var $inPut = $('<input type="text" style="text-align:center" value=0  class="form-control bfh-number" data-min="5" data-max="25" exist="F" GeneType='+type+' id=gene_slice_value_'+inputId+'>');
  var $span1 = $('<span class="input-group-btn"></span>');
  var $buttonPlus = $('<button class="btn btn-default btn-flat" type="button"><i class="fa fa-plus-square"></i></button>');
  var $button1 = $('<button class="btn btn-default btn-flat" type="button">Preview</button>');
  var $buttonMinus = $('<span class="input-group-btn"></span>').append($('<button class="btn btn-default btn-flat" type="button"><i class="fa fa-minus-square"></i></button>'));
  $modal.append($title);
  $modal.append($body);
  $body.append($label).append($inputModal);
  $inputModal.append($buttonMinus).append($inPut).append($span1);
  $span1.append($buttonPlus).append($button1);
 
    $('#Gene_Filter_all').append($modal);
  
  $inPut.on("change",function(e){
    var reg=/^[0-9]+(\.[0-9]+)?$/;
    if(!$(e.currentTarget).val().match(reg)){
      $(e.currentTarget).val(0);
      sweetAlert("warning","warning..","Invalid Input!");
    }
  })
  $buttonPlus.on("click",function(e){
    var value=parseFloat($(e.currentTarget).parent().prev().val());
    $(e.currentTarget).parent().prev().val(value+0.5);
  })
  $buttonMinus.children("button").on("click",function(e){
    var value=parseFloat($(e.currentTarget).parent().next().val());
    $(e.currentTarget).parent().next().val(value-0.5<0?0:value-0.5);
  })
   //choose a percent filter value modal
  var $labelRight = $('<label class="control-label">Minimal Sample Ratio</label>')
  var $bodyGroup = $('<div class="input-group "></div>');
  var $bodyRight = $('<div class="form-group col-lg-6" style=""></div>');
  var $spanInner = $('<span class="input-group-btn" style="padding-left:10px"></span>');
  $modal.append($bodyRight);
  var $Slider = $('<input type="text" value=""/>');
  $bodyRight.append($labelRight).append($bodyGroup);
  $bodyGroup.append($Slider).append($spanInner);
  var value=0.5;
  $Slider.ionRangeSlider({
        grid: true,
        min: 0,
        max: 1,
        from: 0.5,
        hide_min_max:true,
        step:0.01,
        onFinish: function (data) {
          value=data.from;
          $button1.trigger("click");
        }
  });
 
 
  //picture
  $button1.on("click",function(e){
    var number=$(e.currentTarget).parent().prev().val();
    var obj={}
    obj['stamp']=Math.random();
    obj['type']=$(e.currentTarget).parent().prev().attr("GeneType");
    obj['number']=number;
    obj['group']=$(e.currentTarget).parent().prev().attr("id");
    obj['exist']=$(e.currentTarget).parent().prev().attr("exist");
    obj['line']=value;
    Shiny.setInputValue('Gene_Filter_Signal',obj);
    $(e.currentTarget).parent().prev().attr("exist","T");
  })
}
creat_logtrans_button = function(opera,input,tip){
  var $button =$('<a class="btn btn-app btn-info" style="margin:5px"><i class="fa fa-play"></i>'+input+'</a>')
  var $span =$('<span class="badge bg-yellow">ok</span>')
  var $spantip =$('<span style="visibility: hidden;background-color: black;color: #fff;text-align: center;border-radius: 6px;padding: 5px 0;position: absolute;left:10px;top:130px;z-index:1;">'+tip+'</span>')
  $("#Value_Transform_all").find("div:first-child").append($button).append($spantip);
  $button.on("click",function(e){
    var obj={}
    obj['stamp']=Math.random();
    obj['opera']=opera;
    $button.append($span);
    Shiny.setInputValue('Value_Transform_Signal',obj);
  })
  $button.hover(function(){
      $spantip.css("visibility","visible")},
    function(){
      $spantip.css("visibility","hidden")
  })
}
creat_normtrans_button = function(opera,input,tip){
  var $button =$('<a class="btn btn-app " style="margin:5px;"><i class="fa fa-play"></i>'+input+'</a>')
  var $span =$('<span class="badge bg-yellow">ok</span>')
  var $spantip =$('<span style="visibility: hidden;background-color: black;color: #fff;text-align: center;border-radius: 6px;padding: 5px 0;position: absolute;left:10px;top:130px;z-index:1;">'+tip+'</span>')
  $("#Value_Transform_all").find("div:nth-child(2)").append($button).append($spantip);
  $button.on("click",function(e){
    var obj={}
    obj['stamp']=Math.random();
    obj['opera']=opera;
    $button.append($span);
    Shiny.setInputValue('Normalized_Signal',obj);
  })
  $button.hover(function(){
      $spantip.css("visibility","visible")},
    function(){
      $spantip.css("visibility","hidden")
  })
}
//qiefen

slice_gene=function(e){
    var number=$(e).children("div").children("div").find(".form-control").val();
    var obj={}
    var slider=$(e+">:nth-child(4)").children("div").children("input").data("from")
    obj['stamp']=Math.random();
    obj['first']=$(e).children("div").children("div").find(".form-control").attr("first");
    obj['type']=$(e).children("div").children("div").find(".form-control").attr("GeneType");
    obj['number']=number;
    obj['group']=$(e).attr("id");
    obj['line']=slider;
    Shiny.setInputValue('Gene_Slice_Signal',obj);
  }
Shiny.addCustomMessageHandler('gene_type_infomation',function(msg){
  var len=msg.group.length;
  $('#gene_Group_microFilterPlot_panel').nextAll().remove();
  for(var i=0;i<len;i++){
     creat_geneFilter("RNA Filter For Group:"+msg.group[i],msg.group[i],"Rna");
  }
});