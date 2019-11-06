
//creat filter modal
creat_sampleFilter = function(inputId,inputName)
{ 
  
  var $modal = $('<div class="gene_filter construct col-lg-6" exist="F" style="border:1px solid #ccc" id="sample_Group_'+inputName+'_panel"></div>');
  var $dabiaoti=$('<h4>'+inputId+'</h4><hr style="margin-top:0px;margin-bottom:0px">')
  var $invalid_value=$('<div class="form-group col-lg-6" shiny-input-radiogroup shiny-input-container shiny-input-container-inline shiny-bound-input></div>')
  var $titlevalid = $('<label class="control-label">Invalid Value</label>')
  var $div1=$('<div class="input-group margin"></div>')
  var $shinygroup=$('<div class="shiny-options-group "></div>');
  var $checkboxgroup1=$('<div class="pretty p-default p-curve"></div>');
  var $checkboxgroup2=$('<div class="pretty p-default p-curve"></div>');
  var $checkboxgroup3=$('<div class="pretty p-default p-curve"></div>');
  var $checkboxgroup4=$('<div class="pretty p-default p-curve"></div>');
  var $label1=$('<div class="state p-danger"><label>0</label></div>')
  var $label2=$('<div class="state p-danger"><label>NA</label></div>')
  var $label3=$('<div class="state p-danger"><label>NULL</label></div>')
  var $label4=$('<div class="state p-danger"><label>NaN</label></div>')
  var $input1=$('<input type="checkbox" name="'+inputName+'" value="0"> ')
  var $input2=$('<input type="checkbox" name="'+inputName+'" value="NA"> ')
  var $input3=$('<input type="checkbox" name="'+inputName+'" value="NULL"> ')
  //这里读NaN不好统一处理成大写，所以读进来的时候就把他的值设成大写。
  var $input4=$('<input type="checkbox" name="'+inputName+'" value="NAN"> ')
  
  var $button_out=$('<div class="input-group-btn"></div>')
  var $sample_button=$('<button type="button" class="btn btn-default btn-flat">preview</button>')
  
  $modal.append($dabiaoti).append($invalid_value).append($div1);
  $invalid_value.append($titlevalid).append($div1);
  $div1.append($shinygroup).append($button_out);
  $button_out.append($sample_button);
  $shinygroup.append($checkboxgroup1).append($checkboxgroup2).append($checkboxgroup3).append($checkboxgroup4);
  
  $checkboxgroup1.append($input1).append($label1);
  $checkboxgroup2.append($input2).append($label2);
  $checkboxgroup3.append($input3).append($label3);
  $checkboxgroup4.append($input4).append($label4);

  
  
  $('#Sample_Filter_all').append($modal);
  
  //choose a percent filter value modal
  var $labelRight = $('<label class="control-label">Minimal Sample Ratio</label>')
  var $bodyGroup = $('<div class="input-group "></div>');
  var $bodyRight = $('<div class="form-group col-lg-12" style=""></div>');
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
      $sample_button.trigger("click");
    }
  });
  
  //jquery获取复选框值    
  var chk_value =[];//定义一个数组
  
  $sample_button.on("click",function(e){
    chk_value =[];
    $('input[name="'+inputName+'"]:checked').each(function(){//遍历每一个名字为xx的复选框，其中选中的执行函数    
      chk_value.push($(this).val());//将选中的值添加到数组chk_value中    
    })
    var obj={}
    obj['group']=inputName;
    obj['stamp']=Math.random();
    obj['sep']=chk_value;
    obj['exist']=$modal.attr('exist');
    obj['value']=value;
    Shiny.setInputValue('Sample_Filter',obj);
    $modal.attr('exist','T');
  })
  
  //picture
  
}

slice=function(e){
    var obj={}
    var slider=$(e+">:nth-child(4)").children("div").children("input").data("from")
    obj['stamp']=Math.random();
    obj['group']=$(e).attr("id");
    obj['line']=slider;
    Shiny.setInputValue('Sample_Slice_Signal',obj);
  }