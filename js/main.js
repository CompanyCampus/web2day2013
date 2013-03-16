/*
$(function(){
	_.each($('.bloc'),function(i,a,y){
		$(i).css('background-color','rgb('+(a*20)+',255,'+(250-a*25)+')');
	});
});
*/

var blocwidth= 200;
var blocspace=8;

$.fn.randomize = function(selector){
    var $elems = selector ? $(this).find(selector) : $(this).children(),
        $parents = $elems.parent();

    $parents.each(function(){
        $(this).children(selector).sort(function(){
            return Math.round(Math.random()) - 0.5;
        }).remove().appendTo(this);
    });

    return this;
};




jQuery(function($){
   
	_.each($('.blocfloat2h'),function(i,a,y){
      var e = $(i);
      var ah = e.height();
      var v = blocwidth + (blocspace / 2);
      var nt = (ah+(v - (ah%v)))+((Math.floor(ah/v)-1)*(blocspace / 2));
      e.height(nt);
      
	});
   
   _.each(my_images,function(v, k){
      var is_square = (_.uniq(k.split("x")).length == 1);
      var r = _.random(0, (is_square ? 1 : 2));
      var b_class_name = (function(kk){
         switch(kk){
            case "200x200" : return "bloc1";
            case "408x408" : return "bloc2v2h";
            case "408x200" : return "bloc2h";
            case "200x408" : return "bloc2v";
         }
      }(k));
      for(var i = 0 ; i < r ; i++){
         var insert = v[_.random(0,_.size(v)-1)];
         var c  = $('#content');
         var ch = c.find('div.bloc');
         var n = _.random(0,_.size(ch)-1);
         $('<div class="bloc '+b_class_name+'"><img src="/'+insert+'"/></div>').insertAfter($(ch[n]));
      }
   });
   
   $('.randomize_plz').randomize();
   
   
	var mainpage = $('#content');
	mainpage.masonry({
		isAnimated: true,
		itemSelector:'.bloc:not(.hidden)',
//		isFitWidth:true,
		columnWidth:blocwidth + (blocspace)
	});
	/*
	$('h1 a').click(function(e){
		var cls = $(this).attr('href').replace('#','');
		mainpage.find('.bloc').removeClass('hidden'); 
		mainpage.find('.bloc:not(.'+cls+')').addClass('hidden');
		mainpage.masonry('reload'); 
		mainpage.find('.'+cls).show(500);
		mainpage.find('.bloc:not(.'+cls+')').hide(500);

		location.hash = cls;
		e.preventDefault(); 
	});

	var bloc = mainpage.find('.bloc:first'); 
	var cssi = {width:bloc.width(),height:bloc.height()};
	var cssf = null; 

	mainpage.find('a.thumb').click(function(e){
		var elem = $(this); 
		var cls = elem.attr('href').replace('#','');
		var fold = mainpage.find('.unfold').removeClass('unfold').css(cssi); 
		var unfold = elem.parent().addClass('unfold').css(cssf); 
		mainpage.masonry('reload'); 
		if(cssf == null){
			cssf = {
				width : unfold.width(),
				height: unfold.height()
			};
		}
		unfold.css(cssi).animate(cssf);
	});

	if(location.hash != ''){
		$('a[href="'+location.hash+'"]').trigger('click');
	}
	*/
});