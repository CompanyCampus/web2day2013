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
   
});


jQuery(function($){
   
	_.each($('.blocfloat2h'),function(i,a,y){
      var e = $(i);
      var ah = e.height();
      var v = blocwidth + (blocspace / 2);
      var nt = (ah+(v - (ah%v)))+((Math.floor(ah/v)-1)*(blocspace / 2));
      e.height(nt);
      
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