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
        var e = $(i),
            children = e.children(),
            originalHeight = _.reduce(children, function(a, c) { return a + $(c).height(); }, 0),
            unitHeight = blocwidth + (blocspace / 2),
            delta = unitHeight - (originalHeight%unitHeight) + Math.floor(originalHeight / unitHeight - 1) * (blocspace / 2);

        $(i).height(originalHeight + delta);

	});
   
   if(($('.no_more_pic')).length == 0){
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
         $('<div class="optionalOnMobile bloc '+b_class_name+'"><img src="/'+insert+'"/></div>').insertAfter($(ch[n]));
      }
   });
   }
   
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


   	$(document).ready(function() {
         var opts = {
                     height: 616,
                     year:2013,
                     month:4,
                     date:17,
                     header:{
                        left:   '',
                        center: '',
                        right:  ''
                     },
                     defaultView: 'agendaDay',
            			events: 'https://www.google.com/calendar/feeds/gm9kigemqnjj5snv7cee19pb1c%40group.calendar.google.com/public/basic',
            			eventClick: function(event) {
            				// opens events in a popup window
            				window.open(event.url, 'gcalevent', 'width=700,height=600');
            				return false;
            			},
                     eventAfterRender:function( event, element, view ) {
                        var b_class_name = (function(kk){
                           switch(kk){
                              case "salle maxi" : return "salle_maxi";
                              case "salle mini" : return "salle_mini";
                           }
                        }(event.location));
                        console.log(arguments);
                        element.addClass(b_class_name);
                     }
            };
            var opts2 = _.clone(opts);
	
   		$('#calendar-friday').fullCalendar(opts);
         opts2['date'] = 16;
   		$('#calendar-thursday').fullCalendar(opts2);
		
   	});
