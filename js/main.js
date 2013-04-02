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
            originalHeight = e.height();// _.reduce(children, function(a, c) { return a + $(c).height(); }, 0),
            unitHeight = blocwidth + (blocspace / 2),
            delta = unitHeight - (originalHeight%unitHeight) + Math.floor(originalHeight / unitHeight - 1) * (blocspace / 2);

        $(i).height(originalHeight + delta);

	});
   
   if(($('.no_more_pic')).length == 0){
   _.each(my_images,function(v, k){
      var is_square = (_.uniq(k.split("x")).length == 1);
      var r = _.random(0, (is_square ? 1 : 3));
      var b_class_name = (function(kk){
         switch(kk){
            case "200x200" : return "bloc1";
            case "408x408" : return "bloc2v2h";
            case "408x200" : return "bloc2h";
            case "200x408" : return "bloc2v";
         }
      }(k));
      for(var i = 0 ; i < r ; i++){
         var insert = v[_.random(0,_.size(v)-3)];
         var c  = $('#content');
         var ch = c.find('div.bloc');
         var n = _.random(0,_.size(ch)-1);
         $('<div class="optionalOnMobile bloc '+b_class_name+'"><img src="/'+insert+'"/></div>').insertAfter($(ch[n]));
      }
   });
   }
   
   $('.randomize_plz').randomize();
   
   $('.carousel').carousel({interval:10000});
   
   $('.carousel').each(function( index, el ) {
      $(el).find('.item:first').addClass('active');
   });

   if($('.no_partners').length != 0) {
      $("#footer .partners-bloc").remove();
   }
   
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






      /* ==========================================================
       * bootstrap-carousel.js v2.3.1
       * http://twitter.github.com/bootstrap/javascript.html#carousel
       * ==========================================================
       * Copyright 2012 Twitter, Inc.
       *
       * Licensed under the Apache License, Version 2.0 (the "License");
       * you may not use this file except in compliance with the License.
       * You may obtain a copy of the License at
       *
       * http://www.apache.org/licenses/LICENSE-2.0
       *
       * Unless required by applicable law or agreed to in writing, software
       * distributed under the License is distributed on an "AS IS" BASIS,
       * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       * See the License for the specific language governing permissions and
       * limitations under the License.
       * ========================================================== */


      !function ($) {

        "use strict"; // jshint ;_;


       /* CAROUSEL CLASS DEFINITION
        * ========================= */

        var Carousel = function (element, options) {
          this.$element = $(element)
          this.$indicators = this.$element.find('.carousel-indicators')
          this.options = options
          this.options.pause == 'hover' && this.$element
            .on('mouseenter', $.proxy(this.pause, this))
            .on('mouseleave', $.proxy(this.cycle, this))
        }

        Carousel.prototype = {

          cycle: function (e) {
            if (!e) this.paused = false
            if (this.interval) clearInterval(this.interval);
            this.options.interval
              && !this.paused
              && (this.interval = setInterval($.proxy(this.next, this), this.options.interval))
            return this
          }

        , getActiveIndex: function () {
            this.$active = this.$element.find('.item.active')
            this.$items = this.$active.parent().children()
            return this.$items.index(this.$active)
          }

        , to: function (pos) {
            var activeIndex = this.getActiveIndex()
              , that = this

            if (pos > (this.$items.length - 1) || pos < 0) return

            if (this.sliding) {
              return this.$element.one('slid', function () {
                that.to(pos)
              })
            }

            if (activeIndex == pos) {
              return this.pause().cycle()
            }

            return this.slide(pos > activeIndex ? 'next' : 'prev', $(this.$items[pos]))
          }

        , pause: function (e) {
            if (!e) this.paused = true
            if (this.$element.find('.next, .prev').length && $.support.transition.end) {
              this.$element.trigger($.support.transition.end)
              this.cycle(true)
            }
            clearInterval(this.interval)
            this.interval = null
            return this
          }

        , next: function () {
            if (this.sliding) return
            return this.slide('next')
          }

        , prev: function () {
            if (this.sliding) return
            return this.slide('prev')
          }

        , slide: function (type, next) {
            var $active = this.$element.find('.item.active')
              , $next = next || $active[type]()
              , isCycling = this.interval
              , direction = type == 'next' ? 'left' : 'right'
              , fallback  = type == 'next' ? 'first' : 'last'
              , that = this
              , e

            this.sliding = true

            isCycling && this.pause()

            $next = $next.length ? $next : this.$element.find('.item')[fallback]()

            e = $.Event('slide', {
              relatedTarget: $next[0]
            , direction: direction
            })

            if ($next.hasClass('active')) return

            if (this.$indicators.length) {
              this.$indicators.find('.active').removeClass('active')
              this.$element.one('slid', function () {
                var $nextIndicator = $(that.$indicators.children()[that.getActiveIndex()])
                $nextIndicator && $nextIndicator.addClass('active')
              })
            }

            if ($.support.transition && this.$element.hasClass('slide')) {
              this.$element.trigger(e)
              if (e.isDefaultPrevented()) return
              $next.addClass(type)
              $next[0].offsetWidth // force reflow
              $active.addClass(direction)
              $next.addClass(direction)
              this.$element.one($.support.transition.end, function () {
                $next.removeClass([type, direction].join(' ')).addClass('active')
                $active.removeClass(['active', direction].join(' '))
                that.sliding = false
                setTimeout(function () { that.$element.trigger('slid') }, 0)
              })
            } else {
              this.$element.trigger(e)
              if (e.isDefaultPrevented()) return
              $active.removeClass('active')
              $next.addClass('active')
              this.sliding = false
              this.$element.trigger('slid')
            }

            isCycling && this.cycle()

            return this
          }

        }


       /* CAROUSEL PLUGIN DEFINITION
        * ========================== */

        var old = $.fn.carousel

        $.fn.carousel = function (option) {
          return this.each(function () {
            var $this = $(this)
              , data = $this.data('carousel')
              , options = $.extend({}, $.fn.carousel.defaults, typeof option == 'object' && option)
              , action = typeof option == 'string' ? option : options.slide
            if (!data) $this.data('carousel', (data = new Carousel(this, options)))
            if (typeof option == 'number') data.to(option)
            else if (action) data[action]()
            else if (options.interval) data.pause().cycle()
          })
        }

        $.fn.carousel.defaults = {
          interval: 5000
        , pause: 'hover'
        }

        $.fn.carousel.Constructor = Carousel


       /* CAROUSEL NO CONFLICT
        * ==================== */

        $.fn.carousel.noConflict = function () {
          $.fn.carousel = old
          return this
        }

       /* CAROUSEL DATA-API
        * ================= */

        $(document).on('click.carousel.data-api', '[data-slide], [data-slide-to]', function (e) {
          var $this = $(this), href
            , $target = $($this.attr('data-target') || (href = $this.attr('href')) && href.replace(/.*(?=#[^\s]+$)/, '')) //strip for ie7
            , options = $.extend({}, $target.data(), $this.data())
            , slideIndex

          $target.carousel(options)

          if (slideIndex = $this.attr('data-slide-to')) {
            $target.data('carousel').pause().to(slideIndex).cycle()
          }

          e.preventDefault()
        })

      }(window.jQuery);