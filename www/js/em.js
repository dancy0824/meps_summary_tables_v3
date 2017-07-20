$("document").ready(function() {
  
  $('.dropdown-menu').on('click', function(e) {
    if($(this).hasClass('dropdown-menu-form')) {
      e.stopPropagation();
    }
  });
  
 $('.em-tooltip').tooltip({delay: {show: 500, hide: 10, trigger: "hover"}});

 $('.controls-trend').change(function(){
   is_trend = $('input[value="trend"]').is(':checked');

   if(is_trend){
   
    $('.year-start').animate({width: '100%'},400);
    $('.year-main label').text('to:');

    $('.hide-if-trend.slide').slideUp('fast'); 
    
   }else{
     
     $('.year-start').animate({width:'0%'},400);
     $('.year-main label').text('Year:');
   
     $('.hide-if-trend.slide').slideDown('fast');
   }
   

 });

 
 /* I had to put this part inside module2_levels.R to leverage namespace
 
  $('.dropdown').on('shown.bs.dropdown', function() {
    var is_open = 'true';
    Shiny.onInputChange('mydata', is_open);
    console.log("Dropdown menu opened");
  });

  $('.dropdown').on('hidden.bs.dropdown', function() {
    var is_open = 'false';
    Shiny.onInputChange('mydata', is_open);
    console.log("Dropdown menu cloased");
  });
  */
});
