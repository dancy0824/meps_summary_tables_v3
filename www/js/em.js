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
     $('.hide-if-trend').css('visibility','hidden').hide().fadeIn('fast');
     $('.show-if-trend').fadeIn('fast');
   }else{
     $('.hide-if-trend').css('visibility','visible').hide().fadeIn('fast');
     $('.show-if-trend').fadeOut('fast');
   }
   

 });

 
 /* I had to put this part inside modules_ui.R to leverage namespace
 
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
