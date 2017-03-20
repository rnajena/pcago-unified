/**
 * Enables popovers (used for inplace help)
 **/
$(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover({
      container: 'body',
      html: true
    });   
});


