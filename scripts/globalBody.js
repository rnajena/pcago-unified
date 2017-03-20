/**
 * Enables popovers (used for inplace help)
 **/
$(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover({
      container: 'body',
      html: true
    });   
});

/**
 * Make selectize items removable by clicking them
 **/
/*$(document).on('click', 'div.selectize-input div.item', function(e) {
    
    var control = $(this).parent().parent().parent().children("select").first().selectize;
    var value = $(this).attr("data-value");

    console.log(value);
    
    
    control.refreshItems();
    control.refreshOptions();
});*/
