{
	option: function(item, escape) {
		
		var bgr_color = "#fff";
		var fgr_color = "#000";
		var item_type = item.optgroup;
        var item_label = item.label;        
		
		switch(item_type) {		
		case "Associated features":
			bgr_color = "#3498db";
            fgr_color = "#fff";
			break
		}
		
		var style = 'display: inline-block; margin: 2px 5px 2px 5px ;border: 1px solid #ddd; border-radius: 3px; background-color: ' + bgr_color + '; color: ' + fgr_color;

		return '<div style="' + style +'" ><span>'+ escape(item_label) +'</span></div>';
	},
	item: function(item, escape) {
		
		var bgr_color = "#fff";
		var fgr_color = "#000";
		var item_type = item.optgroup;
        var item_label = item.label;        
		
		switch(item_type) {		
		case "Associated features":
			bgr_color = "#3498db";
            fgr_color = "#fff";
			break
		}
		
		var style = 'border: 1px solid #ddd; border-radius: 3px; background-color: ' + bgr_color + '; color: ' + fgr_color;

		return '<div style="' + style +'" class = "item" ><span>'+ escape(item_label) +'</span></div>';
	}
}