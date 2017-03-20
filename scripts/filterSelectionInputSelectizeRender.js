{
	option: function(item, escape) {
		
		var bgr_color = "#fff";
		var fgr_color = "#000";
		var item_type = item.value.includes(":") ? item.value.split(":")[0].toLowerCase() : "";
        var item_label = item.label;
        
        if(item_type != "") {
            item_label = item.label.split(":").slice(1).join(":");
        }
		
		switch(item_type) {		
		case "feature":
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
		var item_type = item.value.includes(":") ? item.value.split(":")[0].toLowerCase() : "";
        var item_label = item.label;
        
        if(item_type != "") {
            item_label = item.label.split(":").slice(1).join(":");
        }
		
		switch(item_type) {		
		case "feature":
			bgr_color = "#3498db";
            fgr_color = "#fff";
			break
		}		
		
		var style = 'border: 1px solid #ddd; border-radius: 3px; background-color: ' + bgr_color + '; color: ' + fgr_color;

		return '<div style="' + style +'" ><span>'+ escape(item_label) +'</span></div>';
	}
}
