var Root = Backbone.Model.extend({
	
},{
	getAttrType: function(attr_name) {
		switch(attr_name) {
			case 'movies':
				return 'collection';
		}
	},
	getComplexType: function(attr_name) {
		switch(attr_name) {
			case 'movies':
				return MovieLibrary;
		}
	},mname: 'root'
});

var RootView = Backbone.View.extend({
	
	tagName: "",
	events: {
		"click #add_movie_button": "add_movie"
	},
	add_movie: function() {
		var movie = new Movie();
	}
	
}, {
	finders: {
		"title": {
			"xpath": "/html/head/title/text()"
		},
		"movies": {
			"xpath": "/html/body/section[@id='movies']/ol/li",
			"view": function() {return MovieView;}
		}
	}
});

var MovieLibrary = Backbone.Collection.extend({
  model: Movie
});
