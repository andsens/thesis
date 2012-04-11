var Actor = Backbone.Model.extend({
	urlRoot : '/actors',
	defaults: {
		"name": "ActorName"
	}
},{
	getAttrType: function(attr_name) {
		switch(attr_name) {
			case 'id':
				return 'node';
			case 'name':
				return 'string';
			case 'roles':
				return 'collection';
		}
	},
	getComplexType: function(attr_name) {
		switch(attr_name) {
			case 'roles':
				return Filmography;
		}
	},mname: 'actor'
});

var ActorView = Backbone.View.extend({
	
	tagName: "span",
	events: {},
	
	template: _.template('<%= name %>'),
	render: function() {
		$(this.el).html(this.template(this.model.toJSON()));
		return this;
	}
}, {
	finders: {
		"id": {
			"xpath": "span",
			"process": function(node) {
				return $(node).attr('id').match(/[0-9]+/)[0];
			}
		},
		"name": {
			"xpath": "span/text()"
		}
	}
});

var Filmography = Backbone.Collection.extend({
  model: Role
});