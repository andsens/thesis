var Movie = Backbone.Model.extend({
	urlRoot : '/movies',
	defaults: {
		"title": "Title",
		"year": "0000"
	}
},{
	getAttrType: function(attr_name) {
		switch(attr_name) {
			case 'id':
				return 'node';
			case 'cast':
				return 'collection';
		}
	},
	getComplexType: function(attr_name) {
		switch(attr_name) {
			case 'cast':
				return Cast;
		}
	},mname: 'movie'
});

var MovieView = Backbone.View.extend({
	
	tagName: "li",
	events: {
		"click .operations .edit": "toggleEditMode",
		"keyup h1": "update",
		"click #add_actor_button": "newRole"
	},
	
	editing: false,
	toggleEditMode: function() {
		this.editing = !this.editing;
		this.$('h1, .year, .synopsis>span, .plot p').attr('contenteditable', this.editing);
		if(this.editing) {
			this.$('.operations .edit').text('Save');
		} else {
			this.$('.operations .edit').text('Saving');
			this.model.set('title', this.$('h1').text());
			this.model.set('year', this.$('.year').text());
			this.model.set('synopsis', this.$('.synopsis>span').text());
			this.model.set('plot', this.$('.plot p').text());
			this.model.save(null, {
				success: _.bind(function() {
					this.$('.operations .edit').text('Done!');
					setTimeout(_.bind(function() {
						this.$('.operations .edit').text('Edit');
					}, this), 500);
				}, this)
			});
		}
	},
	
	update: function() {
		var title = this.$('h1').text();
		this.$('summary.title').text(title);
	},
	
	newRole: function() {
		var actor = new Actor();
		var role = new Role({"movie": this.model, "actor": actor});
		// this.model.get('cast').push(role);
		var role_view = new RoleView({"model": role});
		role_view.render();
		this.$('table.cast').append(role_view.el);
		role_view.edit();
	}
	
}, {
	finders: {
		"id": {
			"xpath": ".",
			"process": function(node) {
				return $(node).attr('id').match(/[0-9]+/)[0];
			}
		},
		"title": {
			"xpath": "details/summary[@class='title']/text()"
		},
		"year": {
			"xpath": "details/div[@class='main']/span[@class='year']/text()"
		},
		"synopsis": {
			"xpath": "details/div[@class='main']/details/summary/span/text()"
		},
		"plot": {
			"xpath": "details/div[@class='main']/details/p/text()"
		},
		"cast": {
			"xpath": "details/div[@class='main']/table/tbody/tr",
			"view": function() {return RoleView;}
		}
	}
});

var Cast = Backbone.Collection.extend({
  model: Role
});
