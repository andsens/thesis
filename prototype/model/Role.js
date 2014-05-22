var Role = Backbone.Model.extend({
	urlRoot : '/roles',
	defaults: {
		"character": "Character"
	}
},{
	getAttrType: function(attr_name) {
		switch(attr_name) {
			case 'id':
				return 'node';
			case 'character':
				return 'string';
			case 'actor':
			case 'movie':
				return 'model';
		}
	},
	getComplexType: function(attr_name) {
		switch(attr_name) {
			case 'actor':
				return Actor;
			case 'movie':
				return Movie;
		}
	},mname: 'role'
});

var RoleView = Backbone.View.extend({
	
	tagName: "tr",
	events: {
		"keydown td.character": "end_edit"
	},
	
	edit: function() {
		this.$('td.actor span, td.character').attr('contenteditable', true);
		this.$('td.actor span, td.character').css('opacity', 0.5);
	},
	
	end_edit: function(e) {
		if(e.keyCode == 13) {
			e.preventDefault();
			this.model.set('character', this.$('td.character').text());
			this.model.get('actor').save({'name': this.$('td.actor span').text()}, {
				success: _.bind(function() {
					this.model.save({movie_id: this.model.get('movie').id, actor_id: this.model.get('actor').id}, {
						success: _.bind(function() {
							this.$('td.character').blur();
							this.$('td.actor span, td.character').attr('contenteditable', false);
							this.$('td.actor span, td.character').animate({opacity: 1}, 500);
						}, this)
					});
				}, this)
			});
		}
	},
	
	template: _.template(
		'<td class="actor"></td>'+
		'<td class="character"><%= character %></td>'),
	render: function() {
		$(this.el).html(this.template(this.model.toJSON()));
		var view = new ActorView({model: this.model.get('actor')});
		$(this.el).find('td.actor').append(view.render().el);
		return this;
	}
}, {
	finders: {
		"id": {
			"xpath": ".",
			"process": function(node) {
				return $(node).attr('id').match(/[0-9]+/)[0];
			}
		},
		"character": {
			"xpath": 'td[@class="character"]/text()'
		},
		"actor": {
			"xpath": 'td[@class="actor"]',
			"view": function() {return ActorView;}
		}
	}
});
