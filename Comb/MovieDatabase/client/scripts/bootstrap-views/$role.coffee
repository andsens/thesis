define [
	'cs!baseviews/$view'
	'text!templates/role.mustache'
	'cs!views/$actor'
	'cs!models/role'
	'chaplin'
], (View, template, ActorView, Role, Chaplin) ->
	'use strict'
	
	class RoleView extends View
		
		template: template
		template = null
		
		tagName: "tr"
		
		initialize: ->
			super
			movie = @options.movie
			unless @model?
				@model = new Role
			
			if @options.el?
				@model.set 'id', (@$el.attr 'id').substring 5
				@model.set 'character', @$('td.character').text()
				
				@subview 'actor', new ActorView {movie, el: @$ 'td.actor'}
				
				@model.set 'actor_id', (@subview 'actor').model.id
				@model.set 'movie_id', movie.id
			else
				@subview 'actor', new ActorView {movie}
			
			@editable 'character', 'td.character'
			
			Chaplin.mediator.subscribe "edit:movie:#{movie.cid}", =>
				@setEditable true
			
			save = =>
				if movie.isNew()
					movie.once 'sync', save
					return
				actor = (@subview 'actor').model
				if actor.isNew()
					actor.once 'sync', save
					return
				@model.set 'movie_id', movie.id
				@model.set 'actor_id', actor.id
				@model.save()
			
			Chaplin.mediator.subscribe "save:movie:#{movie.cid}", =>
				@setEditable false
				if @saveRequired
					save()
					@saveRequired = false
		
		afterRender: ->
			@$el.prepend (@subview 'actor').render().el
