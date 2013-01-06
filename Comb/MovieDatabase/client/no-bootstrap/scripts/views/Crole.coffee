define [
	'cs!views/base/Cview'
	'text!/templates/role.mustache'
	'text!/templates/role.mustache-comb'
	'cs!views/Cactor'
	'cs!models/role'
	'chaplin'
], (View, template, spec, ActorView, Role, Chaplin) ->
	'use strict'
	
	class RoleView extends View
		
		template: template
		template = null
		
		spec: spec
		spec = null
		
		tagName: "tr"
		
		initialize: ->
			super
			movie = @options.movie
			unless @model?
				@model = new Role
			
			if @options.el?
				@model.set 'id', (@$el.attr 'id').substring 5
				@model.set 'character', @$('td.character').text()
				
				@subview 'actor', new ActorView {movie, el: @$ 'td.actor span'}
				
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
			@$('td.actor').append (@subview 'actor').render().el
