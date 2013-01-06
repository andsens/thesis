define [
	'cs!views/base/view'
	'cs!views/actor'
	'cs!models/role'
], (View, ActorView, Role) ->
	'use strict'
	
	class RoleView extends View
		
		# template: template
		# template = null
		
		tagName: "tr"
		
		initialize: ->
			super
			unless @model?
				@model = new Role
				if @options.el?
					@model.set 'id', (@$('td.character').attr 'id').substring 5
					@model.set 'character', @$('td.character').text()
					
					actorView = new ActorView
						el: @$('td.actor span')
						movie_id: @options.movie_id
					@subview "actor", actorView
					
					@model.set 'actor_id', actorView.model.id
					@model.set 'movie_id', @options.movie_id
			
			@editable 'character', 'td.character'
			
			@subscribeToEditableEvents "edit:movie:#{@options.movie_id}", "save:movie:#{@options.movie_id}"
