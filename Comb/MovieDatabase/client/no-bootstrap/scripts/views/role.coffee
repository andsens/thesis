define [
	'cs!views/base/view'
	'cs!models/role'
], (View, Role) ->
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
					@model.set 'id', (@$('td.character').attr 'id').substring 6
					@model.set 'character', @$('td.character').text()
					
					actor = new ActorView el: @$('td.actor span')
					@subview "itemView:#{actor.model.cid}", view
					@collection.push actor.model, silent: true
					
					@model.set 'actor', actor.model.id
