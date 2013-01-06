define [
	'cs!views/base/view'
	'cs!models/actor'
], (View, Actor) ->
	'use strict'
	
	class ActorView extends View
		
		# template: template
		# template = null
		
		tagName: "tr"
		
		initialize: ->
			super
			unless @model?
				@model = new Actor
				if @options.el?
					@model.set 'id', (@$el.attr 'id').substring 6
					@model.set 'character', @$el.text()
					
