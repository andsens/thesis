define [
	'cs!views/base/$view'
	'text!/templates/actor.mustache'
	'cs!models/actor'
], (View, template, Actor) ->
	'use strict'
	
	class ActorView extends View
		
		template: template
		template = null
		
		tagName: "td"
		className: "actor"
		
		initialize: ->
			super
			unless @model?
				@model = new Actor
				if @options.el?
					@model.set 'id', (@$('span').attr 'id').substring 6
					@model.set 'name', @$('span').text()
			
			@editable 'name', 'span'
			@subscribeToEditableEvents "edit:movie:#{@options.movie.cid}", "save:movie:#{@options.movie.cid}"
