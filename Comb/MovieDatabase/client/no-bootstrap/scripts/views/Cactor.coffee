define [
	'cs!views/base/Cview'
	'text!/templates/actor.mustache'
	'json!/templates/actor.mustache-comb'
	'cs!models/actor'
], (View, template, spec, Actor) ->
	'use strict'
	
	class ActorView extends View
		
		template: template
		template = null
		
		spec: spec
		spec = null
		
		tagName: "td"
		className: "actor"
		
		initialize: ->
			super
			unless @model?
				@model = new Actor
				if @data?
					@model.set 'id', @data.id.value
					@model.set 'name', @data.name.value
			
			@editable 'name'
			@subscribeToEditableEvents "edit:movie:#{@options.movie.cid}", "save:movie:#{@options.movie.cid}"
