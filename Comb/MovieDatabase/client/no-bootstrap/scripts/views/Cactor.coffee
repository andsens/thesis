define [
	'cs!views/base/Cview'
	'text!/templates/actor.mustache'
	'text!/templates/actor.mustache-comb'
	'cs!models/actor'
], (View, template, spec, Actor) ->
	'use strict'
	
	class ActorView extends View
		
		template: template
		template = null
		
		spec: spec
		spec = null
		
		tagName: "span"
		
		initialize: ->
			super
			unless @model?
				@model = new Actor
				if @options.el?
					@model.set 'id', (@$el.attr 'id').substring 6
					@model.set 'name', @$el.text()
			
			@editable 'name', null
			@subscribeToEditableEvents "edit:movie:#{@options.movie.cid}", "save:movie:#{@options.movie.cid}"
