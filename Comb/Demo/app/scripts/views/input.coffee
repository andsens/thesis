define [
	'views/base/view'
	'text!templates/input.mustache'
	
	'comb/template'
	'chaplin'
], (View, template,
	CombTpl, Chaplin) ->
	'use strict'
	
	class InputView extends View
		
		template: template
		template = null
		
		tagName: "form"
		
		initialize: ->
			super
			Chaplin.mediator.subscribe 'loadTemplate', @renderForm
			Chaplin.mediator.subscribe 'templateLoaded', @templateLoaded
		
		renderForm: (info) =>
			@spec = info.comb
		
		templateLoaded: (node) =>
			@comb = new CombTpl @spec, node
			# console.log 'get name', (@comb.get 'name', doc)
			# console.log 'get description', (@comb.get 'description', dom)
			# console.log 'get expert skills', (@comb.get 'expert_skills', dom)
			console.log 'get expert skills', (@comb.get 'expert_skills>name')
			Chaplin.mediator.publish 'templateParsed'
			
