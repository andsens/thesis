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
			@comb = new CombTpl info.comb
		
		templateLoaded: (doc) =>
			console.log (@comb.get 'name', doc)
