define [
	'views/base/view'
	'text!templates/viewer.mustache'
	
	'mustache'
	'chaplin'
], (View, template,
	Mustache, Chaplin) ->
	'use strict'
	
	class ViewerView extends View
		
		template: template
		template = null
		container: "body"
		
		className: "row-fluid"
		
		initialize: ->
			super
			Chaplin.mediator.subscribe 'templateLoaded', @renderTemplate
			Chaplin.mediator.subscribe 'valuesChanged', @valuesChanged
		
		renderTemplate: (template) =>
			@$('#rendered').empty().append (Mustache.render template, {})
			@$('#template').empty().text template.replace(/\t/g, "  ")
			Chaplin.mediator.publish 'templateRendered', @$('#rendered')[0]
		
		valuesChanged: (info) =>
			@$('#rendered').empty().append (Mustache.render info.template, info.data)
			Chaplin.mediator.publish 'templateRendered', @$('#rendered')[0]
			
