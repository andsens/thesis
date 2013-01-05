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
			@$el.empty().append (Mustache.render template, {})
			Chaplin.mediator.publish 'templateRendered', @$el[0]
		
		valuesChanged: (info) =>
			@$el.empty().append (Mustache.render info.template, info.data)
			Chaplin.mediator.publish 'templateRendered', @$el[0]
			
