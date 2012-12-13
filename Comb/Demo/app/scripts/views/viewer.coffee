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
			Chaplin.mediator.subscribe 'loadTemplate', @renderTemplate
		
		renderTemplate: (info) =>
			@$el.empty().append (Mustache.render info.template, {})
			Chaplin.mediator.publish 'templateLoaded', @$el[0]
			
