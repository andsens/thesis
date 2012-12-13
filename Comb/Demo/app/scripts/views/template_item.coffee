define [
	'views/base/view'
	'text!templates/template_item.mustache'
	
	'require'
	'chaplin'
], (View, template,
	requirejs, Chaplin) ->
	'use strict'
	
	class TemplateItemView extends View
		
		template: template
		template = null
		
		tagName: "li"
		
		initialize: ->
			super
			if (@model.get 'name') == 'profile'
				window.setTimeout @load, 500
			@delegate 'click', 'a.template-item', (e) =>
				@load()
				e.preventDefault()
				false
		
		load: =>
			require [
				"text!" + (@model.get 'template')
				"json!" + (@model.get 'comb')
			], (template, comb) =>
				Chaplin.mediator.publish 'loadTemplate',
					template: template
					comb: comb
					name: @model.get 'name'
