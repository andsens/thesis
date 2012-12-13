define [
	'views/base/view'
	'text!templates/navigation.mustache',
	
	'views/template_selection',
	
	'bootstrap-dropdown'
], (View, template,
	TemplateSelectionView) ->
	'use strict'
	
	class NavigationView extends View
		
		template: template
		
		container: "#main"
		autoRender: true
		
		className: "row-fluid"
		
		initialize: ->
			super
			@subview 'templates', new TemplateSelectionView(collection: @options.templates)
		
		afterRender: ->
			super
			@$("li.dropdown").append (@subview 'templates').$el
