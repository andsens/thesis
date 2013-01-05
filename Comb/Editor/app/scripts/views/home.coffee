define [
	'views/base/view'
	'text!templates/home.mustache'
	
	'views/input'
	'views/viewer'
], (View, template,
	InputView, ViewerView) ->
	'use strict'
	
	class HomeView extends View
		
		template: template
		template = null
		container: "#main"
		
		className: "row-fluid"
		
		autoRender: true
		
		initialize: ->
			super
			@subview 'input', new InputView()
			@subview 'viewer', new ViewerView()
		
		afterRender: ->
			super
			@$('.view-input').append (@subview 'input').render().$el
			@$('.view-viewer').append (@subview 'viewer').render().$el
