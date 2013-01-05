define [
	'views/navigation'
	'chaplin'
	'models/templates'
	'template-list'
], (NavigationView, Chaplin, Templates, templateList) ->
	'use strict'
	
	class NavigationController extends Chaplin.Controller
		
		initialize: ->
			templates = new Templates templateList
			@navigation = new NavigationView(templates: templates)
