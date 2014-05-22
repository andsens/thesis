define [
	'cs!controllers/home'
	'cs!routes'
	'cs!views/layout'
	'chaplin'
], (HomeController,
	routes, Layout, Chaplin) ->
	'use strict'
	
	
	class App extends Chaplin.Application
		
		title: 'MovieDatabase'
		
		initialize: ->
			super
			@initDispatcher
				controllerSuffix: ''
			@initLayout()
			@initMediator()
			@initControllers()
			@initRouter routes
			Object.freeze? this
		
		initLayout: ->
			@layout = new Layout {@title}
		
		initControllers: ->
			@home = new HomeController()
		
		initMediator: ->
			Chaplin.mediator.user = null
			Chaplin.mediator.seal()
