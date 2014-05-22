define [
	'controllers/navigation'
	'controllers/home'
	'routes'
	'views/layout'
	'chaplin'
], (NavigationController, HomeController,
	routes, Layout, Chaplin) ->
	'use strict'


	class App extends Chaplin.Application
		
		title: 'Comb Demo Application'

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
			@nav = new NavigationController()
			@home = new HomeController()
		
		initMediator: ->
			Chaplin.mediator.user = null
			Chaplin.mediator.seal()
