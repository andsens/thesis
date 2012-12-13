define [
	'controllers/navigation'
	'routes'
	'views/layout'
	'chaplin'
], (NavigationController,
	routes, Layout, Chaplin) ->
	'use strict'


	class Client extends Chaplin.Application
		
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
		
		initMediator: ->
			Chaplin.mediator.user = null
			Chaplin.mediator.seal()
