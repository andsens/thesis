define [
	'views/home'
	'chaplin'
], (HomeView, Chaplin) ->
	'use strict'
	
	class Home extends Chaplin.Controller
		
		index: ->
			view = new HomeView()
