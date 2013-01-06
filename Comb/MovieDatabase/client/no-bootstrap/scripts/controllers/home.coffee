define [
	'cs!views/movies'
	'cs!collections/movies'
	'chaplin'
], (MoviesView, Movies, Chaplin) ->
	'use strict'
	
	class Home extends Chaplin.Controller
		
		initialize: ->
			view = new MoviesView
				collection: new Movies()
				el: $('#movies ol')
