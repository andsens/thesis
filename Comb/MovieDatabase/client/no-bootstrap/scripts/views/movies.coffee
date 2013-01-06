define [
	'cs!views/base/collection_view'
	'cs!views/movie'
	'cs!models/movie'
], (CollectionView, MovieView, Movie) ->
	'use strict'
	
	class MoviesView extends CollectionView
		
		tagName: 'section'
		listSelector: 'ol'
		itemView: MovieView
		
		initialize: ->
			super
			
			if @options.el
				for el in @$('ol>li')
					view = new MovieView {el}
					@subview "itemView:#{view.model.cid}", view
					@collection.push view.model, silent: true
				@$list = @$ @listSelector
				@$('ol>li>details')[0].open = true
			
			
			@delegate 'click', 'ol>li>details', (e) =>
				$(_.without(@$('ol>li>details'), e.currentTarget)).prop 'open', false
			
			@delegate 'click', '#add_movie_button', =>
				@collection.push new Movie()
