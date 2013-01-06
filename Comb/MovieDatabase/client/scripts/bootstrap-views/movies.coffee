define [
	'cs!baseviews/collection_view'
	'cs!views/$movie'
	'cs!views/Cmovie'
	'cs!models/movie'
	'bootstrap'
], (CollectionView, $MovieView, CMovieView, Movie) ->
	'use strict'
	
	MovieView = switch window.localStorage.getItem('type')
		when '$' then $MovieView
		when 'C' then CMovieView
		else throw new Error 'Set a data retrieval type with "window.localStorage.setItem(\'type\', \'$|C\')"'
	
	class MoviesView extends CollectionView
		
		className: 'container-fluid'
		listSelector: 'div.movie-list'
		itemView: MovieView
		
		initialize: ->
			super
			if @options.el
				@$list = @$ @listSelector
				for el in @$list.children()
					view = new MovieView {el}
					@subview "itemView:#{view.model.cid}", view
					@collection.push view.model, silent: true
			
			@delegate 'click', '#add-movie', =>
				@collection.push new Movie()
