define [
	'cs!views/base/collection_view'
	'cs!views/$movie'
	'cs!views/Cmovie'
	'cs!models/movie'
], (CollectionView, $MovieView, CMovieView, Movie) ->
	'use strict'
	
	MovieView = switch window.localStorage.getItem('type')
		when '$' then $MovieView
		when 'C' then CMovieView
		else throw new Error 'Set a data retrieval type with "window.localStorage.setItem(\'type\', \'$|C\')"'
	
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
			
			
			@delegate 'click', 'ol>li>details', (e) =>
				$(_.without(@$('ol>li>details'), e.currentTarget)).prop 'open', false
			
			@delegate 'click', '#add_movie_button', =>
				@collection.push new Movie()
