define [
	'cs!baseviews/collection_view'
	'cs!views/$movie'
	'cs!views/Cmovie'
	'cs!models/movie'
], (CollectionView, $MovieView, CMovieView, Movie) ->
	'use strict'
	
	MovieView = switch window.localStorage.getItem('type')
		when '$' then $MovieView
		else CMovieView
	
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
			
			@delegate 'click', 'button.comb', =>
				switch window.localStorage.getItem('type')
					when 'C' then window.localStorage.setItem('type', '$')
					else window.localStorage.setItem('type', 'C')
				window.location.reload()
			
			switch window.localStorage.getItem('type')
				when 'C' then @$('button.comb').text('Off')
				else @$('button.comb').text('On')
