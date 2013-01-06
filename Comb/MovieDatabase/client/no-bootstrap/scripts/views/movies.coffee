define [
	'cs!views/base/collection_view'
	'cs!views/movie'
	'cs!models/movie'
], (CollectionView, MovieView, Movie) ->
	'use strict'
	
	class MoviesView extends CollectionView
		
		tagName: 'ol'
		itemView: MovieView
		
		initialize: ->
			super
			@delegate 'click', 'li>details', (e) =>
				$(_.without(@$('li>details'), e.currentTarget)).prop 'open', false
			
			for el in @$('li>details')
				view = new MovieView {el}
				@subview "itemView:#{view.model.cid}", view
				@collection.push view.model, silent: true
			
			@$('li>details')[0].open = true
