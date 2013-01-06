define [
	'cs!models/movie'
	'chaplin'
], (Movie, Chaplin) ->

	class Movies extends Chaplin.Collection
		
		url: '/rest/movies'
		model: Movie
