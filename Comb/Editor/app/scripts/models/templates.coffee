define [
	'chaplin',
	'models/template'
], (Chaplin, template) ->
	class Templates extends Chaplin.Collection
		
		model: template
