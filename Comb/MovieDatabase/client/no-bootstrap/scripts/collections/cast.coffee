define [
	'cs!models/role'
	'chaplin'
], (Role, Chaplin) ->

	class Cast extends Chaplin.Collection
		
		url: '/rest/cast'
		model: Role
