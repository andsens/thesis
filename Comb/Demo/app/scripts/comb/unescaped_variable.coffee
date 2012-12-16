define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class UnescapedVariable extends Mustache
		
		initialize: ->
			throw new Error "Unsupported"
