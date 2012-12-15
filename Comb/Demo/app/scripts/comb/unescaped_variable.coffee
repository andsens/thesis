define [
], () ->
	'use strict'
	
	class UnescapedVariable
		
		constructor: (item, @spec, @childNodes, @strOffset, @nodeOffset) ->
			throw new Error "Unsupported"
			@[prop] = val for prop, val of item
		
