define [
	'comb/section'
], (Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node, @verbose = false) ->
			@spec.verbose = @verbose
			# Convert html entities, since it's a bit hard to do in haskell
			@root = new Section 0, @spec, @node, 0, 0
		
		getValues: ->
			@root.getObject()
		
		getSimpleValues: ->
			@root.getSimple()
