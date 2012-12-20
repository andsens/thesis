define [
	'comb/section'
], (Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node, @partials = {}, verbose = false) ->
			@spec.verbose = verbose
			@root = new Section 0, @spec, @partials, @node, 0, 0
		
		getRoot: ->
			@root.getRoot()
		
		getValues: ->
			@root.getValues()[0]
		
		getSimple: ->
			@root.getSimple()
