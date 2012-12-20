define [
	'comb/mustache'
	'comb/section'
	'exports'
], (Mustache, Section, exports) ->
	'use strict'
	
	class Partial extends Mustache
		
		initialize: ->
			super
		
		parse: ->
			super
			unless @partials? and @partials[@name]?
				throw new Error ("Partial specification for " + @name + " not found")
			@partials[@name].verbose = @spec.verbose
			@root = new Section.Section 0, @partials[@name], @partials, @parent
		
		getRoot: ->
			{ type: 'partial', iterations: @root.getObject().iterations[0]}
		
		getValues: ->
			@root.getValues()[0]

		getSimple: ->
			@root.getSimple()
	
	exports.Partial = Partial
