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
			if @partials?[@name]?
				@partials[@name].verbose = @spec.verbose
				@root = new Section.Section 0, @partials[@name], @partials, @parent
		
		getRoot: ->
			if @partials?[@name]?
				{ type: 'partial', iterations: @root.getObject().iterations[0]}
			else
				{ type: 'partial', iterations: {}}
		
		getValues: (merge) ->
			if @partials?[@name]?
				values = @root.getValues([])[0]
				for name, value of values
					merge[name] = value
			return merge
	
	exports.Partial = Partial
