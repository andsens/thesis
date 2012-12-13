define [
	'comb/variable'
	'comb/section'
], (Variable, Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec) ->
			@root = new Section null, {
					stack: []
				}, @spec
		
		get: (name, dom) ->
			query = name.split '>'
			result = @root.get query, dom
			if result?
				return result
			return []
		
		set: (name, dom) ->
			
