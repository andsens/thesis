define [
	'comb/variable'
	'comb/section'
], (Variable, Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node) ->
			@root = new Section {
					id: null
					stack: []
					path: []
					prev: null
					next: null
				}, @spec, @node, 0, 0
		
		get: (name) ->
			query = name.split '>'
			@root.get query
		
		set: (name) ->
			
