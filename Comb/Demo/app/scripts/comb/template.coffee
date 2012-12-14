define [
	'comb/variable'
	'comb/section'
], (Variable, Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @dom) ->
			@root = new Section {
					id: null
					stack: []
					path: []
					content_length: @dom.childNodes.length
				}, @spec, @dom.childNodes, 0
		
		get: (name) ->
			query = name.split '>'
			@root.get query
		
		set: (name) ->
			
