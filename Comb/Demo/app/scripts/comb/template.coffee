define [
	'comb/section'
], (Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node) ->
			@root = new Section @spec['root'], @spec, @node, 0, 0
		
		get: (name) ->
			# query = name.split '>'
			# @root.get query
		
		set: (name) ->
			
