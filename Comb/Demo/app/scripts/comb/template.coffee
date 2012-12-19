define [
	'comb/section'
], (Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node) ->
			# Convert html entities, since it's a bit hard to do in haskell
			@root = new Section 0, @spec, @node, 0, 0
		
		get: (name) ->
			# query = name.split '>'
			# @root.get query
		
		set: (name) ->
			
