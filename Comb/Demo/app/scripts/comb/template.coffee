define [
	'comb/section'
], (Section) ->
	'use strict'
	
	class Template
		
		constructor: (@spec, @node) ->
			# Convert html entities, since it's a bit hard to do in haskell
			html_entities = (text) ->
				(e = document.createElement 'div').innerHTML = text
				e.childNodes[0].data
			for id, item of @spec
				for prop in ['prev', 'first', 'last', 'next'] when item[prop]?.value?.indexOf '&' >= 0
					item[prop].value = html_entities item[prop].value
			@root = new Section @spec['root'], @spec, @node, 0, 0
		
		get: (name) ->
			# query = name.split '>'
			# @root.get query
		
		set: (name) ->
			
