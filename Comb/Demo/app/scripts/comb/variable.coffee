define [
], () ->
	'use strict'
	
	class Variable
		
		constructor: (@id, item) ->
			@[prop] = val for prop, val of item
		
		get: (query, dom) ->
			if @escaped
				return @getString(dom)
		
		getString: (dom) ->
			console.log dom.outerHTML
			# if index is >0 and type of prev is text, reduce index by one and use leftChild
			index = @path[0]
			j = index.i
			if @prev?.type is 'text'
				j--
			dom.childNodes[j]?.data
			
