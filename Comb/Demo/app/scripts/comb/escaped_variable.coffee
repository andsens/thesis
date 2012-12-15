define [
], () ->
	'use strict'
	
	class EscapedVariable
		
		constructor: (item, @spec, @root, @nodeStart, @strStart) ->
			@[prop] = val for prop, val of item
		
		parse: (@nodes, @nodeOffset, @strOffset) ->
			
		
		getString: ->
			# if index is >0 and type of prev is text, reduce index by one and use substring of prev
			index = @path[0]
			j = index.i + @nodeOffset
			if @prev?.type is 'text'
				j--
			if (node = @dom.childNodes[j])?
				string = node.data.substring @strOffset
				@length = @string.length
				if @prev?.type is 'text'
					string = string.substring (string.indexOf @prev.value)+@prev.value.length
					@length = string.length + @prev.value.length
				if @next?.type is 'text'
					string = string.substring 0, (string.lastIndexOf @next.value)
					@length = string.length + @next.value.length
				return string
			return ""
		
		getDom: ->
			@getString()
