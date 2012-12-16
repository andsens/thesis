define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class EscapedVariable extends Mustache
		
		parse: ->
			super
			
			node = @parent.childNodes[@nodeOffset]
			
			if not node?
				if @prev.type is 'text' or @next.type is 'text'
					throw new Error "Expected a node but found none"
				@string = ""
			else
				@string = node.data.substring @strOffset
				if @prev?.type is 'text'
					prevIndex = @string.indexOf @prev.value
					if prevIndex is -1
						throw new Error "Unable to find previous text"
					@string = @string.substring prevIndex + @prev.value.length
				if @next?.type is 'text'
					nextIndex = @string.indexOf @next.value
					if nextIndex is -1
						throw new Error "Unable to find next text"
					@string = @string.substring 0, nextIndex
			
			
			@outerLength = @string.length
			if @prev.type is 'text'
				@outerLength += @prev.value.length
			if @next.type is 'text'
				@outerLength += @next.value.length
			
			console.log "Result:", "'#{@string}'"
