define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class EscapedVariable extends Mustache
		
		initialize: ->
			super
		
		parse: ->
			super
			node = @parent.childNodes[@nodeOffset]
			if not node?
				if @prev.type is 'text' or @next.type is 'text'
					throw new Error "Expected a node but found none"
				@string = ""
			else
				switch @prev.type
					when 'null', 'section', 'escaped' then
					when 'unescaped'
						throw new Error "Unsupported matching type"
					when 'node', 'emptynode', 'comment'
						@nodeOffset += 1
						node = @parent.childNodes[@nodeOffset]
					when 'text'
						prevIndex = node.data.indexOf @prev.value
						if prevIndex is -1
							throw new Error "Unable to find previous text"
						@strOffset += @prev.value.length
				
				@string = node.data.substring @strOffset
				
				if @next?.type is 'text'
					nextIndex = @string.indexOf @next.value
					if nextIndex is -1
						throw new Error "Unable to find next text"
					@string = @string.substring 0, nextIndex
			
			@strOffset += @string.length
			@verifying 'next', @next
			unless @nodeMatches @next
				throw new Error "Unable to match section end"
			console.log "Result:", "'#{@string}'"
