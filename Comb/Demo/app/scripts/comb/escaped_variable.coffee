define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class EscapedVariable extends Mustache
		
		initialize: ->
			super
		
		parse: ->
			super
			node = @node()
			switch @prev.type
				when 'null', 'section', 'escaped', 'text' then
				when 'unescaped'
					throw new Error "Unsupported matching type"
				when 'node', 'emptynode', 'comment'
					@nodeOffset += 1
					node = @node()
			
			nextOffset = 0
			if node?
				@string = node.data.substring @strOffset
				
				if @next.type is 'text'
					nextIndex = @string.indexOf @next.value
					if nextIndex is -1
						throw new Error "Unable to find next text"
					@string = @string.substring 0, nextIndex
				
				if @next.type in ['node', 'emptynode', 'comment', 'null']
					nextOffset += 1
			else
				@string = ""
			
			@strOffset += @string.length
			
			
			@verifying 'next', @next, nextOffset
			unless @nodeMatches @next, nextOffset
				throw new Error "Unable to match section end"
			console.log "Result:", "'#{@string}'"
