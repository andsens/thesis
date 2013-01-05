define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class EscapedVariable extends Mustache
		
		initialize: ->
			super
		
		parse: ->
			super
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
					nextOffset = 1
			else
				@string = ""
			
			@strOffset += @string.length
			
			
			unless @nodeMatches 'next', @next, nextOffset
				throw new Error "Unable to match section end"
		
		update: (text) =>
			node = @node()
			unless node?
				unless @prev.type is 'null' and @next.type is 'null'
					throw new Error "Unable to update, node must exist to begin with"
				@parent.appendChild document.createTextNode ''
				console.log 'Creating text element, this is not a sound thing to do without an existing one'
				node = @node()
			oldStr = node.data
			before = oldStr.substring 0, @strStart
			after = oldStr.substring @strOffset
			node.data = before + text + after
			@string = text
			@strOffset = before.length + @string.length
		
		getRoot: ->
			parent = parentNode = @parent
			if parent.nodeType is 2
				parentNode = parent.ownerElement
			obj =
				type: 'escaped'
				value: @string
				update: @update
				parent: parent
				parentNode: parentNode
			return obj
		
		getValues: (merge) ->
			if merge?
				unless merge instanceof Array
					merge = [merge]
				merge.push @getRoot()
			else
				merge = @getRoot()
			return merge
