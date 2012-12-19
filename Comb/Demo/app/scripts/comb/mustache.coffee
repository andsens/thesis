define [
	'underscore'
], (_) ->
	'use strict'
	
	class Mustache
		
		constructor: ->
			@initialize arguments...
			@parse()
		
		initialize: (@id, @spec, @rootNode, @nodeOffset, @strOffset) ->
			@[prop]     = val for prop, val of @spec[@id]
			if @spec.verbose
				console.log "Construct #{@type}: '#{@name}' (#{@id})",
					"nodeOffset:", @nodeOffset, "strOffset:", @strOffset
			
			unless @id is 0
				switch @prev.type
					when 'section' then throw new Error "Section as first child not yet supported"
					when 'escaped' then throw new Error "Escaped as first child not yet supported"
					when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
				switch @next.type
					when 'section' then throw new Error "Section as first child not yet supported"
					when 'escaped' then throw new Error "Escaped as first child not yet supported"
					when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
			
			@parent = @rootNode
			
			for index, i in @path when i > 0
				if typeof index is 'number'
					# reset the string offset if we have skipped over a tag
					if @strOffset isnt 0
						skipRange = [0..index]
						if i is @path.length-1 and @prev.type in ['section', 'escaped', 'text']
							skipRange = [0..index-1] # Won't be negative since @prev is text
						for offset in skipRange when @node(offset).nodeType in [1, 8]
							@strOffset = 0
							break
					# If this is the last index and the previous node is a text node, we will correct this later on
					@nodeOffset += index
				if i is @path.length-1
					break
				switch typeof index
					when 'number' then @parent = @node()
					when 'string' then @parent = @parent.attributes.getNamedItem index
				@nodeOffset = 0
			
			# Always point at the previous node
			unless @prev.type in ['null']
				@nodeOffset -= 1
			
			if @prev.type is 'null' # We can't really be sure what @prev should be without looking at the path again
				if @nodeOffset isnt 0
					throw new Error "Did not expect to find a previous node"
			else
				unless @nodeMatches 'prev',@prev
					throw new Error "The previous node did not match the expected value"
			
			# move the strOffset over the previous text
			if @prev.type is 'text'
				@strOffset += @prev.value.length
			@strStart = @strOffset
			
		parse: ->
			
		
		nodeMatches: (name, match, offset = 0) ->
			result = @matchNode match, offset
			if @spec.verbose
				node = @node(offset)
				if node?.nodeType in [3, 8]
					node = (node.data.substring 0, @strOffset)+'|'+node.data.substring @strOffset
				inverted = ''
				inverted = 'inverted ' if @inverted
				matched = 'Rejected'
				matched = 'Matched' if result
				console.log "#{matched} #{name} node of #{inverted}#{@type} '#{@name}'",
					node: node,
					nodeOffset: offset
					strOffset: @strOffset
					offset: offset
					, "to", matched
			return result
		
		matchNode: (match, offset = 0) ->
			unless @parent?
				throw new Error "Cannot match node, @parent is undefined!"
			if offset isnt 0
				current = @node()
				unless current?
					throw new Error "Tried to get offset #{offset} when current node is undefined already"
				if offset > 0 and current.nodeType is 3 and current.data.length isnt @strOffset
					return false
				if offset < 0 and @strOffset > 0
					throw new Error "Request to compare previous node, but strOffset is not 0"
			
			node = @node(offset)
			
			unless node?
				unless match.type is 'null'
					return false
			
			return switch match.type
				when 'section' then throw new Error "Unsupported matching type"
				when 'escaped' then throw new Error "Unsupported matching type"
				when 'unescaped' then throw new Error "Unsupported matching type"
				when 'node', 'emptynode' then node.nodeType is 1 and node.tagName is match.name.toUpperCase()
				when 'comment' then node.nodeType is 8
				when 'text' then node.nodeType in [3, 8] and node.data.substring(@strOffset).indexOf(match.value) is 0
				when 'null' then not node?
		
		node: (offset = 0) ->
			offset += @nodeOffset
			return switch @parent.nodeType
				when 1, 2
					@parent.childNodes[offset]
				when 3
					throw new Error "Attempted to childNodes from text node"
				when 8
					if offset > 0
						throw new Error "Attempted to get offset > 0 from comment node"
					# We act like a comment node has childNodes, this assumes only .data is accessed on it
					@parent
			
		
