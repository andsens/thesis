define [
	'underscore'
], (_) ->
	'use strict'
	
	class Mustache
		
		constructor: ->
			@initialize arguments...
			@parse()
		
		initialize: (item, @spec, @parent, @nodeOffset, @strOffset) ->
			@[prop]     = val for prop, val of item
			console.log "Construct #{@type}: '#{@name}' (#{@id})",
				"nodeOffset:", @nodeOffset, "strOffset:", @strOffset, "index: ", @path[1]?.i
			
			if @id isnt 'root'
				switch @path[0].type
					when "offset" then
					when "child"
					else throw new Error "Expected first part of path to be offset or child"
				switch @path[1].type
					when "index"
					else throw new Error "Expected second part of path to be an index"
			
			switch @first?.type
				when 'section' then throw new Error "Section as first child not yet supported"
				when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
			
			for part, i in @path when i > 0
				if part.type is 'index'
					# reset the string offset if we have skipped over a tag
					if @strOffset isnt 0
						skipRange = [0..part.i]
						if i is @path.length-1 and @prev.type in ['section', 'escaped', 'text']
							skipRange = [0..part.i-1] # Won't be negative since @prev is text
						for offset in skipRange when @parent.childNodes[@nodeOffset+offset].nodeType is 1
							console.log 'reset', {node: @parent.childNodes[@nodeOffset+offset]}
							@strOffset = 0
							break
					# If this is the last index and the previous node is a text node, we will correct this later on
					@nodeOffset += part.i
				if i is @path.length-1
					break
				switch part.type
					when 'attribute' then @parent = @parent.attributes.getNamedItem(part.name)
					when 'index'     then @parent = @parent.childNodes[@nodeOffset]
					else                  throw new Error "#{@id}: Unexpected path type #{part.type} at #{i}"
				@nodeOffset = 0
			
			# Always point at the previous node
			unless @prev.type in ['null']
				@nodeOffset -= 1
			
			if @prev.type is 'null' # We can't really be sure what @prev should be without looking at the path again
				if @nodeOffset isnt 0
					throw new Error "Did not expect to find a previous node"
			else
				@verifying 'previous', @prev
				unless @nodeMatches @prev
					console.log @parent.childNodes
					throw new Error "The previous node did not match the expected value"
			
			# move the strOffset over the previous text
			if @prev.type is 'text'
				@strOffset += @prev.value.length
		
		parse: ->
			
		
		verifying: (name, match, offset = 0) ->
			node = @parent.childNodes[@nodeOffset+offset] if @parent?
			if node?.nodeType is 3
				node = (node.data.substring 0, @strOffset)+'|'+node.data.substring @strOffset
			inverted = ''
			inverted = 'inverted ' if @inverted
			console.log "Verifying #{name} node of #{inverted}#{@type} '#{@name}'",
				node: node,
				nodeOffset: offset
				strOffset: @strOffset
				offset: offset
				, "to", match
		
		nodeMatches: (match, offset = 0) ->
			unless @parent?
				throw new Error "Cannot match node, @parent is undefined!"
			if offset > 0
				current = @parent.childNodes[@nodeOffset]
				if current.nodeType is 3 and current.data.length isnt @strOffset
					console.log 'string not gobbled up',
						string: (current.data.substring 0, @strOffset)+'|'+current.data.substring @strOffset
					return false
			if offset < 0 and @strOffset > 0
				throw new Error "Request to compare previous node, but strOffset is not 0"
			
			node = @parent.childNodes[@nodeOffset+offset]
			
			unless node?
				unless match.type is 'null'
					throw new Error "The node to match does not exist"
			
			result = switch match.type
				when 'section'  then throw new Error "Unsupported matching type"
				when 'escaped' then throw new Error "Unsupported matching type" #node.nodeType is 3
				when 'unescaped' then throw new Error "Unsupported matching type"
				when 'node', 'emptynode' then node.nodeType is 1 and node.tagName is match.name.toUpperCase()
				when 'comment' then node.nodeType is 8
				when 'text' then node.nodeType is 3 and node.data.substring(@strOffset).indexOf(match.value) is 0
				when 'null' then node?
			console.log result
			return result
