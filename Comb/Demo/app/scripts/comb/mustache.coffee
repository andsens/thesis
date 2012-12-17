define [
	'underscore'
], (_) ->
	'use strict'
	
	class Mustache
		
		constructor: ->
			@initialize arguments...
			@runParse()
		
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
		
		runParse: ->
			@findParent()
			
			@verifyPrevious()
			
			if @path[1]?.i > 0
				# reset the string offset if we have skipped over a tag
				for offset in [0..@path[1].i-1] when @parent.childNodes[@nodeOffset+offset].nodeType is 1
					@strOffset = 0
					console.log {resetnode: @parent.childNodes[@nodeOffset+offset]}
					break
			
			# move the strOffset over the previous text
			if @prev.type is 'text'
				@strOffset += @prev.value.length
				console.log @strOffset
			
			@parse()
		
		parse: ->
			
		
		findParent: ->
			for part, i in @path when i isnt 0
				if part.type is 'index'
					@nodeOffset += part.i
				if i is @path.length-1
					break
				switch part.type
					when 'attribute' then @parent = @parent.attributes.getNamedItem(part.name)
					when 'index'     then @parent = @parent.childNodes[@nodeOffset]
					else                  throw new Error "Unexpected path type #{part.type}"
				@nodeOffset = 0
			
			# Always point at the previous node
			unless @prev.type in ['null']
				@nodeOffset -= 1
		
		verifyPrevious: ->
			@verifying 'previous', @prev
			
			if @prev.type is 'null'
				if @nodeOffset isnt 0
					throw new Error "Did not expect to find a previous node"
			else unless @nodeMatches @prev
				console.log @parent.childNodes
				throw new Error "The previous node did not match the expected value"
		
		verifying: (name, match, nextNode = false) ->
			offset = @nodeOffset
			offset += 1 if nextNode
			node = @parent.childNodes[offset]
			if node?.nodeType is 3
				node = (node.data.substring 0, @strOffset)+'|'+node.data.substring @strOffset
			inverted = ''
			inverted = ' inverted' if @inverted
			console.log "Verifying #{name} node of#{inverted} #{@type} '#{@name}'",
				node: node,
				nodeOffset: offset
				strOffset: @strOffset
				nextNode: nextNode
				, "to", match
		
		nodeMatches: (match, nextNode) ->
			node = @parent.childNodes[@nodeOffset]
			
			if nextNode
				if node.nodeType is 3 and node.data.length isnt @strOffset
					console.log 'string not gobbled up',
						string: (node.data.substring 0, @strOffset)+'|'+node.data.substring @strOffset
					return false
				node = @parent.childNodes[@nodeOffset+1]
			
			unless node?
				unless match.type is 'null'
					throw new Error "The node to match does not exist"
			
			result = switch match.type
				when 'section'  then throw new Error "Unsupported matching type"
				when 'escaped' then throw new Error "Unsupported matching type"
				when 'unescaped' then throw new Error "Unsupported matching type"
				when 'node', 'emptynode' then node.nodeType is 1 and node.tagName is match.name.toUpperCase()
				when 'comment' then node.nodeType is 8
				when 'text' then node.nodeType is 3 and node.data.substring(@strOffset).indexOf(match.value) is 0
				when 'null' then node?
			console.log result
			return result
