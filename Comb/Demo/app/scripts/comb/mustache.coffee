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
			console.log "Construct #{@type}: '#{@name}' (#{@id}) nodeOffset:", @nodeOffset, "strOffset:", @strOffset
			
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
			@parse()
			@verifyNext()
		
		parse: ->
			
		
		findParent: ->
			for part, i in @path when i isnt 0
				if part.type is 'index'
					if i is 1 and part.i > 0
						# reset the string offset if we have skipped over a tag, that does not include the previous node
						for offset in [0..part.i] when @parent.childNodes[@nodeOffset+offset].nodeType is 1
							@strOffset = 0
							break
					@nodeOffset += part.i
				if i is @path.length-1
					break
				switch part.type
					when 'attribute' then @parent = @parent.attributes.getNamedItem(part.name)
					when 'index'     then @parent = @parent.childNodes[@nodeOffset]
					else                  throw new Error "Unexpected path type #{part.type}"
				@nodeOffset = 0
			
			# if @prev.type is 'text'
			# 	@nodeOffset -= 1
			# 	if @nodeOffset < 0
			# 		throw new Error "Previous correction got the nodeOffset down to #{@nodeOffset}"
			# else if @path[0].type is 'offset'
			# 	console.log 'dec'
			# 	# We want to point at the previous node and not the element itself
			# 	@nodeOffset -= 1
			# 	if @nodeOffset < 0
			# 		throw new Error "Offset correction got the nodeOffset down to #{@nodeOffset}"
				
				
		verifyPrevious: ->
			@verifying 'previous', @prev
			
			if @prev.type is 'null'
				if @nodeOffset isnt 0
					throw new Error "Did not expect to find a previous node"
			else unless @nodeMatches @prev
				console.log @parent.childNodes
				throw new Error "The previous node did not match the expected value"
		
		verifying: (name, match) ->
			node = @parent.childNodes[@nodeOffset]
			if node?.nodeType is 3
				node = node.data
			console.log "Verifying #{name}",
				name: @name,
				node: node,
				nodeOffset: @nodeOffset
				strOffset: @strOffset
				, "to", match
		
		verifyNext: ->
			@verifying 'next', @next
			unless @nodeMatches @next
				throw new Error "Unable to match section end"
		
		nodeMatches: (match) ->
			node = @parent.childNodes[@nodeOffset]
			return switch match.type
				when 'section'  then throw new Error "Unsupported matching type"
				when 'escaped' then throw new Error "Unsupported matching type"
				when 'unescaped' then throw new Error "Unsupported matching type"
				when 'node', 'emptynode' then node.nodeType is 1 and node.tagName is match.name.toUpperCase()
				when 'comment' then node.nodeType is 8
				when 'text' then node.nodeType is 3 and node.data.substring(@strOffset).indexOf(match.value) is 0
				when 'null' then node?
