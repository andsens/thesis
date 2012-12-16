define [
	'underscore'
], (_) ->
	'use strict'
	
	class Mustache
		
		constructor: ->
			@initialize arguments...
			@parse()
		
		initialize: (item, @spec, @root, @nodeStart, @strStart) ->
			@[prop]     = val for prop, val of item
			console.log "Construct #{@type}: '#{@name}' (#{@id}) parent:", @root, "nodeOffset:", @nodeStart, "strOffset:", @strStart
			
			if @id isnt 'root'
				switch @path[0].type
					when "offset", "child"
					else throw new Error "Expected first part of path to be offset or child"
				switch @path[1].type
					when "index"
					else throw new Error "Expected second part of path to be an index"
			
			switch @first?.type
				when 'section' then throw new Error "Section as first child not yet supported"
				when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
			
			@parent     = @root
			@nodeOffset = @nodeStart
			@strOffset  = @strStart
		
		parse: ->
			@findParent()
			@verifyPrevious()
		
		findParent: ->
			for part, i in @path when i isnt 0
				if part.type is 'index'
					@nodeOffset += part.i
					if i is 1
						# reset the string offset if we have skipped over a tag, that does not include the previous node
						for offset in [@nodeStart..@nodeOffset-1] when @parent.childNodes[offset].nodeType is 1
							console.log @parent.childNodes[offset]
							@strOffset = 0
							console.log 'reset'
							break
				break if i is @path.length-1
				if part.type is 'attribute'
					@parent = @parent.attributes.getNamedItem(part.name)
				else
					@parent = @parent.childNodes[@nodeOffset]
				@nodeOffset = 0
			
			if @prev.type is 'text'
				@nodeOffset = Math.max(@nodeOffset-1, 0)
		
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
