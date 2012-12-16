define [
	'underscore'
], (_) ->
	'use strict'
	
	class Mustache
		
		constructor: (item, @spec, @root, @nodeStart, @strStart) ->
			@[prop]     = val for prop, val of item
			console.log "Construct:", @type, @name, "parent:", @root, "nodeOffset:", @nodeStart, "strOffset:", @strStart
			
			if @id isnt 'root'
				switch @path[0].type
					when "offset", "child"
					else throw new Error "Expected first part of path to be offset or child"
				switch @path[1].type
					when "index"
					else throw new Error "Expected second part of path to be an index"
			
			@initialize()
		
		initialize: ->
			@parent     = @root
			@nodeOffset = @nodeStart
			@strOffset  = @strStart
			@size       = 0
			@parse()
		
		parse: ->
			@findParent()
			@verifyPrevious()
		
		findParent: ->
			if @path[0].type is 'offset'
				throw new Error "offset!"
			
			for part, i in @path
				continue if i is 0
				if part.type is 'index'
					# reset the string offset if we are skipping over a tag
					if i is 1
						for offset in [@nodeOffset..part.i] when @parent.childNodes[offset].nodeType is 1
							@strOffset = 0
							break
					@nodeOffset += part.i
				break if i is @path.length-1
				if part.type is 'attribute'
					@parent = @parent.attributes.getNamedItem(part.name)
				else
					@parent = @parent.childNodes[@nodeOffset]
				@strOffset = 0
				@nodeOffset = 0
			
			if @prev.type is 'text'
				@nodeOffset = Math.max(@nodeOffset-1, 0)
		
		verifyPrevious: ->
			if @prev.type is 'null'
				if @nodeOffset isnt 0
					throw new Error "The previous node did not match the expected value"
			else unless @nodeMatches @prev
				throw new Error "The previous node did not match the expected value"
		
		
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
