define [
	'comb/escaped_variable'
	'comb/unescaped_variable'
	'comb/section'
	'underscore'
], (UnescapedVariable, EscapedVariable, Section,
	_) ->
	'use strict'
	
	class Section
		
		separator: '.'
		
		constructor: (item, @spec, @root, @nodeStart, @strStart) ->
			@[prop]     = val for prop, val of item
			@parent     = @root
			@nodeOffset = @nodeStart
			@strOffset  = @strStart
			@children   = (for id, child of @spec when _.last child.stack is @id)
			@iterations = []
			
			if @id isnt 'root'
				switch @path[0].type
					when "offset", "child"
					else throw new Error "Expected first part of path to be offset or child"
				switch @path[1].type
					when "index"
					else throw new Error "Expected second part of path to be an index"
			
			for part, i in @path
				continue if i is 0
				if part.type is 'index'
					@nodeOffset += part.i
				break if i is @path.length-2
				if part.type is 'attribute'
					@parent = @parent.attributes.getNamedItem(part.name)
				else
					@parent = @parent.childNodes[part.i + @nodeOffset]
				@nodeOffset = 0
			
			@nodes = @parent.childNodes
			
			unless @nodeMatches @prev
				throw new Error "The previous node did not match the expected value"
			
			switch @prev.type
				when 'text' then @strOffset += @prev.value.length
				when 'node', 'emptynode', 'comment' then @nodeOffset += 1
			
			iterate = ->
				return false if @nodeMatches @next
				if @id isnt 'root' and not @nodeMatches @first
					throw new Error "Unable to find first node in iteration #{i}"
				return true
			
			i = 0
			while iterate()
				
				iteration = {}
				
				for child in @children when child.path[0].type is 'child'
					if child.path[0].node isnt @id then throw new Error "Unexpected path in list of children"
						if child.type is 'escaped'
							iteration[child.id] = new EscapedVariable child, @spec, @parent, @nodeOffset, @strOffset
						if child.type is 'unescaped'
							throw new Error "Unescaped variables are not yet supported"
						if child.type is 'section'
							iteration[child.id] = new Section child, @spec, @parent, @nodeOffset, @strOffset
				
				remaining = (for child in @children when child.path[0].type is 'offset')
				
				while remaining.length isnt 0
					child = remaining.shift()
					offsetNode = iteration[child.path[0].node]
					unless offsetNode?
						remaining.push child
						throw new Error "Something is wrong with the ordering of the offset children"
						continue
					nodeEnd = offsetNode.nodeEnd
					strEnd = offsetNode.strEnd
					if child.type is 'escaped'
						iteration[child.id] = new EscapedVariable child, @spec, @parent, nodeEnd, strEnd
					if child.type is 'unescaped'
						throw new Error "Unescaped variables are not yet supported"
					if child.type is 'section'
						iteration[child.id] = new Section child, @spec, @parent, nodeEnd, strEnd
				
				for id, obj of iteration when obj.parent is @parent
					
				
				iterations.push iteration
					
		
		
		nodeMatches = (match) ->
			node = @nodes[@nodeOffset]
			switch match.type
				when 'section'  then throw new Error "Unsupported matching type"
				when 'escaped_variable' then throw new Error "Unsupported matching type"
				when 'unescaped_variable' then throw new Error "Unsupported matching type"
				when 'node', 'emptynode' then return node.nodeType is 1 and node.tagName is match.name
				when 'comment' then return node.nodeType is 8
				when 'text' then return node.nodeType is 3 and node.data.substring(@strOffset).indexOf(match.value) is 0
				when 'null' then return node?
		
		
			# @variables = []
			# @sections = []
			# for id, child of @spec
			# 	continue unless _.last child.stack is @id
			# 	if child.type is 'variable'
			# 		if child.escaped
			# 			@variables.push new EscapedVariable item
			# 		else
			# 			@variables.push new UnescapedVariable item
			# 	else
			# 		@sections.push new Section item, @spec
		
		
		
		
		
		
		
		
		parse: () ->
			
			
			
			
			path_matches = (path, offsetId, itemPath) ->
				return false unless path.length+2 is itemPath.length
				for part, i in path
					itemPart = itemPath[itemPath.length-i-2]
					return false unless itemPart?
					return false unless itemPart.type is 'index'
					return false unless part is itemPart.i
				itemPart = _.last itemPath
				return false unless itemPart?
				if itemPart.type is 'root'
					return true if offsetId is null
				if itemPart.type is 'offset'
					return true if offsetId is itemPart.node
				return false
			
			var_matches = (item, nodes, strOffset, nodeOffset) ->
				node = nodes.childNodes[nodeOffset]
				unless node?
					return false
				if item.prev?.type is 'text'
					prev = node.data.substring strOffset, item.prev.value.length
					return prev is item.prev.value
				throw new Error "Unsupported"
			
			section_matches = (item, nodes, strOffset, nodeOffset) ->
				node = nodes.childNodes[nodeOffset]
				unless node?
					return false
				if item.prev?.type is 'text'
					prev = node.data.substring strOffset, item.prev.value.length
					return prev is item.prev.value
				throw new Error "Unsupported"
			
			recurse = (nodePos, nodeOffset, strOffset, offsetId, path, nodes, children) =>
				child = _.find children, (child) => path_matches [], offsetId, child.path
				while child?
					if child.type is 'variable'
						if not var_matches child, nodes, strOffset, nodeOffset
							break
						resolution = new Variable child, @spec, nodes, strOffset, nodeOffset
						strOffset = resolution.length
					else
						if not section_matches child, strOffset, nodeOffset
							break
						resolution = new Section child, @spec, nodes, strOffset, nodeOffset
						strOffset = resolution.trailingText.length
						nodeOffset = resolution.length
					parsedItems.push resolution
					child = _.find children, (child) => path_matches [], resolution.id, child.path
				
				for node, i in nodes
					path.push pos
					child = _.find children, (child) -> path_matches path, offsetId, child.path
					if child?
						if child.type is 'variable'
							if not var_matches child, nodes, strOffset, nodeOffset
								break
							resolution = new Variable child, @spec, node.childNodes
						else
							if not section_matches child, strOffset, nodeOffset
								break
							resolution = new Section child, @spec, node.childNodes
						parsedItems.push resolution
						recurse 0, resolution.length, [], resolution.id, node.childNodes, children
					else
						recurse 0, 0, offsetId, path, node.childNodes, children
					path.pop()
					pos++
					if path.length is 0 and pos is @content_length
						console.log @name, "stopped after", pos+posOffset, " with nodes: ", nodes.length, node, "  curlen is ", @length
						return pos + posOffset
				return nodePos + nodeOffset
			
			unless @childNodes[@nodeOffset]?
				console.log @name, 'no dice'
				return []
			if @first?.type is 'text'
				strOffset = @strOffset
				if @prev?.type is 'text'
					strOffset += @prev.value.length
				first = @childNodes[@nodeOffset].data.substring strOffset
				if first isnt @first.value
					return []
			
			children = []
			for id, child of @spec
				if @id is null
					continue if child.stack.length > 0
				else
					continue unless child.stack[0] is @id
				children.push child
			
			@leadingText = ""
			if @prev?.type is 'text'
				@leadingText += @prev.value
			if @first?.type is 'text'
				@leadingText += @first.value
			@trailingText = ""
			if @next?.type is 'text'
				@trailingText += @next.value
			if @last?.type is 'text'
				@trailingText += @last.value
			
			@length = recurse nodePos, @nodeOffset, @strOffset+@leadingText.length, [], @id, @childNodes, children
			console.log parsedItems
			return parsedItems
		
		get: (query) ->
			
