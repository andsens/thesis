define [
	'comb/variable'
	'comb/section'
	'underscore'
], (Variable, Section,
	_) ->
	'use strict'
	
	class Section
		
		separator: '.'
		
		constructor: (item, @spec, @childNodes, @strOffset, @nodeOffset) ->
			@[prop] = val for prop, val of item
			
			@itemsById = {}
			@itemsByName = {}
			if @path.length isnt 0
				index = @path[0]
				nodePos = index.i
				if @prev?.type is 'text' and @first?.type is 'text'
					nodePos--
			else
				nodePos = 0
			@nodeOffset += nodePos
			
			@parsed = @parse()
			
		
		parse: () ->
			parsedItems = []
			
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
			
