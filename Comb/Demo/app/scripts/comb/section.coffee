define [
	'comb/mustache'
	
	'comb/escaped_variable'
	'comb/unescaped_variable'
	
	'underscore'
], (Mustache,
	EscapedVariable, UnescapedVariable,
	_) ->
	'use strict'
	
	class Section extends Mustache
		
		initialize: ->
			@children   = (child for id, child of @spec when (_.last child.stack) is @id)
			@iterations = []
			super
		
		parse: ->
			super
			
			# move the offset up to the first node
			switch @prev.type
				when 'text'
					@strOffset += @prev.value.length
					switch @first.type
						when 'section' then throw new Error "Section as first child not yet supported"
						when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
						when 'text', 'escaped' then # The first node is part of the previous node
						else @nodeOffset += 1
				when 'node', 'emptynode', 'comment'
					@strOffset = 0
					@nodeOffset += 1
			
			iterate = =>
				if @id is 'root'
					return false unless @parent.childNodes[@nodeOffset+1]?
					return true
				return true if @nodeMatches @first
				return false if @nodeMatches @next
				console.log @first, @nodeOffset, @parent.childNodes[@nodeOffset]
				throw new Error "Unable to find first node in iteration #{i}"
			
			i = 0
			while iterate()
				unless @id is 'root'
					# The mergepoint between two iterations is counted only once
					if i > 0 and @first.type is 'text' and @last.type is 'text'
						@nodeOffset--
						@size--
					
				
				iteration = {}
				
				# Parse the children that are not offset by preceeding items
				for child in @children when child.path[0].type is 'child'
					if child.path[0].node isnt @id
						throw new Error {msg: "Unexpected path in list of children", path: child.path[0]}
					switch child.type
						when 'section'
							obj = new Section child, @spec, @parent, @nodeOffset, @strOffset
						when 'escaped'
							obj = new EscapedVariable child, @spec, @parent, @nodeOffset, @strOffset
						when 'unescaped'
							throw new Error "Unescaped variables are not yet supported"
					iteration[child.id] = obj
				
				remaining = (child for child in @children when child.path[0].type is 'offset')
				
				# Parse the children that are offset by preceeding items
				while remaining.length isnt 0
					child = remaining.shift()
					offsetNode = iteration[child.path[0].node]
					unless offsetNode?
						throw new Error "Something is wrong with the ordering of the offset children"
						remaining.push child
						continue
					endNode = offsetNode.endNode
					strEnd = offsetNode.strEnd
					switch child.type
						when 'section'
							iteration[child.id] = new Section child, @spec, @parent, endNode, strEnd
						when 'escaped'
							iteration[child.id] = new EscapedVariable child, @spec, @parent, endNode, strEnd
						when 'unescaped'
							throw new Error "Unescaped variables are not yet supported"
				
				unless @id is 'root'
					# Check how much the children have affected the length of this section
					for content, j in @contents
						switch content
							when '#node', '#emptynode', "#comment"
								@nodeOffset += 1
								@strOffset = 0
							when "#text"
								if j is 0 and i > 0
									@nodeOffset += 1
									break
								if j is content.length - 1
									if @next.type isnt 'text'
										@nodeOffset += 1
										break
									@strOffset += @last.value.length
							else
								offsetNode = iteration[content]
								switch offsetNode.type
									when 'section'
										@nodeOffset += offsetNode.size
										@strOffset += offsetNode.lastStr.length
									when 'escaped'
										@strOffset += offsetNode.outerLength
									when 'unescaped'
										throw new Error "Unescaped variables are not yet supported"
				
				@iterations.push iteration
				i++
			console.log @iterations
		
