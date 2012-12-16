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
			super
			@children   = (child for id, child of @spec when (_.last child.stack) is @id)
			@iterations = []
		
		parse: ->
			super
			
			# move the offset up to the first node
			# switch @prev.type
			# 	when 'text'
			# 		unless @first.type is 'text' or @first.type is 'escaped'
			# 			@nodeOffset += 1
			# 			console.log 'inc'
			# 		@strOffset += @prev.value.length
			# 	when 'node', 'emptynode', 'comment'
			# 		@nodeOffset += 1
			# 		@strOffset = 0
			
			firstMatches = =>
				if @id is 'root'
					return @parent.childNodes[@nodeOffset+1]?
				@verifying 'first', @first
				if @nodeMatches @first
					return true
				
				if @next.type isnt 'text' then @nodeOffset += 1
				unless @nodeMatches @next
					console.log 'firstMatches',
						next: @next
						first: @first
						nodeOffset: @nodeOffset
						strOffset: @strOffset
						node: @parent.childNodes[@nodeOffset]
					throw new Error "Unable to find first node"
			
			while firstMatches()
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
				
				# Parse the children that are offset by preceeding items
				for child in @children when child.path[0].type is 'offset'
					offsetNode = iteration[child.path[0].node]
					unless offsetNode?
						throw new Error "Something is wrong with the ordering of the offset children"
					switch child.type
						when 'section'
							obj = new Section child, @spec, @parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'escaped'
							obj = new EscapedVariable child, @spec, @parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'unescaped'
							throw new Error "Unescaped variables are not yet supported"
					iteration[child.id] = obj
				
				# Check how much the children have affected the length of this section
				for content, j in @contents
					switch content
						when '#node', '#emptynode', "#comment"
							@nodeOffset += 1
							@strOffset = 0
						when "#text"
							@nodeOffset += 1
						else
							offsetNode = iteration[content]
							@nodeOffset = offsetNode.nodeOffset
							@strOffset = offsetNode.strOffset
				
				if @last.type is 'text'
					@strOffset += @last.value.length
				
				@iterations.push iteration
				
				
				# The mergepoint between two last and first or next is counted only once
				if @next.type is 'text' and @last.type is 'text'
					@nodeOffset -= 1
				if @nodeMatches @next
					break
				if @next.type is 'text' and @last.type is 'text'
					@nodeOffset += 1
				
				if @first.type is 'text' and @last.type is 'text'
					@nodeOffset -= 1
				
				console.log 'ITERATE'
			
			console.log @iterations
		




