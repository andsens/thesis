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
			iterate = true
			# @nodeOffset always points at at @prev
			if @id isnt 'root'
				prev_first_joined = @prev.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				prev_next_joined  = @prev.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				last_first_joined = @last.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				last_next_joined  = @last.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				
				prev_first_joined = prev_first_joined or @prev.type is 'null'
				prev_next_joined  = prev_next_joined or @prev.type is 'null'
				last_next_joined  = last_next_joined or @next.type is 'null'
				
				@verifying 'first', @first, !prev_first_joined # True / False, potato / potahto
				unless @nodeMatches @first, !prev_first_joined
					@verifying 'next', @next, !prev_next_joined
					unless @nodeMatches @next, !prev_next_joined
						throw new Error "Unable to match the next node"
					iterate = false
			
			i = 0
			while iterate
				iteration = {}
				
				# Parse the children that are not offset by preceeding items
				for child in @children when child.path[0].type is 'child'
					if child.path[0].node isnt @id
						throw new Error {msg: "Unexpected path in list of children", path: child.path[0]}
					# Child nodes have no idea whether @prev/@last count towards the nodeOffset or not
					childNodeOffset = @nodeOffset
					if @id isnt 'root'
						if (i is 0 and not prev_first_joined) or (i > 0 and not last_first_joined)
							childNodeOffset += 1
					switch child.type
						when 'section'
							obj = new Section child, @spec, @parent, childNodeOffset, @strOffset
						when 'escaped'
							obj = new EscapedVariable child, @spec, @parent, childNodeOffset, @strOffset
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
				
				@iterations.push iteration
				break if @id is 'root'
				
				# Check how much the children have affected the length of this section
				for content, j in @contents
					switch content
						when '#node', '#emptynode', "#comment"
							@nodeOffset += 1
							@strOffset = 0
						when '#text'
							if j is 0
								break if i is 0 and prev_first_joined
								break if i > 0 and last_first_joined
							if not @contents[j-1]? or @contents[j-1] in ['#node', '#emptynode', '#comment']
								@nodeOffset += 1
						else
							offsetNode = iteration[content]
							@nodeOffset = offsetNode.nodeOffset
							@strOffset = offsetNode.strOffset
				
				@verifying 'last', @last
				unless @nodeMatches @last
					throw new Error "Unable to verify last element in iteration"
				
				if @last.type in ['text', 'escaped']
					@strOffset += @last.value.length
				
				
				@verifying 'first', @first, !last_first_joined
				if @nodeMatches @first, !last_first_joined
					@verifying 'next', @next, !last_next_joined
					if @nodeMatches @next, !last_next_joined
						throw new Error "Was able to both match a continuation and an end of the iteration"
					console.log 'ITERATE'
				else
					iterate = false
				i++	
			
			@verifying 'next', @next, !last_next_joined
			unless @nodeMatches @next, !last_next_joined
				throw new Error "Unable to match the next node"
			
			console.log @iterations
		




