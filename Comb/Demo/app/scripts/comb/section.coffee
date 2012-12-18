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
			prev_first_joined = false
			prev_next_joined  = false
			last_first_joined = false
			last_next_joined  = false
			if @id isnt 'root'
				# True / False, potato / potahto
				prev_first_joined = @prev.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				prev_next_joined  = @prev.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				last_first_joined = @last.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				last_next_joined  = @last.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				
				prev_first_joined = prev_first_joined or @prev.type is 'null'
				prev_next_joined  = prev_next_joined or @prev.type is 'null'
				last_next_joined  = last_next_joined or @next.type is 'null'
			
				prev_first_offset = if prev_first_joined then 0 else 1
				prev_next_offset  = if prev_next_joined  then 0 else 1
				last_first_offset = if last_first_joined then 0 else 1
				last_next_offset  = if last_next_joined  then 0 else 1
			
				@verifying 'first', @first, prev_first_offset
				unless @nodeMatches @first, prev_first_offset
					@verifying 'next', @next, prev_next_offset
					unless @nodeMatches @next, prev_next_offset
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
						childNodeOffset += if (i is 0) then prev_first_offset else last_first_offset
					switch child.type
						when 'section'
							obj = new Section child, @spec, @parent, childNodeOffset, @strOffset
						when 'escaped'
							obj = new EscapedVariable child, @spec, @parent, childNodeOffset, @strOffset
						when 'unescaped'
							obj = new UnescapedVariable child, @spec, @parent, childNodeOffset, @strOffset
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
							obj = new UnescapedVariable child, @spec, @parent, offsetNode.nodeOffset, offsetNode.strOffset
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
				
				
				@verifying 'first', @first, last_first_offset
				if @nodeMatches @first, last_first_offset
					@verifying 'next', @next, last_next_offset
					if @nodeMatches @next, last_next_offset
						throw new Error "Was able to both match a continuation and an end of the iteration"
					console.log 'ITERATE'
				else
					iterate = false
				i++	
			
			if @id isnt 'root'
				@verifying 'next', @next, last_next_offset
				unless @nodeMatches @next, last_next_offset
					throw new Error "Unable to match the next node"
			
			console.log @iterations
		




