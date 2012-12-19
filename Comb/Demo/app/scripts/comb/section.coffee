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
		
		legal_first_next_var: ['node', 'emptynode', 'comment', 'null']
		
		initialize: ->
			super
			
			unless @id is 0
				switch @first.type
					when 'section' then throw new Error "Section as first child not yet supported"
					when 'escaped' then throw new Error "Escaped as first child not yet supported"
					when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
				switch @last.type
					when 'section' then throw new Error "Section as first child not yet supported"
					when 'escaped' then throw new Error "Escaped as first child not yet supported"
					when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
				# 	when 'escaped'
				# 		unless @next.type in @legal_first_next_var
				# 			throw new Error "@next must be one of "+@legal_first_next_var.join(' ')+
				# 				" if an escaped variable is to be the first child of a section"
				# 	when 'unescaped' then throw new Error "Unescaped as first child not yet supported"
				# switch @next.type
				# 	when 'section' then throw new Error "Section as first child not yet supported"
				# 	when 'escaped'
				# 		unless @first.type in @legal_first_next_var
				# 			throw new Error "@first must be one of "+@legal_first_next_var.join(' ')+
				# 				" if an escaped variable is to be the next sibling of a section"
			
			@children   = ({id, child} for child, id in @spec when child.section is @id)
			@iterations = []
			console.log @id, @children
		
		parse: ->
			super
			# @nodeOffset always points at at @prev
			prev_first_joined = false
			prev_next_joined  = false
			last_first_joined = false
			last_next_joined  = false
			if @id isnt 0
				# True / False, potato / potahto
				prev_first_joined = @prev.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				prev_next_joined  = @prev.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				last_first_joined = @last.type in ['text', 'escaped'] and @first.type in ['text', 'escaped']
				last_next_joined  = @last.type in ['text', 'escaped'] and @next.type in ['text', 'escaped']
				
				prev_first_joined = prev_first_joined or @prev.type is 'null'
				prev_next_joined  = prev_next_joined or @prev.type is 'null'
				# last_next_joined  = last_next_joined or @next.type is 'null'
			
				prev_first_offset = if prev_first_joined then 0 else 1
				prev_next_offset  = if prev_next_joined  then 0 else 1
				last_first_offset = if last_first_joined then 0 else 1
				last_next_offset  = if last_next_joined  then 0 else 1
			
				@verifying 'first', @first, prev_first_offset
				unless @nodeMatches @first, prev_first_offset
					@verifying 'next', @next, prev_next_offset
					unless @nodeMatches @next, prev_next_offset
						throw new Error "Unable to match the next node"
					return
			
			i = 0
			while true
				iteration = {}
				
				# Parse the children that are not offset by preceeding items
				for {id, child:{type,path:[top]}} in @children when top.type is 'child'
					if top.node isnt @id
						throw new Error {msg: "Unexpected path in list of children", path: child.path[0]}
					# Child nodes have no idea whether @prev/@last count towards the nodeOffset or not
					childNodeOffset = @nodeOffset
					if @id isnt 0
						childNodeOffset += if (i is 0) then prev_first_offset else last_first_offset
					switch type
						when 'section'
							obj = new Section id, @spec, @parent, childNodeOffset, @strOffset
						when 'escaped'
							obj = new EscapedVariable id, @spec, @parent, childNodeOffset, @strOffset
						when 'unescaped'
							obj = new UnescapedVariable id, @spec, @parent, childNodeOffset, @strOffset
					iteration[id] = obj
				
				# Parse the children that are offset by preceeding items
				for {id, child:{type,path:[top]}} in @children when top.type is 'offset'
					offsetNode = iteration[top.node]
					unless offsetNode?
						throw new Error "Something is wrong with the ordering of the offset children"
					switch type
						when 'section'
							obj = new Section id, @spec, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'escaped'
							obj = new EscapedVariable id, @spec, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'unescaped'
							obj = new UnescapedVariable id, @spec, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
					iteration[id] = obj
				
				@iterations.push iteration
				break if @id is 0
				
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
				
				
				@verifying 'next', @next, last_next_offset
				nextMatched = @nodeMatches @next, last_next_offset
				
				@verifying 'first', @first, last_first_offset
				if @nodeMatches @first, last_first_offset
					if nextMatched
						throw new Error "Was able to both match a continuation and an end of the iteration"
				else
					unless nextMatched
						console.log @parent, @node(1)
						throw new Error "Unable to match the next node"
					break
				console.log 'ITERATE'
				i++
			
			# All the following offsets rely on the nodeOffset to be at the next node
			@nodeOffset += if (i is 0) then last_next_offset else prev_next_offset
			
			console.log @iterations
		




