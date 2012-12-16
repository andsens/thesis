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
			
			# move the strOffset up to the first node
			if @prev.type is 'text'
				@strOffset += @prev.value.length
			
			prev_first_joined = @prev in ['text', 'escaped'] and @first in ['text', 'escaped']
			last_first_joined = @last in ['text', 'escaped'] and @first in ['text', 'escaped']
			last_next_joined = @last in ['text', 'escaped'] and @next in ['text', 'escaped']
			
			# @nodeOffset always points at @prev
			if @id isnt 'root'
				@nodeOffset += 1 unless prev_first_joined
				return unless @nodeMatches @first
				@nodeOffset -= 1 unless prev_first_joined
			
			i = 0
			while true
				iteration = {}
				
				# Parse the children that are not offset by preceeding items
				for child in @children when child.path[0].type is 'child'
					if child.path[0].node isnt @id
						throw new Error {msg: "Unexpected path in list of children", path: child.path[0]}
					# parent.childNodes[0].childNodes[0] is quite different from parent.childNodes[0+0]
					# This happens when we are using an offset for initialization
					childNodeOffset = @nodeOffset
					# We only need to correct it for the first child though
					# if childNodeOffset > 0
					if childNodeOffset > 0 and @path[0].type is 'child'
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
				
				if @last.type in ['text', 'escaped']
					@strOffset += @last.value.length
				
				
				# The mergepoint between last and first or next is counted only once
				@nodeOffset -= 1 if last_next_joined
				if @nodeMatches @next
					break
				@nodeOffset += 1 unless last_first_joined
				return unless @nodeMatches @first
				@nodeOffset -= 1 unless last_first_joined
				
				i++	
				console.log 'ITERATE'
			
			console.log @iterations
		




