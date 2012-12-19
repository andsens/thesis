define [
	'comb/mustache'
	
	'comb/partial'
	'comb/escaped_variable'
	'comb/unescaped_variable'
	
	'exports'
	'underscore'
], (Mustache,
	Partial, EscapedVariable, UnescapedVariable,
	exports, _) ->
	'use strict'
	
	class Section extends Mustache
		
		initialize: ->
			super
			
			@children   = ({id, child} for child, id in @spec when child.section is @id)
			@iterations = []
		
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
			
				unless @nodeMatches 'first', @first, prev_first_offset
					unless @nodeMatches 'next', @next, prev_next_offset
						throw new Error "Unable to match the next node"
					return
			
			i = 0
			while true
				iteration = {}
				
				# Parse the children that are not offset by preceeding items
				for {id, child:{type,offset}} in @children when not offset
					# Child nodes have no idea whether @prev/@last count towards the nodeOffset or not
					childNodeOffset = @nodeOffset
					if @id isnt 0
						childNodeOffset += if (i is 0) then prev_first_offset else last_first_offset
					switch type
						when 'section'
							obj = new Section id, @spec, @partials, @parent, childNodeOffset, @strOffset
						when 'partial'
							obj = new Partial id, @spec, @partials, @parent, childNodeOffset, @strOffset
						when 'escaped'
							obj = new EscapedVariable id, @spec, @partials, @parent, childNodeOffset, @strOffset
						when 'unescaped'
							obj = new UnescapedVariable id, @spec, @partials, @parent, childNodeOffset, @strOffset
					iteration[id] = obj
				
				# Parse the children that are offset by preceeding items
				for {id, child:{type,offset,path:[offsetNodeId]}} in @children when offset
					offsetNode = iteration[offsetNodeId]
					unless offsetNode?
						throw new Error "Something is wrong with the ordering of the offset children"
					switch type
						when 'section'
							obj = new Section id, @spec, @partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'partial'
							obj = new Partial id, @spec, @partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'escaped'
							obj = new EscapedVariable id, @spec, @partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
						when 'unescaped'
							obj = new UnescapedVariable id, @spec, @partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset
					iteration[id] = obj
				
				@iterations.push iteration
				break if @id is 0
				
				# Check how much the children have affected the length of this section
				for content, j in @contents
					switch content
						when '#node', '#emptynode', "#comment"
							@strOffset = 0
							break if i is 0 and j is 0 and prev_first_joined
							@nodeOffset += 1
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
				
				unless @nodeMatches 'last', @last
					throw new Error "Unable to verify last element in iteration"
				
				if @last.type in ['text', 'escaped']
					@strOffset += @last.value.length
				
				
				nextMatched = @nodeMatches 'next', @next, last_next_offset
				
				if @nodeMatches 'first', @first, last_first_offset
					if nextMatched
						throw new Error "Was able to both match a continuation and an end of the iteration"
				else
					unless nextMatched
						throw new Error "Unable to match the next node"
					break
				i++
			
			# All the following offsets rely on the nodeOffset to be at the next node
			@nodeOffset += if (i is 0) then last_next_offset else prev_next_offset
		
		getObject: ->
			object = []
			for iteration in @iterations
				values = {}
				for id, item of iteration
					unless values[item.name]?
						values[item.name] = []
					values[item.name].unshift item.getObject()
				object.unshift values
			return {type: 'section', iterations: object}
		
		getSimple: ->
			if @iterations.length is 0
				return null
			
			object = []
			for iteration in @iterations
				values = {}
				for id, item of iteration
					if values[item.name]?
						unless _.isArray values[item.name]
							values[item.name] = [values[item.name]]
						values[item.name].unshift item.getSimple()
					else
						values[item.name] = item.getSimple()
				object.unshift values
			if object.length is 1
				object = object[0]
			object
	
	exports.Section = Section
