define [
	'comb/mustache'
], (Mustache) ->
	'use strict'
	
	class UnescapedVariable extends Mustache
		
		initialize: ->
			super
			@nodes = []
		
		parse: ->
			super
			node = @node()
			switch @prev.type
				when 'section', 'escaped', 'text'
					if node.nodeType is 3 and node.data.length is @strOffset
						@nodeOffset += 1
				when 'null' then
				when 'unescaped'
					throw new Error "Unsupported matching type"
				when 'node', 'emptynode', 'comment'
					@nodeOffset += 1
			
			node = @node()
			while node?
				@nodes.push node
				node = node.nextSibling
		
		getRoot: ->
			parent = parentNode = @parent
			if parent.nodeType is 2
				parentNode = parent.ownerElement
			obj =
				type: 'unescaped'
				nodes: @nodes
				update: @update
				parent: parent
				parentNode: parentNode
			return obj
		
		getValues: (merge) ->
			if merge?
				unless _.isArray merge
					merge = [merge]
				merge.push @getRoot()
			else
				merge = @getRoot()
			return merge
		
		update: ->
			
