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
		
		getObject: ->
			type: 'unescaped'
			nodes: @nodes

		getSimple: ->
			@nodes
