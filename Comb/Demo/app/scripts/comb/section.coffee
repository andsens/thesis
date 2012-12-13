define [
	'comb/variable'
	'comb/section'
	'underscore'
], (Variable, Section,
	_) ->
	'use strict'
	
	class Section
		
		separator: '.'
		
		constructor: (@id, item, @spec) ->
			@[prop] = val for prop, val of item
			
			(stack_match = @stack.slice 0).unshift @name
			belongsHere = (stack) =>
				if not @id?
					return stack.length == 0
				return false if stack_match.length isnt stack.length
				for part, i in stack
					if stack_match[i] isnt part
						return false
				return true
			@itemsById = {}
			@itemsByName = {}
			for id, item of @spec when belongsHere item.stack
				if item.type is 'variable'
					resolution = new Variable id, item
				else
					resolution = new Section id, item, @spec
				@itemsById[resolution.id] = resolution
				if @itemsByName[resolution.name]?
					@itemsByName[resolution.name].push resolution
				else
					@itemsByName[resolution.name] = [resolution]
		
		get: (query, dom) ->
			if query.length is 0
				return [dom]
			current = query.shift()
			items = @itemsByName[current]
			results = []
			if items?
				_.each (items), (item) =>
					subDom = @find item, dom
					console.log subDom.outerHtml
					result = item.get query, subDom
					if result?
						results = results.concat result
			if results.length isnt 0
				return results
				
		
		find: (item, dom) ->
			root = _.last item.path
			if root.type is 'offset'
				dom = @offset root, dom
			
			route = _.initial _.rest item.path
			while part = route.pop()
				dom = @index part, dom
				console.log dom.outerHTML
			dom
		
		index: (index, dom) ->
			dom.childNodes[index.i]
			# _.rest dom.childNodes, index.i

                       # "path": [{"type": "index", "i": 0}, {"type": "index", "i": 1},
                       #          {"type": "index", "i": 1}, {"type": "root"}],
