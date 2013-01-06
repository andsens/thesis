define [
	'views/base/view'
	'text!templates/mustache.mustache'
	
	'json!templates/mustache.mustache-comb'
	
	'comb/template'
	'chaplin'
	'jquery'
], (View, mustacheTpl, mustacheComb
	CombTpl, Chaplin, $) ->
	'use strict'
	
	class InputView extends View
		
		template: mustacheTpl
		template = null
		
		tagName: "form"
		className: "form-horizontal"
		
		initialize: ->
			super
			@$el.on 'submit', -> false
			Chaplin.mediator.subscribe 'templateSelected', @loadTemplate
		
		loadTemplate: (info) =>
			require [
				"text!" + info['template-path']
				"json!" + info['comb-path']
			], (template, spec) =>
				@viewerSpec = spec
				@currentTemplate = template
				Chaplin.mediator.subscribe 'templateRendered', @runComb
				Chaplin.mediator.publish 'templateLoaded', template
		
		runComb: (dom) =>
			@templateData = {}
			@combView = new CombTpl @viewerSpec, dom, {}
			@render()
			@combInput = new CombTpl mustacheComb, @$el[0], {mustache: mustacheComb}
			
			input = @combInput.getValues()
			output = @combView.getValues()
			@mapSection input.section[0], {"root": [output]}, data = {}
			@templateData = data.root[0]
		
		mapSection: (section, output, data) ->
			name = section.name[0].value
			data[name] ?= []
			
			$pushIter = $ section.name[1].parentNode
			$popIter = $ section.name[2].parentNode
			
			$pushIter.on 'click', (e) =>
				data[name].push {}
				@reloadTemplate()
			
			$popIter.on 'click', (e) =>
				data[name].pop()
				@reloadTemplate()
			
			for iteration, i in section.iterations
				data[name][i] = {}
				for subSection in iteration.section
					@mapSection subSection, output[name][i], data[name][i]
				for unescaped in iteration.unescaped
					@mapUnescaped unescaped, output[name][i], data[name][i]
				for escaped in iteration.escaped
					@mapEscaped escaped, output[name][i], data[name][i]
		
		mapUnescaped: (unescaped, output, data) ->
			name = unescaped.name.value
			input = unescaped.nodes[0]
			val = output[name][0]
			data[name] = val.value
			
			$input = $ input.parentNode
			$input.on 'keyup', (e) ->
				string = $input.val()
				data[name] = string
				val.update string
		
		mapEscaped: (escaped, output, data) ->
			name = escaped.name[0].value
			input = escaped.value[0]
			val = output[name][0]
			data[name] = val.value
			
			$input = $ input.parentNode
			$input.on 'keyup', (e) ->
				string = $input.val()
				data[name] = string
				val.update string
		
		type: (root) ->
			for type in ['section', 'unescaped', 'escaped']
				return type if root[type].length
		
		reloadTemplate: ->
			Chaplin.mediator.publish 'valuesChanged',
				template: @currentTemplate
				data: @templateData
		
		getPartials: ->
			{mustache: mustacheTpl}
		
		getTemplateData: ->
			unless @combView?
				return super arguments...
			root = @combView.getRoot()
			templatified = @templatify root
			return {section: [templatified], name: 'root'}
		
		templatify: (object, name) ->
			switch object.type
				when 'section', 'partial'
					iterations = []
					for iteration, i in object.iterations
						section = []
						unescaped = []
						escaped = []
						for itemName, items of iteration
							for item in items
								# We don't really care about a variable being mentioned twice
								temp = @templatify item, itemName
								if temp.iterations?
									section.push temp
								if temp.nodes?
									unescaped.push temp
								if temp.value?
									escaped.push temp
								break
						iterations.push {section, unescaped, escaped}
					obj = {iterations, name}
				when 'unescaped'
					obj = {nodes: object.nodes, name}
				when 'escaped'
					obj = {value: object.value, name}
			return obj
