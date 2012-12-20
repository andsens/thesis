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
			Chaplin.mediator.subscribe 'templateSelected', @loadTemplate
		
		loadTemplate: (info) =>
			require [
				"text!" + info['template-path']
				"json!" + info['comb-path']
			], (template, spec) =>
				@viewerSpec = spec
				Chaplin.mediator.subscribe 'templateRendered', @runComb
				Chaplin.mediator.publish 'templateLoaded', template, {}
		
		runComb: (dom) =>
			return unless @viewerSpec?
			@combView = new CombTpl @viewerSpec, dom
			@render()
			@combInput = new CombTpl mustacheComb, @$el[0], {mustache: mustacheComb}
			
			input = @combInput.getValues()
			output = @combView.getValues()
			console.log input, output
			@mapValues input.section[0].iterations, output
			
		mapValues: (input, output) ->
			for {mustache:root}, i in input
				do (root) =>
					type = @type(root)
					part = root[type][0]
					name = part.name.value
					switch type
						when 'section'
							@mapValues part.iterations, output[name]
						when 'escaped'
							$value = $ part.value.parentNode
							console.log output[name]
							$value.on 'keyup', (e) ->
								output[name].update $value.val()
							# console.log name, output[name]
						when 'unescaped'
							console.log name, output[name]
		
		type: (part) ->
			for type in ['section', 'unescaped', 'escaped']
				return type if part[type].length
		
		getPartials: ->
			{mustache: mustacheTpl}
		
		getTemplateData: ->
			unless @combView?
				return super arguments...
			root = @combView.getRoot()
			return @templatify root
			console.log root
			# return (@templatify {type: 'section', iterations: root}, 'root')
		
		templatify: (object, name) ->
			switch object.type
				when 'section', 'partial'
					iterations = []
					for iteration, i in object.iterations
						for itemName, items of iteration
							for item in items
								# We don't really care about a variable being mentioned twice
								iterations.push @templatify item, itemName
								break
					obj = {section: true, escaped: false, unescaped: false, iterations, name, variables: iterations.length, i: object.iterations.length}
				when 'escaped'
					obj = {section: false, escaped: true, unescaped: false, value: object.value, name}
				when 'unescaped'
					obj = {section: false, escaped: false, unescaped: true, nodes: object.nodes, name}
			return obj
