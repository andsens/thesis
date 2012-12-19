define [
	'views/base/view'
	'text!templates/input.mustache'
	'text!templates/mustache.mustache'
	
	'json!templates/input.mustache-comb'
	'json!templates/mustache.mustache-comb'
	
	'comb/template'
	'chaplin'
], (View, inputTpl, mustacheTpl,
	inputComb, mustacheComb
	CombTpl, Chaplin) ->
	'use strict'
	
	class InputView extends View
		
		template: inputTpl
		template = null
		
		# tagName: "form"
		className: "indent"
		
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
				Chaplin.mediator.publish 'templateLoaded', template
		
		runComb: (dom) =>
			return unless @viewerSpec?
			@combView = new CombTpl @viewerSpec, dom
			@render()
			@combInput = new CombTpl inputComb, @$el[0], {mustache: mustacheComb}
			console.log @combInput.getSimpleValues()
		
		getTemplateData: ->
			unless @combView?
				return super arguments...
			root = @combView.getValues()
			values = @templatify root, 'root'
			values
		
		templatify: (object, name) ->
			switch object.type
				when 'section'
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
		
		getPartials: ->
			{mustache: mustacheTpl}
