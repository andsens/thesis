define [
	'views/base/view'
	'text!templates/input.mustache'
	'text!templates/mustache.mustache'
	
	'comb/template'
	'chaplin'
], (View, template, mustacheTpl
	CombTpl, Chaplin) ->
	'use strict'
	
	class InputView extends View
		
		template: template
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
				@spec = spec
				Chaplin.mediator.subscribe 'templateRendered', @runComb
				Chaplin.mediator.publish 'templateLoaded', template
		
		runComb: (dom) =>
			return unless @spec?
			@comb = new CombTpl @spec, dom
			@render()
		
		getTemplateData: ->
			unless @comb?
				return super arguments...
			root = @comb.getValues()
			values = @templatify root, 'root'
			console.log values
			{values}
		
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
