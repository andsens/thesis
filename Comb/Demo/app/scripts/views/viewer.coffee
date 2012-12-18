define [
	'views/base/view'
	'text!templates/viewer.mustache'
	
	'mustache'
	'chaplin'
], (View, template,
	Mustache, Chaplin) ->
	'use strict'
	
	class ViewerView extends View
		
		template: template
		template = null
		container: "body"
		
		className: "row-fluid"
		
		initialize: ->
			super
			Chaplin.mediator.subscribe 'loadTemplate', @renderTemplate
		
		renderTemplate: (info) =>
			data =
				name: 'test'
				description: 'LAAADIDA<h1>blaaahrg</h1><br/>'
				blah: 'blahtext'
				begin:
					end: 'endtext'
				own: [
						deeper: [
								deepvar: 'uno1'
								deepvar2: 'due1'
							,
								deepvar: 'uno2'
								deepvar2: 'due2'
						]
					,
						deeper:
							deepvar: 'dos'
					,
						deeper:
							deepvar: 'tres'
					]
				expert_skills: [
					  name: 'coding'
					, name: 'architecture'
				]
			@$el.empty().append (Mustache.render info.template, data)
			console.log Mustache.render info.template, data
			Chaplin.mediator.publish 'templateLoaded', @$el[0]
			
