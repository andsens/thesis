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
			Chaplin.mediator.subscribe 'templateLoaded', @renderTemplate
		
		renderTemplate: (template) =>
			data =
				name: 'test'
				description: 'LAAADIDA<h1>blaaahrg</h1><br/>'
				formatdate:
					date_of_employment: 'date_of_employment'
				blah: 'blahtext'
				begin:
					end: 'endtext'
				# formal_photo: 'blah'
				informal_photo: 'blah'
				own: true
					# 	deeper: [
					# 			deepvar: 'uno1'
					# 			deepvar2: 'due1'
					# 		,
					# 			deepvar: 'uno2'
					# 			deepvar2: 'due2'
					# 	]
					# ,
					# 	deeper:
					# 		deepvar: 'dos'
					# ,
					# 	deeper:
					# 		deepvar: 'tres'
					# ]
				expert_skills: [
					  name: 'coding'
					, name: 'architecture'
				]
			@$el.empty().append (Mustache.render template, data)
			Chaplin.mediator.publish 'templateRendered', @$el[0]
			
