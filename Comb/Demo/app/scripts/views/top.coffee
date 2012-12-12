define [
	'views/base/view'
	'text!templates/top'
], (View, template) ->
	'use strict'
	
	class Top extends View
		
		template: template
