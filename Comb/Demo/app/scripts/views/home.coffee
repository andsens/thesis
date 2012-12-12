define [
	'views/base/view'
	'text!templates/home.mustache'
], (View, template) ->
	'use strict'
	
	class Home extends View
		
		template: template
		template = null
		container: "body"
		
		tagName: "div"
		className: "row-fluid"
		
		autoRender: true
