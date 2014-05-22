define [
	'mustache'
	'chaplin'
], (Mustache, Chaplin) ->
	'use strict'
	
	class View extends Chaplin.View
		
		getTemplateFunction: ->
			template = @template
			if @getPartials?
				partials = @getPartials()
			else
				partials = {}
			return (view) ->
				Mustache.render template, view, partials
