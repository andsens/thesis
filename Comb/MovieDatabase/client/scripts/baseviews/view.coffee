define [
	'mustache'
	'chaplin'
], (Mustache, Chaplin) ->
	'use strict'
	
	class View extends Chaplin.View
		
		initialize: ->
			super
			@partials ?= {}
		
		getTemplateFunction: ->
			return (data) =>
				Mustache.render @template, data, @partials
