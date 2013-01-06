define [
	'mustache'
	'chaplin'
], (Mustache, Chaplin) ->
	'use strict'
	
	class View extends Chaplin.View
		
		initialize: ->
			super
			@partials ?= {}
			@editableSelectors = []
			@saveRequired = false
		
		getTemplateFunction: ->
			return (data) =>
				Mustache.render @template, data, @partials
		
		editable: (prop, selector) ->
			@editableSelectors.push selector
			if selector
				@delegate 'keyup', selector, =>
					@saveRequired = true
					@model.set prop, @$(selector).text(), silent: true
			else
				@delegate 'keyup', =>
					@saveRequired = true
					@model.set prop, @$el.text(), silent: true
		
		setEditable: (editable) ->
			for selector in @editableSelectors
				if selector?
					@$(selector).prop 'contenteditable', editable
				else
					@$el.prop 'contenteditable', editable
		
		subscribeToEditableEvents: (edit, save) ->
			Chaplin.mediator.subscribe edit, =>
				@setEditable true
			Chaplin.mediator.subscribe save, =>
				@setEditable false
				if @saveRequired
					@model.save()
					@saveRequired = false
