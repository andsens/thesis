define [
	'cs!views/base/view'
	'chaplin'
], (BaseView, Chaplin) ->
	'use strict'
	
	class View extends BaseView
		
		initialize: ->
			super
			@editableSelectors = []
			@saveRequired = if @model? then @model.isNew() else true
		
		editable: (prop, selector) ->
			@editableSelectors.push selector
			@delegate 'keyup', selector, =>
				@saveRequired = true
				@model.set prop, @$(selector).text(), silent: true
		
		setEditable: (editable) ->
			for selector in @editableSelectors
				@$(selector).prop 'contenteditable', editable
		
		subscribeToEditableEvents: (edit, save) ->
			Chaplin.mediator.subscribe edit, =>
				@setEditable true
			Chaplin.mediator.subscribe save, =>
				@setEditable false
				if @saveRequired
					@model.save()
					@saveRequired = false
