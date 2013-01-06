define [
	'cs!views/base/view'
	'vendor/comb/template'
	'jquery'
	'chaplin'
], (BaseView, Comb, $, Chaplin) ->
	'use strict'
	
	class View extends BaseView
		
		initialize: ->
			super
			if @spec? and @options.el?
				@data = (new Comb @spec, @el, @partials).getValues()
				@trigger 'parsed'
			@editableNodes = []
			@saveRequired = if @model? then @model.isNew() else true
		
		afterRender: ->
			if @spec?
				@data = (new Comb @spec, @el, @partials).getValues()
				@trigger 'parsed'
		
		editable: (prop, field) ->
			unless field?
				field = prop
			
			unless @data?
				@once 'parsed', => @editable prop, field
				return
			
			$node = $(@data[field].parentNode)
			@editableNodes.push $node
			$node.on 'keyup', =>
				@saveRequired = true
				@model.set prop, $node.text(), silent: true
		
		setEditable: (editable) ->
			for $node in @editableNodes
				$node.prop 'contenteditable', editable
		
		subscribeToEditableEvents: (edit, save) ->
			Chaplin.mediator.subscribe edit, =>
				@setEditable true
			Chaplin.mediator.subscribe save, =>
				@setEditable false
				if @saveRequired
					@model.save()
					@saveRequired = false
