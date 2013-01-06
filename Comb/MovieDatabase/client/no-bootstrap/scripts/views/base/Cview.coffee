define [
	'cs!views/base/view'
	'vendor/comb/template'
	'chaplin'
], (BaseView, Comb, Chaplin) ->
	'use strict'
	
	class View extends BaseView
		
		initialize: ->
			super
			@data = {}
			if @spec? and @options.el?
				@data = (new Comb @spec, @el, @partials).getValues()
			console.log @data
		
		afterRender: ->
			if @spec?
				@data = (new Comb @spec, @el, @partials).getValues()
		
