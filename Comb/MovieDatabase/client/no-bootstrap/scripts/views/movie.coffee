define [
	'cs!views/base/view'
	'cs!models/movie'
	'watch'
], (View, Movie, unbound) ->
	'use strict'
	
	class MovieView extends View
		
		# template: template
		# template = null
		
		tagName: "details"
		
		initialize: ->
			super
			
			unless @model?
				@model = new Movie
				if @options.el?
					@model.set 'id', (@$el.attr 'id').substring 6
					@model.set 'title', @$('summary.title').text()
					@model.set 'year', @$('span.year').text()
					@model.set 'synopsis', @$('summary.synopsis span').text()
					@model.set 'plot', @$('details.plot p').text()
			
			onKeyUp = (prop, sel) =>
				@delegate 'keyup', sel, =>
					@model.set prop, @$(sel).text(), silent: true
			
			onKeyUp 'title', 'summary.title'
			onKeyUp 'year', 'span.year'
			onKeyUp 'synopsis', 'summary.synopsis span'
			onKeyUp 'plot', 'details.plot p'
			
			editing = false
			
			@delegate 'click', '.edit.command', =>
				editing = !editing
				
				if editing
					text = 'Save'
				else
					text = 'Edit'
				for sel in ['summary.title', 'span.year', 'summary.synopsis span', 'details.plot p']
					@$(sel).prop 'contenteditable', editing
				@$('.edit.command').text text
				unless editing
					@model.save()
				
