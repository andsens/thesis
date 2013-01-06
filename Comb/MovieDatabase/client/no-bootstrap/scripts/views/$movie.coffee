define [
	'cs!views/base/$view'
	'text!/templates/movie.mustache'
	'cs!views/$cast'
	'cs!models/movie'
	'cs!collections/cast'
	'chaplin'
], (View, template, CastView, Movie, Cast, Chaplin) ->
	'use strict'
	
	class MovieView extends View
		
		template: template
		template = null
		
		tagName: "li"
		
		initialize: ->
			super
			
			unless @model?
				@model = new Movie
			
			if @options.el?
				@model.set 'id', (@$('>details').attr 'id').substring 6
				@model.set 'title', @$('summary.title').text()
				@model.set 'year', @$('span.year').text()
				@model.set 'synopsis', @$('summary.synopsis span').text()
				@model.set 'plot', @$('details.plot p').text()
			
				@subview 'cast', new CastView
					el: @$('table.cast')[0]
					collection: new Cast()
					movie: @model
			
			@delegate 'keyup', 'h1', =>
				@$('summary.title').text (@$ 'h1').text()
			
			@editable 'title', 'h1'
			@editable 'year', 'span.year'
			@editable 'synopsis', 'summary.synopsis span'
			@editable 'plot', 'details.plot p'
			
			@subscribeToEditableEvents "edit:movie:#{@model.cid}", "save:movie:#{@model.cid}"
			Chaplin.mediator.subscribe "save:movie:#{@model.cid}", =>
				@$('.edit.command').text 'Edit'
			
			Chaplin.mediator.subscribe "edit:movie:#{@model.cid}", =>
				@$('.edit.command').text 'Save'
			
			
			editing = false
			@delegate 'click', '.edit.command', =>
				editing = !editing
				if editing
					Chaplin.mediator.publish "edit:movie:#{@model.cid}"
				else
					Chaplin.mediator.publish "save:movie:#{@model.cid}"
		
		afterRender: ->
			@subview 'cast', new CastView
				el: @$('table.cast')[0]
				collection: new Cast()
				movie: @model
