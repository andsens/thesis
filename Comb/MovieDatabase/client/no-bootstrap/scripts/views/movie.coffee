define [
	'cs!views/base/view'
	'cs!views/cast'
	'cs!models/movie'
	'cs!collections/cast'
	'chaplin'
], (View, CastView, Movie, Cast, Chaplin) ->
	'use strict'
	
	class MovieView extends View
		
		# template: template
		# template = null
		
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
					
					@subview 'roles', new CastView
						el: @$('table.cast')[0]
						collection: new Cast()
						movie_id: @model.id
			
			@editable 'title', 'summary.title'
			@editable 'year', 'span.year'
			@editable 'synopsis', 'summary.synopsis span'
			@editable 'plot', 'details.plot p'
			
			@subscribeToEditableEvents "edit:movie:#{@options.movie_id}", "save:movie:#{@options.movie_id}"
			Chaplin.mediator.subscribe "save:movie:#{@model.id}", =>
				@$('.edit.command').text 'Edit'
			
			Chaplin.mediator.subscribe "edit:movie:#{@model.id}", =>
				@$('.edit.command').text 'Save'
			
			
			editing = false
			@delegate 'click', '.edit.command', =>
				editing = !editing
				if editing
					Chaplin.mediator.publish "edit:movie:#{@model.id}"
				else
					Chaplin.mediator.publish "save:movie:#{@model.id}"
			
