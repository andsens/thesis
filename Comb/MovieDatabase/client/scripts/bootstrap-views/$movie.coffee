define [
	'cs!baseviews/$view'
	'text!templates/movie.mustache'
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
				@model.set 'id', (@$('>div').attr 'id').substring 6
				@model.set 'title', @$('h1').text()
				@model.set 'year', @$('h3').text()
				@model.set 'synopsis', @$("#plot-#{@model.id} div").text()
				@model.set 'plot', @$("#synopsis-#{@model.id} div").text()
			
				@subview 'cast', new CastView
					el: @$('table.cast')[0]
					collection: new Cast()
					movie: @model
			
			@editable 'title', 'h1'
			@editable 'year', 'h3'
			@editable 'synopsis', "#plot-#{@model.id} div"
			@editable 'plot', "#synopsis-#{@model.id} div"
			
			@subscribeToEditableEvents "edit:movie:#{@model.cid}", "save:movie:#{@model.cid}"
			Chaplin.mediator.subscribe "save:movie:#{@model.cid}", =>
				@$('.edit-movie i').attr 'class', 'icon-edit'
			
			Chaplin.mediator.subscribe "edit:movie:#{@model.cid}", =>
				@$('.edit-movie i').attr 'class', 'icon-ok'
			
			
			editing = false
			@delegate 'click', '.edit-movie', =>
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
