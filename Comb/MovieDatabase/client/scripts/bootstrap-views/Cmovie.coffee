define [
	'cs!baseviews/Cview'
	'text!templates/movie.mustache'
	'json!templates/movie.mustache-comb'
	'cs!views/Ccast'
	'cs!models/movie'
	'cs!collections/cast'
	'chaplin'
], (View, template, spec, CastView, Movie, Cast, Chaplin) ->
	'use strict'
	
	class MovieView extends View
		
		template: template
		template = null
		
		spec: spec
		spec = null
		
		className: 'row-fluid'
		
		initialize: ->
			super
			unless @model?
				@model = new Movie
			
			if @data?
				@model.set 'id', @data.id.value
				@model.set 'title', @data.title.value
				@model.set 'year', @data.year.value
				@model.set 'synopsis', @data.synopsis.value
				@model.set 'plot', @data.plot.value
			
				@subview 'cast', new CastView
					el: @$('table.cast')[0]
					collection: new Cast()
					movie: @model
					data: @data.cast
			
			@editable 'title'
			@editable 'year'
			@editable 'synopsis'
			@editable 'plot'
			
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
