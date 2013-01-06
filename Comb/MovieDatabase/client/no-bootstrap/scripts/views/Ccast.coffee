define [
	'cs!views/base/collection_view'
	'cs!views/Crole'
	'cs!models/role'
], (CollectionView, RoleView, Role) ->
	'use strict'
	
	class CastView extends CollectionView
		
		tagName: 'table'
		className: 'cast'
		listSelector: 'tbody'
		itemView: RoleView
		
		initialize: ->
			super
			if @options.el?
				for el in @$('tbody tr')
					view = new RoleView {el, movie: @options.movie}
					@subview "itemView:#{view.model.cid}", view
					@collection.push view.model, silent: true
				@$list = @$ @listSelector
			
			@delegate 'click', '#add_role_button', =>
				@collection.push new Role()
		
		getView: (model) ->
			new RoleView {model, movie: @options.movie}
