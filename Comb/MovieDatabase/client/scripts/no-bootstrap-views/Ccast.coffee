define [
	'cs!baseviews/collection_view'
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
			if @options.data?
				for role in @options.data
					view = new RoleView {el: role.id[0].parentNode, id: role.id.value, movie: @options.movie}
					@subview "itemView:#{view.model.cid}", view
					@collection.push view.model, silent: true
				@$list = @$ @listSelector
			
			@delegate 'click', '#add_role_button', =>
				@collection.push new Role()
		
		getView: (model) ->
			new RoleView {model, movie: @options.movie}
