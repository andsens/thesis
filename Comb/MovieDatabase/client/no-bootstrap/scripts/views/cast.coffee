define [
	'cs!views/base/collection_view'
	'cs!views/role'
	'cs!models/role'
], (CollectionView, RoleView, Role) ->
	'use strict'
	
	class CastView extends CollectionView
		
		tagName: 'table'
		className: 'cast'
		itemView: RoleView
		
		initialize: ->
			super
			for el in @$('tbody tr')
				view = new RoleView {el, movie_id: @options.movie_id}
				@subview "itemView:#{view.model.cid}", view
				@collection.push view.model, silent: true
