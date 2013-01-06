define [
	'cs!views/base/collection_view'
	'cs!views/role'
	'cs!models/role'
], (CollectionView, RoleView, Role) ->
	'use strict'
	
	class CastView extends CollectionView
		
		tagName: 'tbody'
		itemView: RoleView
		
		initialize: ->
			super
			for el in @$('tr')
				view = new RoleView {el}
				@subview "itemView:#{view.model.cid}", view
				@collection.push view.model, silent: true
