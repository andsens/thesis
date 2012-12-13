define [
	'views/base/collection_view',
	'views/template_item'
	
	'chaplin'
], (CollectionView, TemplateItemView,
	Chaplin) ->
	'use strict'
	
	class TemplateSelectionView extends CollectionView
		
		itemView: TemplateItemView
		
		tagName: "ul"
		className: "dropdown-menu"
		
		render: ->
			super
			window.coll = @collection
			@$el.attr "role", "menu"
			@$el.attr "aria-labelledby", "templatesDD"
