define [
	'chaplin',
	'cs!views/base/view'
], (Chaplin, View) ->
	'use strict'

	class CollectionView extends Chaplin.CollectionView
		autoRender: false
		getTemplateFunction: View::getTemplateFunction
