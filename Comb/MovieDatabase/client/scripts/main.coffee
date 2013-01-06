define [], ->
	require.config
		enforceDefine: true
		shim:
			jquery:
				exports: '$'
			bootstrap:
				deps: ['jquery']
				exports: '$.fn.popover'
			underscore:
				exports: '_'
			backbone:
				deps: ['underscore', 'jquery']
				exports: 'Backbone'
		paths:
			jquery: 'vendor/jquery.min'
			comb: 'vendor/comb'
			bootstrap: 'vendor/bootstrap.min'
			underscore: 'vendor/underscore-min'
			backbone: 'vendor/backbone/backbone'
			mustache: 'vendor/mustache/mustache'
			chaplin: 'vendor/chaplin/chaplin'
			text: 'vendor/requirejs-text/text'
			json: 'vendor/requirejs-plugins/src/json'
			'coffee-script': 'vendor/coffee-script'
			cs: 'vendor/cs'
			templates: '../templates'

	require ['cs!app'], (App) ->
		'use strict'
		app = new App()
		app.initialize()
