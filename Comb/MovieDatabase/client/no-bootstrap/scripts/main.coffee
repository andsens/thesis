define [], ->
	require.config
		enforceDefine: true
		shim:
			jquery:
				exports: '$'
			watch:
				deps: ['jquery']
				exports: 'jQuery.fn.watch'
			underscore:
				exports: '_'
			backbone:
				deps: ['underscore', 'jquery']
				exports: 'Backbone'
		paths:
			jquery: 'vendor/jquery.min'
			comb: 'vendor/comb'
			watch: 'vendor/jquery.watch'
			underscore: 'vendor/underscore-min'
			backbone: 'vendor/backbone/backbone'
			mustache: 'vendor/mustache/mustache'
			chaplin: 'vendor/chaplin/chaplin'
			text: 'vendor/requirejs-text/text'
			json: 'vendor/requirejs-plugins/src/json'
			'coffee-script': 'vendor/coffee-script'
			cs: 'vendor/cs'

	require ['cs!app'], (App) ->
		'use strict'
		app = new App()
		app.initialize()
