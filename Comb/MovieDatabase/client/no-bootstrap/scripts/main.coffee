require.config
	shim:
		jquery:
			exports: '$'
		underscore:
			exports: '_'
		backbone:
			deps: ['underscore', 'jquery']
			exports: 'Backbone'
	paths:
		jquery: 'vendor/jquery.min'
		underscore: 'vendor/underscore-min'
		backbone: 'vendor/backbone/backbone'
		mustache: 'vendor/mustache/mustache'
		text: 'vendor/requirejs-text/text'
		json: 'vendor/requirejs-plugins/src/json'
		'coffee-script': 'vendor/coffee-script'
		cs: 'vendor/cs'


require ['cs!app'], (app) ->
	console.log app
