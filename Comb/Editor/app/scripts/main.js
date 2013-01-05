require.config({
  shim: {
		backbone: {
			deps: ['underscore', 'jquery'],
			exports: 'Backbone'
		},
		underscore: {
			exports: '_'
		}
  },

  paths: {
    hm: 'vendor/hm',
    esprima: 'vendor/esprima',
    jquery: 'vendor/jquery.min',
    chaplin: 'vendor/chaplin/chaplin',
		backbone: '../../components/backbone/backbone',
		underscore: '../../components/underscore/underscore',
		mustache: '../../components/mustache/mustache',
		text: '../../components/requirejs-text/text',
		json: '../../components/requirejs-plugins/src/json'
  }
});
 
require(['app'], function(App) {
	'use strict';
	var app = new App();
	app.initialize();
});
