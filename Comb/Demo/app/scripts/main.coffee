require.config
	enforceDefine: true
	baseUrl: 'scripts/'
	shim:
		backbone:
			deps: ['underscore', 'jquery']
			exports: 'Backbone'
		underscore:
			exports: '_'
		'bootstrap-affix':
			deps: ['jquery']
			exports: '$.fn.affix'
		'bootstrap-alert':
			deps: ['jquery']
			exports: '$.fn.alert'
		'bootstrap-dropdown':
			deps: ['jquery']
			exports: '$.fn.dropdown'
		'bootstrap-tooltip':
			deps: ['jquery']
			exports: '$.fn.tooltip'
		'bootstrap-modal':
			deps: ['jquery']
			exports: '$.fn.modal'
		'bootstrap-transition':
			deps: ['jquery']
			exports: '$.support.transition'
		'bootstrap-button':
			deps: ['jquery']
			exports: '$.fn.button'
		'bootstrap-popover':
			deps: ['jquery', 'bootstrap-tooltip']
			exports: '$.fn.popover'
		'bootstrap-typeahead':
			deps: ['jquery']
			exports: '$.fn.typeahead'
		'bootstrap-carousel':
			deps: ['jquery']
			exports: '$.fn.carousel'
		'bootstrap-scrollspy':
			deps: ['jquery']
			exports: '$.fn.scrollspy'
		'bootstrap-collapse':
			deps: ['jquery']
			exports: '$.fn.collapse'
		'bootstrap-tab':
			deps: ['jquery']
			exports: '$.fn.tab'
	paths:
		hm: 'vendor/hm'
		esprima: 'vendor/esprima'
		jquery: 'vendor/jquery.min'
		modernizr: 'vendor/modernizr.min'
		chaplin: 'vendor/chaplin/src/chaplin'
		backbone: '../../components/backbone/backbone'
		underscore: '../../components/underscore/underscore'
		mustache: '../../components/mustache/mustache'
		text: '../../components/requirejs-text/text'
		json: '../../components/requirejs-plugins/src/json'
		'bootstrap-affix': 'vendor/bootstrap/bootstrap-affix'
		'bootstrap-alert': 'vendor/bootstrap/bootstrap-alert'
		'bootstrap-dropdown': 'vendor/bootstrap/bootstrap-dropdown'
		'bootstrap-tooltip': 'vendor/bootstrap/bootstrap-tooltip'
		'bootstrap-modal': 'vendor/bootstrap/bootstrap-modal'
		'bootstrap-transition': 'vendor/bootstrap/bootstrap-transition'
		'bootstrap-button': 'vendor/bootstrap/bootstrap-button'
		'bootstrap-popover': 'vendor/bootstrap/bootstrap-popover'
		'bootstrap-typeahead': 'vendor/bootstrap/bootstrap-typeahead'
		'bootstrap-carousel': 'vendor/bootstrap/bootstrap-carousel'
		'bootstrap-scrollspy': 'vendor/bootstrap/bootstrap-scrollspy'
		'bootstrap-collapse': 'vendor/bootstrap/bootstrap-collapse'
		'bootstrap-tab': 'vendor/bootstrap/bootstrap-tab'
require ['Client'], (Client) ->
	client = new Client
	client.initialize()
