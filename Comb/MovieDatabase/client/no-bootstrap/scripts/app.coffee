define [
	'jquery'
	'underscore'
], ($, _) ->
	$('section#movies li>details>summary').click (e) ->
		$('section#movies li>details').prop 'open', false
  return 'Hello from Comb!';
