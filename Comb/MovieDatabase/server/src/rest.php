<?php
use \MovieDatabase\Model as M;
require 'init.inc.php';

global $payload;
$payload = json_decode(file_get_contents('php://input'));

$router = new \Respect\Rest\Router('/rest');

$router->post('/movies', function() {
	$movie = new M\Movie;
	$movie->update_attributes((array) $payload);
});
$router->put('/movie/*', function($id) {
	$movie = M\Movie::find($id);
	unset($payload->cast);
	$movie->update_attributes((array) $payload);
});

$router->post('/actor', function() {
	$actor = new M\Actor;
	$actor->update_attributes((array) $payload);
});
$router->put('/actor/*', function($id) {
	$actor = M\Actor::find($id);
	$actor->update_attributes((array) $payload);
});

$router->post('/role', function() {
	$role = new M\Role;
	unset($payload->movie);
	unset($payload->actor);
	$role->update_attributes((array) $payload);
});
$router->put('/role/*', function($id) {
	$role = M\Role::find($id);
	$role->update_attributes((array) $payload);
});
