<?php
use \MovieDatabase\Model as M;
require 'init.inc.php';

$payload = json_decode(file_get_contents('php://input'));

$router = new \Respect\Rest\Router('/rest');

$router->post('/movies', function() use ($payload) {
	$movie = new M\Movie;
	$movie->update_attributes((array) $payload);
});
$router->put('/movies/*', function($id) use ($payload) {
	$movie = M\Movie::find($id);
	unset($payload->cast);
	$movie->update_attributes((array) $payload);
	return json_encode($movie->to_array());
});

$router->post('/actors', function() use ($payload) {
	$actor = new M\Actor;
	$actor->update_attributes((array) $payload);
});
$router->put('/actors/*', function($id) use ($payload) {
	$actor = M\Actor::find($id);
	$actor->update_attributes((array) $payload);
});

$router->post('/roles', function() use ($payload) {
	$role = new M\Role;
	unset($payload->movie);
	unset($payload->actor);
	$role->update_attributes((array) $payload);
});
$router->put('/roles/*', function($id) use ($payload) {
	$role = M\Role::find($id);
	$role->update_attributes((array) $payload);
});
