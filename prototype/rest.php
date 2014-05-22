<?php
require_once 'controller/connect.php';
require_once 'model/Movie.class.php';
require_once 'model/Actor.class.php';
require_once 'model/Role.class.php';

echo handle_request($_SERVER['REQUEST_METHOD'], $_SERVER['REQUEST_URI'], file_get_contents('php://input'));
function handle_request($method, $uri, $payload) {
	preg_match('#^/([a-z]+)(/([0-9]+))?$#', $uri, $matches);
	if(!empty($payload)) {
		if($_SERVER['CONTENT_TYPE'] == 'application/json')
			$payload = json_decode($payload);
		else {
			trigger_error('Can only understand json.', E_USER_ERROR);
		}
	} else {
		$payload = null;
	}
	
	$model_name = ActiveRecord\classify($matches[1], true);
	switch($method) {
		case 'POST':
			return post($model_name, $payload);
		case 'GET':
			$id = $matches[3];
			return get($model_name, $id);
		case 'PUT':
			$id = $matches[3];
			return put($model_name, $id, $payload);
		case 'DELETE':
			$id = $matches[3];
			return delete($model_name, $id);
	}
}

function post($model_name, $payload) {
	switch($model_name) {
		case 'Movie':
			$model = new Movie();
			break;
		case 'Actor':
			$model = new Actor();
			break;
		case 'Role':
			$model = new Role();
			unset($payload->movie);
			unset($payload->actor);
			break;
	}
	$model->update_attributes((array) $payload);
	return $model->to_json();
}

function get($model_name, $id) {
	
}

function put($model_name, $id, $payload) {
	$model = get_model($model_name, $id);
	unset($payload->cast);
	$model->update_attributes((array) $payload);
	return $model->to_json();
}

function delete($model_name, $id) {
	
}

function get_model($model_name, $id) {
	switch($model_name) {
		case 'Movie':
			return Movie::find($id);
		case 'Actor':
			return Actor::find($id);
	}
	return null;
}