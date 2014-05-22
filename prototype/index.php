<?php
try {
	require_once 'controller/connect.php';
	require_once 'model/Movie.class.php';
	require_once 'model/Actor.class.php';
	require_once 'model/Role.class.php';
	require_once 'controller/EditMovie.class.php';
	{
		$header = new stdClass;
		$header->menu = array();
		$overview = new stdClass;
		$overview->title = 'Overview';
		$overview->href = '/';
		$new = new stdClass;
		$new->title = 'New movie';
		$new->href = '/movies';
		$header->menu[] = $overview;
		$header->menu[] = $new;
	}
	$show = null;
	if(array_key_exists('show', $_GET))
		$show = $_GET['show'];
	switch($show) {
		case 'edit':
			$editMovieCtrl = EditMovie::getInstance();
			$movie = $editMovieCtrl->handleForm($_POST);
			include 'view/edit.tpl';
			break;
		default:
			$movies = Movie::find('all');
			include 'view/overview.tpl';
	}
} catch(Exception $e) {
	echo "<pre>{$e->getMessage()}</pre>";
}