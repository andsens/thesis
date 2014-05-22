<?php
class EditMovie {
	
	private function __construct() {
		
	}
	
	public function getInstance() {
		static $self;
		if(!isset($self))
			$self = new self();
		return $self;
	}
	
	public function handleForm(array $vars) {
		if(empty($_GET['id'])) {
			if(empty($vars))
				return new Movie;
			$movie = new Movie($vars);
			$movie->save();
			return $movie;
		} else {
			$movie = Movie::find($_GET['id']);
			$movie->update_attributes($vars);
			$movie->save();
			return $movie;
		}
	}
}