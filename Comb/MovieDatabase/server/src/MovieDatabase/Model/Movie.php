<?php
namespace MovieDatabase\Model;

class Movie extends \ActiveRecord\Model {
	static $has_many = array(
		array('roles'),
		array('actors', 'through' => 'roles')
	);
	static $alias_attribute = array(
     'cast' => 'roles');
}
