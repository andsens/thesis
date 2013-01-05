<?php
namespace MovieDatabase\Model;
var_dump(class_exists('\MovieDatabase\Model\Movie', false));

class Movie extends \ActiveRecord\Model {
	static $has_many = array(
		array('roles'),
		array('actors', 'through' => 'roles')
	);
	static $alias_attribute = array(
     'cast' => 'roles');
}
var_dump('after');
