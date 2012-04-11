<?php
class Actor extends ActiveRecord\Model {
	static $has_many = array(
		array('roles'),
		array('movies', 'through' => 'roles')
	);
}
