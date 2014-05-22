<?php
namespace MovieDatabase\Model;
class Role extends \ActiveRecord\Model {
	static $table_name = 'cast';
	static $belongs_to = array(
		array('movie'),
		array('actor')
	);
	# Hack to get activerecord to return true when asked if the property is set... ffs
	static $alias_attribute = array(
     'actor' => 'actor');
}
