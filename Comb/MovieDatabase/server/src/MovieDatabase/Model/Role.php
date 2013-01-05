<?php
namespace MovieDatabase\Model;
class Role extends \ActiveRecord\Model {
	static $table_name = 'cast';
	static $belongs_to = array(
		array('movie'),
		array('actor')
	);
}
