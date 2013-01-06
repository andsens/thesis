<?php
use \MovieDatabase\Model as M;
require 'init.inc.php';


$movies = M\Movie::find('all');
$mustache = new Mustache_Engine(array(
	'cache' => __DIR__.'/templates/cache',
	'loader' => new Mustache_Loader_FilesystemLoader(__DIR__.'/templates'),
	'partials_loader' => new Mustache_Loader_FilesystemLoader(__DIR__.'/templates')
));

$index_tpl = file_get_contents('templates/index.mustache');
// foreach($movies as $movie) {
// 	echo '<pre>';
// 	$role = $movie->cast[0];
// 	$id = 'actor';
// 	var_dump(isset($role->$id));
// 	var_dump($role->$id);
//   if (is_object($role)) {
//       if (method_exists($role, $id)) {
//           echo $role->$id();
//       } elseif (isset($role->$id)) {
//           echo $role->$id;
//       }
//   } elseif (is_array($role) && array_key_exists($id, $role)) {
//       echo $role[$id];
//   }
// 	print_r($movie->cast[0]->actor->name);
// 	die();
// }
// echo '<pre>';
print($mustache->render('index', array('movies' => $movies)));
