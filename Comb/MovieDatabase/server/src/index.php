<?php
use \MovieDatabase\Model as M;
require 'init.inc.php';

$template_type = "";
if(isset($_GET['template'])) {
	$template_type = $_GET['template'];
}

switch($template_type) {
	case 'bootstrap': $tpl_type = 'bootstrap-'; break;
	case 'no-bootstrap': $tpl_type = 'no-bootstrap-'; break;
	default: $tpl_type = 'no-tpl_type-'; break;
}

$movies = M\Movie::find('all');
$mustache = new Mustache_Engine(array(
	'cache' => __DIR__."/{$tpl_type}templates/cache",
	'loader' => new Mustache_Loader_FilesystemLoader(__DIR__."/{$tpl_type}templates"),
	'partials_loader' => new Mustache_Loader_FilesystemLoader(__DIR__."/{$tpl_type}templates")
));

print($mustache->render('index', array('movies' => $movies)));
