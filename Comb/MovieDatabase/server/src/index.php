<?php
use \MovieDatabase\Model as M;
require 'init.inc.php';

$mustache = new Mustache_Engine;

$index_tpl = file_get_contents('index.mustache');
$movies = M\Movie::find('all');

print($mustache->render($index_tpl, array('movies' => $movies)));
