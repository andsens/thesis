<?php
require 'vendor/autoload.php';
$mustache = new Mustache_Engine(array(
	'loader' => new Mustache_Loader_FilesystemLoader(__DIR__)
));
$tpl = $mustache->loadTemplate('templates/profile.mustache');
echo $tpl->render(array('name' => 'World!'));
