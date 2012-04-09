<?php
require_once 'lib/php-activerecord/ActiveRecord.php';
ActiveRecord\Config::initialize(function($cfg)
{
    $cfg->set_model_directory(__DIR__.'/../model');
    $cfg->set_connections(
      array(
        'development' => 'mysql://movie-organizer:password@localhost/movie_organizer;unix_socket=/tmp/mysql.sock'
      )
    );
});
