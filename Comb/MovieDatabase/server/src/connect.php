<?php
\ActiveRecord\Config::initialize(function($cfg)
{
    $cfg->set_model_directory(__DIR__.'/MovieDatabase/model');
    $cfg->set_connections(
      array(
        'development' => 'mysql://moviedb:password@localhost/moviedb;unix_socket=/tmp/mysql.sock'
      )
    );
});
