Comb
====

Demo applications
=================
Both demo applications have only been tested in Google Chrome version 23.0.1271.97  
They should however be able to run in any webkit browser.

Movie Database Demo
-------------------
The Movie Database Demo is hosted as an apache vhost.  
The configuration for the vhost:
```
<VirtualHost *:80>
    ServerName moviedb
    DocumentRoot "/path/to/MovieDatabase/"
    LogLevel debug
    <Directory "/path/to/MovieDatabase">
        AllowOverride All
        Options +FollowSymLinks
        Order deny,allow
        Deny from all
        Allow from localhost
        
    </Directory>
</VirtualHost>
```

The MySQL database structure is located in `MovieDatabase/server/src/database.sql`.
Connection parameters are configured in `MovieDatabase/server/src/connect.php`.

Editor Demo
-----------
The Editor Demo uses yeoman. It has only been tested using the `yeoman server` command.  
Change into the Editor/ directory and run `yeoman server`, the a web browser should launch,
displaying the application.
