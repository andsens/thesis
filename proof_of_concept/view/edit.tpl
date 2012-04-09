<!DOCTYPE HTML>
<html>
<head>
	<?php
		include 'view/head.tpl';
	?>
</head>
<body id="new">
	<header>
		<?php
			extract((array) $header);
			include 'view/types/header.tpl';
		?>
	</header>
	<section id="movie-form">
		<?php
			extract($movie->to_array());
			include 'view/types/movie.edit.tpl';
		?>
	</section>
	<footer></footer>
</body>
</html>