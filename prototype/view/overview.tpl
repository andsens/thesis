<!DOCTYPE HTML>
<html>
<head>
	<?php
		include 'view/head.tpl';
	?>
</head>
<body id="overview">
	<header>
		<?php
			extract((array) $header);
			include 'view/types/header.tpl';
		?>
	</header>
	<section id="movies">
		<ol>
			<?php foreach($movies as $movie) {
				$cast = $movie->cast;
				extract($movie->to_array());
				require 'view/types/movie.view.tpl';
			} ?>
		</ol>
		<span id="add_movie_button" class="command">add</span>
	</section>
	<footer></footer>
</body>
</html>