<nav>
	<ul>
		<?php foreach($menu as $item) {
			extract((array) $item);
			?><li><a href="<?php echo $href ?>"><?php echo $title; ?></a></li><?php
		} ?>
	</ul>
</nav>