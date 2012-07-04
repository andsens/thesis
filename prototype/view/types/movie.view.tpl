<li id="movie-<?php echo $id ?>">
	<details>
		<summary class="title"><?php echo $title; ?></summary>
		<header>
			<section class="operations">
				<span class="edit command">Edit</span>
				<a>Delete</a>
			</section>
		</header>
		<div class="main">
			<h1><?php echo $title; ?></h1>
			(<span class="year"><?php echo $year; ?></span>)
			<div class="poster">
				<?php echo $poster; ?>
			</div>
			<details class="plot">
				<summary class="synopsis">
						Synopsis: <span><?php echo $synopsis; ?></span>
					</summary>
				<p><?php echo $plot; ?></p>
			</details>
			<table class="cast">
				<thead>
					<tr>
						<td>Actor</td>
						<td>Character</td>
					</tr>
				</thead>
				<tbody>
					<?php foreach($cast as $role) {
						$actor = $role->actor;
						extract($role->to_array());
						require 'view/types/movie/role.tpl';
					} ?>
				</tbody>
			</table>
			<span id="add_actor_button" class="command">add</span>
		</div>
	</details>
</li>
