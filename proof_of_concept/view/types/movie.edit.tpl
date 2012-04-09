<form action="/movies/<?php echo $id; ?>" method="post">
	<fieldset>
		<legend>Release information</legend>
		<ol>
			<li>
				<label for="title">Movie title</label>
				<input type="text" id="title" name="title" value="<?php echo $title; ?>" />
			</li>
			<li>
				<label for="year">Year</label>
				<input type="number" id="year" name="year" value="<?php echo $year; ?>" minlength="4" maxlength="4" />
			</li>
			<li>
				<label for="tagline">Tagline</label>
				<input type="text" id="tagline" name="tagline" value="<?php echo $tagline; ?>" />
			</li>
			<li>
				<label for="poster">Poster</label>
				<input type="file" id="poster" name="poster" value="<?php echo $poster; ?>" />
			</li>
	</fieldset>
	<fieldset>
		<legend>Plot</legend>
		<ol>
			<li>
				<label for="synopsis">Synopsis</label>
				<textarea id="synopsis" name="synopsis"><?php echo $synopsis; ?></textarea>
			</li>
			<li>
				<label for="plot">Plot</label>
				<textarea id="plot" name="plot"><?php echo $plot; ?></textarea>
			</li>
		</ol>
	</fieldset>
	<fieldset>
		<legend>Actions</legend>
		<input type="submit" />
		<input type="reset" />
	</fieldset>
</form>