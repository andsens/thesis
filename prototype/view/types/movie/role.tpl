<tr id="id-<?php echo $id; ?>">
		<td class="actor"><?php
		$roles = $actor->roles;
		extract($actor->to_array());
		require 'view/types/movie/role/actor.tpl';
		?></td>
		<td class="character"><?php echo $character; ?></td>
</tr>