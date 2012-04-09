// less.watch();
$(function() {
	$('section#movies li>details').click(function() {
		$(_.without($('section#movies li>details'), this)).prop('open', false);
	});
});
$(function() {
	root = parser.getViewModel(Root, RootView, document);
	console.log(root);
});