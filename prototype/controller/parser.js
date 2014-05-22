var parser = {
	getViewModel: function(model, view, context) {
		console.log(context);
		var attributes = {};
		var evaluator = new XPathEvaluator();
		_.each(view.finders, function(finder, attr) {
			var xpath = finder.xpath;
			var attribute;
			var sub_context;
			var sub_model;
			console.log(model.mname+' '+attr+' '+model.getAttrType(attr));
			switch(model.getAttrType(attr)) {
				case 'collection':
					attribute = new (model.getComplexType(attr))();
					sub_model = attribute.model;
					var nodes = evaluator.evaluate(xpath, context, null, XPathResult.ANY_TYPE, null);
					if(finder.process)
						nodes = finder.process(nodes);
					blahrg = nodes;
					while ((sub_context = nodes.iterateNext()) !== null) {
						var sub_instance = parser.getViewModel(sub_model, finder.view(), sub_context);
						attribute.push(sub_instance.model);
					}
					break;
				case 'model':
					sub_model = model.getComplexType(attr);
					var node = evaluator.evaluate(xpath, context, null, XPathResult.ANY_TYPE, null);
					if(finder.process)
						node = finder.process(node);
					if ((sub_context = node.iterateNext()) !== null) {
						attribute = parser.getViewModel(sub_model, finder.view(), sub_context).model;
					}
					break;
				case 'node':
					attribute = evaluator.evaluate(xpath, context, null, XPathResult.ANY_TYPE, null).iterateNext();
					if(finder.process)
						attribute = finder.process(attribute);
					break;
				default:
					attribute = evaluator.evaluate(xpath, context, null, XPathResult.STRING_TYPE, null).stringValue;
					if(finder.process)
						attribute = finder.process(attribute);
					break;
			}
			attributes[attr] = attribute;
		});
		var instance = new model(attributes);
		var view_instance	= new view({model: instance, el: context});
		console.log('out '+model.mname);
		return view_instance;
	}
};