Master thesis of Anders Ingemann
================================

This is the repository of my master thesis, which I began in 2012 and ended february 2013.
The end-result is a parser for the [mustache templating language](http://mustache.github.io/)
and a javascript library which uses the output of that parser.

The parser outputs a JSON document that specifies locations of mustache variables and sections
in a template. This document is then used by the client to pinpoint those locations in the rendered
template via the DOM.  
This allows client-side apps to:

1. Rebuild the data that was used to render a mustache template on the server without any back-channel
2. Obtain references to DOM nodes that contain variable data without using any query selectors

Notable features
----------------
* The parser recognizes any html that is also a well-formed xml document (mustache variables as attribute names do not work)
* Mustache sections that act as loops are recognized by the client side library - the library will return an array
containing the data for each iteration (this also goes for nested loops).
* Data in comment nodes is also recognized
* Whitespace is significant, which means the client library will only return the true contents of `var` in
  a template like this: `<div>  {{var}}  </div>`
* The client library will not only return the data used to render the template, but also the nodes
  each piece of data is located in.

Downloads
---------------------
You can download my entire thesis as a tarball from S3: [s3-eu-west-1.amazonaws.com/ingemann/thesis-package.tar.gz](https://s3-eu-west-1.amazonaws.com/ingemann/thesis-package.tar.gz).
The PDFs of my thesis are in the `thesis/` folder
(available in 3 versions: [print](thesis/thesis.print.pdf), [PC](thesis/thesis.pc.pdf)
and [ebook](thesis/thesis.ebook.pdf)).

Disclaimer
----------
The library is only finished to the point where I was able present my results.
I have not touched it since and it requires quite a bit work to become production ready.
