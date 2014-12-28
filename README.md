Getting started
===============

#####Download dependencies

	make update

#####Compile project

	make compile
or simply 

	make
 
#####Run shell with loaded libraries

	make start

#####Loading image (in erlang shell)

	img_proc:load("priv/lena.png").

#####Saving image
	img_proc:save(Img, "path/to/new/img.png").
	
#####Filtering
	img_proc:filterGauss(Img).

TIPS
----

#####Pretty record displaying enabling (in erlang shell)

	rr(img_proc).
#####Unbound variable

	f(A).

KNOWN BUGS
----------

Erlang shell (at first) do not detect app module for autocompliting. 
