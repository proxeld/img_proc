Image Processing in Erlang
==========================

Project created for university subject (which involves concurrent and distributed programming).
According to <a href="http://learnyousomeerlang.com/">this</a> great book "Erlang is no silver bullet and will be particularly bad at things like image and signal processing...". 
Taking into account previous sentence: this project is not intended to be used. It's more for education and testing purpose.

###Dependencies
<a href="https://github.com/evanmiller/erl_img">erl_img</a> is used for reading and writing images. 

Getting started
===============

Download dependencies

	make update

Compile project

	make compile
or simply 

	make
	
Testing API

	make test
	
Run shell with loaded libraries

	make start

Loading image (in erlang shell)

	img_proc:load("priv/lena.png").

Saving image

	img_proc:save(Img, "path/to/new/img.png").
	
Other API functions

	img_proc:filterGauss(Img).
	img_proc:filterAverage(Img).
	img_proc:filterMeadian(Img).
	img_proc:filterMin(Img).
	img_proc:filterMax(Img).
	img_proc:erode(Img).
	img_proc:dilate(Img).
	img_proc:open(Img).
	img_proc:close(Img).
	

TIPS
----

Pretty record displaying enabling (in erlang shell)

	rr(img_proc).
	
Unbounding variable in erlang shell

	f(A).

KNOWN BUGS
----------

Erlang shell (at first) do not detect app module for autocompliting. 
