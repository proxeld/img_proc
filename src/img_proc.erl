-module(img_proc).
-export([load/1, save/1, save/2, test/0, filterGauss/1, filterAverage/1]).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
%**************************************
% Main module with image processing API
%**************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Loads image by given path
%% [Char] => {atom, #erl_image}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(Path) ->
	erl_img:load(Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saves image 
%% Uses the same path it was read from
%% #erl_image => atom.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save(ErlImg) ->
	erl_img:save(ErlImg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saves image according with 
%% a given path 
%% #erl_image -> [Char] => atom.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save(ErlImg, Path) ->
	NewImg = ErlImg#erl_image{filename=Path},
	erl_img:save(NewImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with gaussian mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterGauss(ErlImg) ->
	filters:gauss(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterAverage(ErlImg) ->
	filters:average(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Used for testing purpose
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
	{_, ErlImg} = load("priv/lenaSzum256.png"),
	Lena = filterAverage(ErlImg),
	save(Lena, "./tmp_lena.png").