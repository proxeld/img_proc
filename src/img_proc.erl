-module(img_proc).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
-define(OUT_DIR, "out/").
-export([load/1, save/1, save/2,
		filterGauss/1, filterAverage/1, filterMedian/1, filterMin/1, filterMax/1,
		erode/1, dilate/1, open/1, close/1, tophat/1, bothat/1,
		test/0, test_time/0, vars/0]).

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
	filters:gaussian(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterAverage(ErlImg) ->
	filters:average_filter(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - median filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterMedian(ErlImg) ->
	filters:median(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - min filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterMin(ErlImg) ->
	filters:min(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - median filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterMax(ErlImg) ->
	filters:max(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Opens image
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erode(ErlImg) ->
	morfologic:erode(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Opens image
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dilate(ErlImg) ->
	morfologic:dilate(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Opens image
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open(ErlImg) ->
	morfologic:open(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Closes image
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close(ErlImg) ->
	morfologic:close(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local extrema (maximum) detection
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tophat(ErlImg) ->
	morfologic:tophat(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local extrema (minimum) detection
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bothat(ErlImg) ->
	morfologic:bothat(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Used for testing purpose
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
	{_, ErlImg} = load("priv/lena.png"),
	Gauss = filterGauss(ErlImg),
	Avg = filterAverage(ErlImg),
	Med = filterMedian(ErlImg),
	Dil = dilate(ErlImg),
	Erode = erode(ErlImg),
	Open = open(ErlImg),
	Close = close(ErlImg),
	Tophat = tophat(ErlImg),
	Bothat = bothat(ErlImg),
	save(Gauss, ?OUT_DIR ++ "gauss.png"),
	save(Avg, ?OUT_DIR ++ "avg.png"),
	save(Med, ?OUT_DIR ++ "med.png"),
	save(Dil, ?OUT_DIR ++ "dil.png"),
	save(Erode, ?OUT_DIR ++ "erode.png"),
	save(Open, ?OUT_DIR ++ "open.png"),
	save(Close, ?OUT_DIR ++ "close.png"),
	save(Tophat, ?OUT_DIR ++ "tophat.png"),
	save(Bothat, ?OUT_DIR ++ "bothat.png").

test_time() -> 
	{_, ErlImg} = load("priv/lenaSzum256.png"),
	Img = utils:erl_img_to_image(ErlImg),
	io:format("--------------~n"),
	time:avg(utils, get_px, [Img, 255, 255], 512*512),
	io:format("~n--------------~n").

vars() ->
	{_, E} = load("priv/g4.png"),
	I = utils:erl_img_to_image(E),
	{E, I}.