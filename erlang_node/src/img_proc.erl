-module(img_proc).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
-define(OUT_DIR, "out/").
-export([load/1, save/1, save/2,
		filterGauss/1, filterAverage/1, filterMedian/1, filterMin/1, filterMax/1,
		erode/1, dilate/1, open/1, close/1, tophat/1, bothat/1, prewitt/1, roberts/1, sobel/1,
		test/0, testMenu/0, test_time/0, get/1]).

%**************************************
% Main module with image processing API
%**************************************

get(Data)->
	{_, E} = load("priv/g4.png"),
	Pxmap = (lists:nth(1, E#erl_image.pixmaps))#erl_pixmap{pixels = [Data]},
	NewE = E#erl_image{pixmaps = [Pxmap]},
	Res = morfologic:tophat(NewE),
	save(Res, "/home/proxeld/n.png").

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
	filters:average(ErlImg).

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
%% Prewitt gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prewitt(ErlImg) ->
	filters:prewitt(ErlImg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Roberts gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roberts(ErlImg) ->
	filters:roberts(ErlImg).		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sobel gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sobel(ErlImg) ->
	filters:sobel(ErlImg).		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Used for testing purpose
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
	{_, ErlImg} = load("priv/jet.png"),
	Gauss = filterGauss(ErlImg),
	Avg = filterAverage(ErlImg),
	Med = filterMedian(ErlImg),
	Dil = dilate(ErlImg),
	Erode = erode(ErlImg),
	Open = open(ErlImg),
	Close = close(ErlImg),
	Tophat = tophat(ErlImg),
	Bothat = bothat(ErlImg),
	Prewitt = prewitt(ErlImg),
	Sobel = sobel(ErlImg),
	save(Gauss, ?OUT_DIR ++ "gauss.png"),
	save(Avg, ?OUT_DIR ++ "avg.png"),
	save(Med, ?OUT_DIR ++ "med.png"),
	save(Dil, ?OUT_DIR ++ "dil.png"),
	save(Erode, ?OUT_DIR ++ "erode.png"),
	save(Open, ?OUT_DIR ++ "open.png"),
	save(Close, ?OUT_DIR ++ "close.png"),
	save(Tophat, ?OUT_DIR ++ "tophat.png"),
	save(Bothat, ?OUT_DIR ++ "bothat.png"),
	save(Tophat, ?OUT_DIR ++ "tophat.png"),
	save(Prewitt, ?OUT_DIR ++ "prewitt.png"),
	save(Sobel, ?OUT_DIR ++ "sobel.png").

testMenu() ->
	io:format("\e[J"),
	io:format("-------------------------------------------~n"),
	io:format("-        IMG PROCESSING IN ERLANG         -~n"),
	io:format("-------------------------------------------~n"),
	io:format("-       Enter path to png image file      -~n"),
	io:format("- (or leave blank to use Lena as default) -~n"),
	io:format("-------------------------------------------~n"),
	ImgPath = io:get_line("Image> "),
	{_, ErlImg} = testImgOpen(ImgPath),
	io:format("-------------------------------------------~n"),
	io:format("-       Select operation to perform       -~n"),
	io:format("-------------------------------------------~n"),
	io:format("-                FILTERS                  -~n"),
	io:format("-------------------------------------------~n"),
	io:format("- 1) Gaussian                             -~n"),
	io:format("- 2) Mean                                 -~n"),
	io:format("- 3) Median                               -~n"),
	io:format("-------------------------------------------~n"),
	io:format("-           MORPHOLOGY-BASED              -~n"),
	io:format("-------------------------------------------~n"),
	io:format("- 4) Erosion                              -~n"),
	io:format("- 5) Dilation                             -~n"),
	io:format("- 6) Opening                              -~n"),
	io:format("- 7) Closing                              -~n"),
	io:format("- 8) Top-Hat transformation               -~n"),
	io:format("- 9) Bottom-Hat transformation            -~n"),
	io:format("- 10) Prewitt gradient                    -~n"),
	io:format("- 11) Roberts gradient                    -~n"),
	io:format("- 12) Sobel gradient                    -~n"),
	io:format("-------------------------------------------~n"),
	Operation = io:get_line("Number> "),
	case Operation of
		"1\n" -> 
			io:format("-           Gaussian filter               -~n"),
			Gauss = filterGauss(ErlImg),
			save(Gauss, ?OUT_DIR ++ "gauss.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "gauss.png"]);

		"2\n" -> 
			io:format("-             Mean filter                 -~n"),
			Avg = filterAverage(ErlImg),
			save(Avg, ?OUT_DIR ++ "mean.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "mean.png"]);

		"3\n" -> 
			io:format("-            Median filter                -~n"),
			Med = filterMedian(ErlImg),
			save(Med, ?OUT_DIR ++ "median.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "median.png"]);

		"4\n" -> 
			io:format("-                Erosion                  -~n"),
			Erode = erode(ErlImg),
			save(Erode, ?OUT_DIR ++ "erosion.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "erosion.png"]);

		"5\n" -> 
			io:format("-                Dilation                 -~n"),
			Dil = dilate(ErlImg),
			save(Dil, ?OUT_DIR ++ "dilation.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "dilation.png"]);

		"6\n" -> 
			io:format("-                Opening                  -~n"),
			Open = open(ErlImg),
			save(Open, ?OUT_DIR ++ "opening.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "opening.png"]);

		"7\n" -> 
			io:format("-                Closing                  -~n"),
			Close = close(ErlImg),
			save(Close, ?OUT_DIR ++ "closing.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "closing.png"]);

		"8\n" -> 
			io:format("-        Top-Hat transformation           -~n"),
			Tophat = tophat(ErlImg),
			save(Tophat, ?OUT_DIR ++ "tophat.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "tophat.png"]);

		"9\n" -> 
			io:format("-      Bottom-Hat transformation          -~n"),
			Bothat = bothat(ErlImg),
			save(Bothat, ?OUT_DIR ++ "bothat.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "bothat.png"]);

		"10\n" -> 
			io:format("-          Prewitt gradfient              -~n"),
			Prewitt = prewitt(ErlImg),
			save(Prewitt, ?OUT_DIR ++ "prewitt.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "prewitt.png"]);

		"11\n" -> 
			io:format("-          Roberts gradient               -~n"),
			Roberts = roberts(ErlImg),
			save(Roberts, ?OUT_DIR ++ "roberts.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "roberts.png"]);

		"12\n" -> 
			io:format("-          Roberts gradient               -~n"),
			Sobel = sobel(ErlImg),
			save(Sobel, ?OUT_DIR ++ "sobel.png"),
			io:format("Output file saved to: ~s !~n", [?OUT_DIR ++ "sobel.png"]);
		
		_ -> io:format("-         Wrong operation!               -~n")
	end,
	testMenu().

testImgOpen(ImgPath) ->
	Path = lists:sublist(ImgPath, 1, utils:len(ImgPath)-1),
	io:format("Path: ~s~n", [Path]),
	case Path of
		"" -> load("priv/lena.png");
		_  -> load(Path)
	end.

test_time() -> 
	{_, ErlImg} = load("priv/lenaSzum256.png"),
	Img = utils:erl_img_to_image(ErlImg),
	io:format("--------------~n"),
	time:avg(utils, get_px, [Img, 255, 255], 512*512),
	io:format("~n--------------~n").