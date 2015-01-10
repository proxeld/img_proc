-module(morfologic).
-export([erode/1, dilate/1, open/1, close/1, tophat/1, bothat/1]).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").

%*************************************
% Module for morfologic operations
%*************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erosion operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erode(ErlImg) ->
	filters:min(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dilatation operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dilate(ErlImg) ->
	filters:max(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Opening image operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open(ErlImg) ->
	dilate(erode(ErlImg)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Closing image operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close(ErlImg) ->
	erode(dilate(ErlImg)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local extrema (maximum) detection
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tophat(ErlImg) ->
	Opened = utils:erl_img_to_image(open(ErlImg)),
	Origin = utils:erl_img_to_image(ErlImg),
	OpenedLofL = utils:array_to_list_2D(Opened#image.matrix),
	OriginLofL = utils:array_to_list_2D(Origin#image.matrix),
	% Opened-Origin
	Res = utils:zipwith2D(fun(A,B)-> abs(A-B) end, OpenedLofL, OriginLofL),
	utils:synchronize_img(ErlImg, Origin#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local extrema (minimum) detection
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bothat(ErlImg) ->
	Closed = utils:erl_img_to_image(close(ErlImg)),
	Origin = utils:erl_img_to_image(ErlImg),
	ClosedLofL = utils:array_to_list_2D(Closed#image.matrix),
	OriginLofL = utils:array_to_list_2D(Origin#image.matrix),
	% Closed-Origin
	Res = utils:zipwith2D(fun(A,B)-> abs(A-B) end, ClosedLofL, OriginLofL),
	utils:synchronize_img(ErlImg, Origin#image{matrix=Res}).