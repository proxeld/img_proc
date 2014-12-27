-module(img_proc).
-compile([export_all]).
% include records definition
-include_lib("erl_img/include/erl_img.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Loads image by given path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(Path) ->
	erl_img:load(Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saves image 
%  Uses the same path it was read from
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save(Img) ->
	erl_img:save(Img).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saves image according with 
%  a given path 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save(Img, Path) ->
	NewImg = Img#erl_image{filename=Path},
	erl_img:save(NewImg).