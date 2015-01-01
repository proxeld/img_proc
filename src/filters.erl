-module(filters).
-export([strel/1, conv/2, gauss/1, average/1]).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
%*************************************
% Module for contextual operations
%*************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns structural element
%% atom => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strel(Type) ->
	M = 
	case Type of
		gauss -> 	
			 [[1,2,1],
			 [2,4,2],
			 [1,2,1]];
		average ->
			 [[1,1,1],
			 [1,1,1],
			 [1,1,1]];
		_ -> throw({badarg, "There is now structural element of this type"})
	end,

	[[E/lists:sum(lists:flatten(M)) || E <- R] || R <- M].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executes convolution
%% [[Num]] -> [[Num]] => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conv(Image, SE) ->
	convrow(Image, SE, 1, []).

convrow(Image, SE, Row, Acc) ->
	ComputedRow = 
		[
			round(
				lists:sum(
					utils:muliplyL(extractNeighbours(Image, Row, Col), lists:flatten(SE))
				)
			)

			||

			Col <- lists:seq(1, utils:len(Image))
		],
	% io:format("~p", [ComputedRow]),
	Rows = utils:len(Image),
	if
		Row =<  Rows ->
			convrow(Image, SE, Row+1, lists:append(Acc, [ComputedRow]));
		true ->
			Acc
	end.
	

extractNeighbours(Image, Row, Col) -> 
	N =
	[
		try utils:getPixel(Image, Row+R, Col+C) of
			Val -> Val
		catch
			throw:{outofrange, _} -> 0
		end
		|| R <- [-1,0,1], C <- [-1,0,1]
	],
	% io:format("neighbourhood: ~p~n", [N]),
	N.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with gaussian mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gauss(ErlImg) ->
	Image = utils:erlImgToImage(ErlImg),
	Res = filters:conv(Image#image.matrix, filters:strel(gauss)),
	utils:synchronizeImg(ErlImg, Image#image{matrix=Res}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
average(ErlImg) ->
	Image = utils:erlImgToImage(ErlImg),
	Res = filters:conv(Image#image.matrix, filters:strel(average)),
	utils:synchronizeImg(ErlImg, Image#image{matrix=Res}).