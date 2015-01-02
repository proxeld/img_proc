-module(filters).
-export([strel/1, conv/2, gauss/1, average/1, mean/1, meanFl/1, meanConcurrent/1, meanConcurrentRow/2]).
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
%% #image -> array of arrays => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conv(Image, SE) ->
	convrow(Image, SE, 0, []).

convrow(Image, SE, Row, Acc) ->

	ComputedRow = 
		[
			round(
				lists:sum(
					math:muliplyL(extractNeighbours(Image, Row, Col), lists:flatten(SE))
				)
			)

			||

			Col <- lists:seq(0, Image#image.width-1)
		],
	% io:format("~p~n", [array:to_list(Array2D)]),
	% io:format("~p~n", [ComputedRow]),
	Rows = Image#image.height,
	if
		Row <  Rows ->
			convrow(Image, SE, Row+1, lists:append(Acc, [ComputedRow]));
		true ->
			Acc
	end.

%% For the version with lists, not arrays
% convrow(Image, SE, Row, Acc) ->
% 	ComputedRow = 
% 		[
% 			round(
% 				lists:sum(
% 					math:muliplyL(extractNeighbours(Image, Row, Col), lists:flatten(SE))
% 				)
% 			)
% 			||
% 			Col <- lists:seq(1, utils:len(Image))
% 		],
% 	% io:format("~p", [ComputedRow]),
% 	Rows = utils:len(Image),
% 	if
% 		Row =<  Rows ->
% 			convrow(Image, SE, Row+1, lists:append(Acc, [ComputedRow]));
% 		true ->
% 			Acc
% 	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract 9 neighbours from image
%% given certain position
%% #image -> Num -> Num => [Num]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extractNeighbours(Image, Row, Col) -> 
	% io:format("Extracting neighbourhood for row ~p, col ~p~n", [Row, Col]),
	N =
	[
		% getPx method is critical here - it has to be fast!
		try utils:getPx(Image, Row+R, Col+C) of			
			Val ->
				% io:format("~p ~p~n", [Row+R, Col+C]),
				Val
		catch
			throw:{outofrange, _} -> 
				% io:format("Exception catched ~p ~p~n", [Row+R, Col+C]),
				% zeros out of boumdaries
				0
		end
		|| R <- [-1,0,1], C <- [-1,0,1]
	],
	% io:format("neighbourhood: ~p~n", [N]),
	N.	

%% For the version with lists, not arrays
% extractNeighbours(Image, Row, Col) -> 
% 	N =
% 	[
% 		try utils:getPixel(Image, Row+R, Col+C) of
% 			Val -> Val
% 		catch
% 			throw:{outofrange, _} -> 0
% 		end
% 		|| R <- [-1,0,1], C <- [-1,0,1]
% 	],
% 	% io:format("neighbourhood: ~p~n", [N]),
% 	N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mean filtering logic
%% #image => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meanFl(Image) ->
	meanRow(Image, 0, []).
meanRow(Image, Row, Acc) ->
	ComputedRow = 
		[
			round(
				math:median(extractNeighbours(Image, Row, Col))
			)

			||

			Col <- lists:seq(0, Image#image.width-1)
		],
	Rows = Image#image.height,
	if
		Row <  Rows ->
			meanRow(Image, Row+1, lists:append(Acc, [ComputedRow]));
		true ->
			Acc
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mean concurrent filtering logic
%% #image => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meanConcurrent(Image) ->
	Workers = [spawn(?MODULE, meanConcurrentRow, [Image, Row]) 
				|| 
			   Row <- lists:seq(0, Image#image.height-1)],
	% spawn workers
	[Worker ! {self(), start} || Worker <- Workers],
	% here we have in Rows computed data, but not sorted
	Rows = meanSupervisor(Image#image.height, []),
	Sorted = lists:sort(Rows),
	{_, ImageData} = lists:unzip(Sorted),
	ImageData.
	% io:format("After concurrent computing: ~p~n", [ImageData]).
meanConcurrentRow(Image, Row) ->
	receive
		{From, start} ->
			From ! {Row, [
						round(
							math:median(extractNeighbours(Image, Row, Col))
						)
						||
						Col <- lists:seq(0, Image#image.width-1)
					]};
		{From, _} ->
			From ! {erorr, "Wrong message. Try again if you wish."},
			meanConcurrentRow(Image, Row);
		_ ->
			meanConcurrentRow(Image, Row)
	end.
meanSupervisor(Remained, Acc) ->
	receive
		{RowNr, ComputedRow} when is_list(ComputedRow) ->
			case Remained of
				1 -> [{RowNr, ComputedRow}|Acc];
				_ ->
					% io:format("Recived row: ~p~n", [{RowNr, ComputedRow}]), 
					meanSupervisor(Remained-1, [{RowNr, ComputedRow}|Acc])
			end;
		_ -> 
			throw({bad_arg, "Argument should be a list"})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with gaussian mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gauss(ErlImg) ->
	Image = utils:erlImgToImage(ErlImg),
	Res = filters:conv(Image, filters:strel(gauss)),
	utils:synchronizeImg(ErlImg, Image#image{matrix=Res}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
average(ErlImg) ->
	Image = utils:erlImgToImage(ErlImg),
	Res = filters:conv(Image, filters:strel(average)),
	utils:synchronizeImg(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mean(ErlImg) ->
	Image = utils:erlImgToImage(ErlImg),
	Res = meanConcurrent(Image),
	utils:synchronizeImg(ErlImg, Image#image{matrix=Res}).