-module(filters).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
-export([strel/1, conv/2, gauss/1, process_image_with_mask/2,
	average/1, median/1, min/1, max/1, erode/1, dilate/1,
	open/1, close/1]).

%*************************************
% Module for contextual operations
%*************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns structural element
%% It's already normalized
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
	conv_row(Image, SE, 0, []).
conv_row(Image, SE, Row, Acc) ->

	ComputedRow = 
		[
			round(
				lists:sum(
					math:muliply_lists(utils:extract_neighbours_from_array(Image, Row, Col), lists:flatten(SE))
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
			conv_row(Image, SE, Row+1, lists:append(Acc, [ComputedRow]));
		true ->
			Acc
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executes convolution
%% Image matrix data have to be list 
%% of lists.
%% Significantly slower
%% #image -> array of arrays => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conv(Image, SE) ->
% 	conv_row(Image, SE, 0, []).
% conv_row(Image, SE, Row, Acc) ->
% 	ComputedRow = 
% 		[
% 			round(
% 				lists:sum(
% 					math:muliplyL(utils:extract_neighbours_from_list(Image, Row, Col), lists:flatten(SE))
% 				)
% 			)
% 			||
% 			Col <- lists:seq(1, utils:len(Image))
% 		],
% 	% io:format("~p", [ComputedRow]),
% 	Rows = utils:len(Image),
% 	if
% 		Row =<  Rows ->
% 			conv_row(Image, SE, Row+1, lists:append(Acc, [ComputedRow]));
% 		true ->
% 			Acc
% 	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% General function for concurrent image 
%% proc. with some kind of operation on mask
%% #image -> ([Num] -> Integer) => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_image_with_mask(Image, Fn) ->
	Height = Image#image.height,
	MasterPid = self(),
	% spawn supervisor
	Supervisor = 
		spawn(fun() -> process_image_with_mask_supervisor(MasterPid, Height, []) end),
	% spawn workers
	Workers = [
				spawn(fun() -> process_row_with_mask(Image, Row, Fn) end) 
				  || 
			    Row <- lists:seq(0, Height-1)
			  ],
	% send signal to start processing
	[Worker ! {Supervisor, start} || Worker <- Workers],
	receive
		{result, ImageMatrix} ->
			ImageMatrix;
		_ ->
			throw({result_lost, "Result not matched to receive"})
	end.
process_row_with_mask(Image, Row, Fn) ->
	Width = Image#image.width,
	receive
		{From, start} ->
			From ! {Row, [
						Fn(utils:extract_neighbours_from_array(Image, Row, Col))
						||
						Col <- lists:seq(0, Width-1)
					]};
		{From, _} ->
			From ! {erorr, "Wrong message. Try again if you wish."},
			process_row_with_mask(Image, Row, Fn);
		_ ->
			process_row_with_mask(Image, Row, Fn)
	end.
process_image_with_mask_supervisor(Parent, Remained, Acc) ->
	receive
		{RowNr, ComputedRow} when is_list(ComputedRow) ->
			case Remained of
				1 -> 
					Result = [{RowNr, ComputedRow}|Acc],
					{_, ImageMatrix} = lists:unzip(lists:sort(Result)),
					Parent ! {result, ImageMatrix},
					ok;
				_ ->
					% io:format("Recived row: ~p~n", [{RowNr, ComputedRow}]), 
					process_image_with_mask_supervisor(Parent, Remained-1, [{RowNr, ComputedRow}|Acc])
			end;
		_ -> 
			throw({bad_arg, "Argument should be a list"})
	end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with gaussian mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gauss(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = filters:conv(Image, filters:strel(gauss)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image with average mask
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
average(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = filters:conv(Image, filters:strel(average)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - midian filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
median(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = process_image_with_mask
		(
			Image, 
			fun(L) -> round(math:median(L)) end
		),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - min filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = process_image_with_mask
		(
			Image, 
			fun(L) -> round(lists:min(L)) end
		),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filters image - max filter
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = process_image_with_mask
		(
			Image, 
			fun(L) -> round(lists:max(L)) end
		),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erosion operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erode(ErlImg) ->
	min(ErlImg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dilatation operation
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dilate(ErlImg) ->
	max(ErlImg).

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

% TODO: 
% - uwspółbieżnić filtr uśredniający i gaussa