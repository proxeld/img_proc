-module(filters).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
-export([strel/1, gradMask/1, conv/2, gradient/2, gaussian/1, process_image_with_mask/2,
	average/1, median/1, min/1, max/1, prewitt/1, roberts/1, sobel/1]).


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
%% Returns mask for gradient
%% atom => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gradMask(Type) ->
	case Type of
		prewitt ->
			 [[-1/3,-1/3,-1/3],
			 [0,0,0],
			 [1/3,1/3,1/3]];
		roberts ->
			 [[0,0,0],
			 [-1,0,0],
			 [0,1,0]];
		sobel ->
			[[-1/4,0,1/4],
			[-2/4,0,2/4],
			[-1/4,0,1/4]];
		_ -> throw({badarg, "There is now gradient mask of this type"})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executes gradient filtering
%% #image -> array of arrays => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gradient(Image, SE) ->
	process_image_with_mask
		(
			Image, 
			fun(L) -> round(abs(lists:sum(math:muliply_lists(L, lists:flatten(SE))))) end
		).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Executes convolution
%% #image -> array of arrays => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conv(Image, SE) ->
	process_image_with_mask
		(
			Image, 
			fun(L) -> round(lists:sum(math:muliply_lists(L, lists:flatten(SE)))) end
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% General function for concurrent image 
%% proc. with some kind of operation on mask
%% #image -> ([Num] -> Integer) => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_image_with_mask(Image, Fn) ->
	utils:supported_or_exception(Image#image.format),
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
%% Gaussian
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gaussian(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = conv(Image, strel(gauss)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Average
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
average(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = conv(Image, strel(average)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prewitt gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prewitt(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = gradient(Image, gradMask(prewitt)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Roberts gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roberts(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = gradient(Image, gradMask(roberts)),
	utils:synchronize_img(ErlImg, Image#image{matrix=Res}).		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sobel gradient
%% #erl_image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sobel(ErlImg) ->
	Image = utils:erl_img_to_image(ErlImg),
	Res = gradient(Image, gradMask(sobel)),
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