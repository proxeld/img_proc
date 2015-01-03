-module(utils).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
-export([get_pixel_from_list2D/3, get_pixel_from_array2D/3, len/1, array_len/1, erl_img_to_image/1,
	synchronize_img/2, merge/2, bin_to_array/1, every_snd/1, print/1,
	extract_neighbours_from_array/3, extract_neighbours_from_list/3, 
	list_to_array_2D/1]).

%*************************************
% Module with useful functions
%*************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract one element from the list
%% of lists
%% May be faster - pass size of matrix
%% in arguments
%% [[X]] -> Integer -> Integer => X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_pixel_from_list2D(Matrix, Row, Column) ->
	Height = len(Matrix),
	Line =
	if 
		Row > Height orelse Row < 1 -> 
			throw({outofrange, "Index is out of range"});
		true -> 
			lists:nth(Row, Matrix)
	end,

	Width = len(Line),
	if 
		Column > Width orelse Column < 1 ->
			throw({outofrange, "Index is out of range"});
		true ->
			lists:nth(Column, Line)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract one element from the 
%% two-dimentional array
%% #image -> Integer -> Integer => X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_pixel_from_array2D(Image, Row, Column) ->
	Width = Image#image.width,
	Height = Image#image.height,
	Array2D = Image#image.matrix,

	if
		Row >= Height orelse Row < 0 orelse Column >= Width orelse Column < 0 ->
			throw({outofrange, "Index is out of range"});
		true ->	
			array:get(Column, array:get(Row, Array2D))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Length of the list
%% [X] => Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
len(L) -> len(L,0).
len([], Acc) -> Acc;
len([_|T], Acc) -> len(T,Acc+1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Length of the array
%% WARNING: Slows down computations
%% array => Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
array_len(Array) -> 
	length(array:to_list(Array)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Merges two lists taking head from
%% first list, head from second and 
%% snd element from frist, snd element
%% from snd list and so on
%% [X] -> [X] => [X]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge([], Rest) ->
	Rest;
merge(Rest, []) ->
	Rest;
merge([H1|T1], [H2|T2]) ->
	[H1,H2|merge(T1,T2)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts binary to array
%% <<X/bajt,T/binary>> => array of X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bin_to_array(Binary) ->
	bin_to_array(Binary, 0, array:new()).
bin_to_array(<<>>, _, Acc) ->
	Acc;
bin_to_array(<<H,T/binary>>, Idx, Acc) ->
	NewAcc = array:set(Idx, H, Acc),
	bin_to_array(T, Idx+1, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts list to array
%% [X] => array of X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_to_array(List) ->
	list_to_array(List, 0, array:new()).
list_to_array([], _, Acc) ->
	Acc;
list_to_array([H|T], Idx, Acc) ->
	NewAcc = array:set(Idx, H, Acc),
	list_to_array(T, Idx+1, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts list of lists to array 
%% of arrays
%% [[X]] => array of arrays of X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_to_array_2D(LofL) ->
	list_to_array([list_to_array(List) || List <- LofL]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Takes every second element from list
%% starting with first
%% [X] => [X]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
every_snd([]) ->
	[];
every_snd([H]) ->
	[H];
every_snd([H1,_|T]) ->
	[H1] ++ every_snd(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract 9 neighbours from image
%% given certain position.
%% Image matrix data have to be array 
%% of arrays
%% #image -> Num -> Num => [Num]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_neighbours_from_array(Image, Row, Col) -> 
	% io:format("Extracting neighbourhood for row ~p, col ~p~n", [Row, Col]),
	N =
	[
		% get_pixel_from_array2D method is critical here - it has to be fast!
		try utils:get_pixel_from_array2D(Image, Row+R, Col+C) of			
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract 9 neighbours from image
%% given certain position.
%% Image matrix data have to be list 
%% of lists
%% #image -> Num -> Num => [Num]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_neighbours_from_list(Image, Row, Col) -> 
	N =
	[
		try utils:get_pixel_from_list2D(Image, Row+R, Col+C) of
			Val -> Val
		catch
			throw:{outofrange, _} -> 0
		end
		|| R <- [-1,0,1], C <- [-1,0,1]
	],
	% io:format("neighbourhood: ~p~n", [N]),
	N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts #erl_image to record #image
%% (see img_proc.hrl)
%% #erl_image => #image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erl_img_to_image(Img) ->
	Pixmap = lists:nth(1, Img#erl_image.pixmaps),
	Pixels = Pixmap#erl_pixmap.pixels,

	Mx = case Img#erl_image.format of
		gray8 ->
			% list of lists: 
			% L = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels ],
			L = [bin_to_array(Bajts) || {_, Bajts} <- Pixels ],
			Alpha = [],
			list_to_array(L);
		gray8a8 ->
			% just ignore transparency
			LofL = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels ],
			LofA = [list_to_array(every_snd(List)) || List <- LofL],
			Alpha = [every_snd(T) || [_|T] <- LofL],
			list_to_array(LofA);
		r8g8b8 ->
			% list of lists: 
			Matrix = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels],
			LofL = [rowToRGB(Row) || Row <- Matrix],
			LofA = [list_to_array(List) || List <- LofL],
			Alpha = [],
			list_to_array(LofA);
		r8g8b8a8 ->
			Matrix = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels],
			LofL = [rowToRGBA(Row) || Row <- Matrix],
			LofA = [list_to_array(List) || List <- LofL],
			Alpha = [],
			list_to_array(LofA);
		_ ->
			Alpha = [],
			throw({error, "Image format not supported", Img#erl_image.format})
	end,

	#image{format=Img#erl_image.format,
		width=Img#erl_image.width,
		height=Img#erl_image.height,
		alpha=Alpha,
		matrix=Mx}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Synchronizes #image with #erl_image
%% (see img_proc.hrl)
%% #erl_image -> #image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
synchronize_img(ErlImg, Img) ->

	Pxs = case Img#image.format of
		gray8 ->
			Rows = lists:zip(
						lists:seq(0,ErlImg#erl_image.height-1),
						Img#image.matrix
					),
			[{Nr, binary:list_to_bin(Row)} || {Nr, Row} <- Rows];
		gray8a8 ->
			% merge image matrix with transparency channel
			Rows = lists:zip(
						lists:seq(0,ErlImg#erl_image.height-1),
						Img#image.matrix
					),
			WithAlpha = 
				[{Nr, merge(Row, lists:nth(Nr+1, Img#image.alpha))} 
					|| {Nr, Row} <- Rows],
			[{Nr, binary:list_to_bin(Row)} || {Nr, Row} <- WithAlpha];
		r8g8b8 ->
			todo;
		r8g8b8a8 ->
			todo;
		_ ->
			throw({error, "Image format not supported", Img#image.format})
	end,
	%io:format("Pixmap to synchronize:~p~n", [Pxs]),
	Pixmap = lists:nth(1, ErlImg#erl_image.pixmaps),
	ErlImg#erl_image{
		format=Img#image.format,
		width=Img#image.width,
		height=Img#image.height,
		pixmaps=[Pixmap#erl_pixmap{pixels=Pxs}]
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conversts list to list of tuples of
%  three [] -> [{R,G,B}]
%  List size must be dividable by 3
%  [X] => [{X,X,X}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rowToRGB([R,G,B]) ->
	[{R,G,B}];
rowToRGB([R,G,B|T]) ->
	[{R,G,B}|rowToRGB(T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conversts list to list of tuples of
%  three [] -> [{R,G,B,A}]
%  List size must be dividable by 4
%  [X] => [{X,X,X,X}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rowToRGBA([R,G,B,A]) ->
	[{R,G,B,A}];
rowToRGBA([R,G,B,A|T]) ->
	[{R,G,B,A}|rowToRGBA(T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Printing image matrix
%% Img - #erl_image record
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print(Img) ->
	Pixmaps = Img#erl_image.pixmaps,
	printh(Pixmaps).
printh([]) ->
	done;
printh([P|T]) ->
	Pixels = P#erl_pixmap.pixels,
	[print_row(Bajts) || {_, Bajts} <- Pixels],	
	printh(T).
print_row(Row) ->
	[io:format("~B, ", [Content]) || <<Content>> <= Row],
	io:format("~n").