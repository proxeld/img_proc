-module(utils).
-export([getPixel/3, len/1, avg/1, muliplyM/2, muliplyL/2, 
	erlImgToImage/1, synchronizeImg/2, print/1]).
-include("deps/erl_img/include/erl_img.hrl").
-include("img_proc.hrl").
%*************************************
% Module with useful functions
%*************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extract one element from the list
%% of lists
%% [[X]] -> Integer -> Integer => X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getPixel(Matrix, Row, Column) ->
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
%% Length of the list
%% [X] => Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
len(L) -> len(L,0).
len([], Acc) -> Acc;
len([_|T], Acc) -> len(T,Acc+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Average of values from list
%% [Num] => Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
avg(List) ->
	lists:sum(List)/lists:flatlength(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multiplies two matrixes of the same
%% size
%% [[Num]] -> [[Num]] => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
muliplyM(M1, M2) ->
	muliplyh(M1, M2, []).
muliplyh([], [], Acc) -> 
	Acc;
muliplyh([H1|T1], [H2|T2], Acc) ->
	% io:format("Row1: ~p~nRow2: ~p~n", [H1, H2]),
	NewAcc = lists:append(Acc, [muliplyL(H1,H2)]),
	muliplyh(T1,T2, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multiplies two lists
%% [Num] -> [Num] => [Num]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
muliplyL(V1, V2) ->
	lists:zipwith(fun(A,B)->A*B end, V1, V2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts #erl_image to record #image
%% (see img_proc.hrl)
%% #erl_image => #image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erlImgToImage(Img) ->
	Pixmap = lists:nth(1, Img#erl_image.pixmaps),
	Pixels = Pixmap#erl_pixmap.pixels,

	Mx = case Img#erl_image.format of
		gray8 ->
			[binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels ];
		r8g8b8 ->
			Matrix = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels],
			[rowToRGB(Row) || Row <- Matrix];
		r8g8b8a8 ->
			Matrix = [binary:bin_to_list(Bajts) || {_, Bajts} <- Pixels],
			[rowToRGBA(Row) || Row <- Matrix];
		_ ->
			throw({error, "Image format not supported", Img#erl_image.format})
	end,

	#image{format=Img#erl_image.format,
		width=Img#erl_image.width,
		height=Img#erl_image.height,
		matrix=Mx}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Synchronizes #image with #erl_image
%% (see img_proc.hrl)
%% #erl_image #image => #erl_image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
synchronizeImg(ErlImg, Img) ->

	Pxs = case Img#image.format of
		gray8 ->
			Rows = lists:zip(
						lists:seq(0,ErlImg#erl_image.height-1),
						Img#image.matrix
					),
			[{Nr, binary:list_to_bin(Row)} || {Nr, Row} <- Rows];
		r8g8b8 ->
			todo;
		r8g8b8a8 ->
			todo;
		_ ->
			throw({error, "Image format not supported", Img#erl_image.format})
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