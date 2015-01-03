-module(math).
-export([avg/1, muliply_lists/2, muliply_list_of_lists/2, even/1, odd/1, median/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Average of values from list
%% [Num] => Num
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
avg(List) ->
	lists:sum(List)/lists:flatlength(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multiplies two lists
%% [Num] -> [Num] => [Num]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
muliply_lists(V1, V2) ->
	lists:zipwith(fun(A,B)->A*B end, V1, V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Multiplies two matrixes of the same
%% size
%% [[Num]] -> [[Num]] => [[Num]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
muliply_list_of_lists(M1, M2) ->
	muliplyh(M1, M2, []).
muliplyh([], [], Acc) -> 
	Acc;
muliplyh([H1|T1], [H2|T2], Acc) ->
	% io:format("Row1: ~p~nRow2: ~p~n", [H1, H2]),
	NewAcc = lists:append(Acc, [muliply_lists(H1,H2)]),
	muliplyh(T1,T2, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks if number is even
%% Num => boolean
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
even(Num) when is_integer(Num) ->
    if
        Num band 1 == 0 -> true;
        true            -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks if number is odd
%% Num => boolean
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
odd(Num) when is_integer(Num) ->
	not even(Num).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Median of values from list
%% [Num] => Num
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
median(List) when is_list(List) ->
    SortedList = lists:sort(List),
    Length = length(SortedList),
    case even(Length) of
        true -> [A,B] = lists:sublist(SortedList, round(Length/2), 2), (A+B)/2;
        false -> lists:nth( round((Length+1)/2), SortedList )
    end.