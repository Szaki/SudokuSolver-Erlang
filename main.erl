-module(sudokusolver).
-author('Szaki').
-vsn('2019-11-25').
-export([sudoku/1]).
%-compile(export_all).

-type sspec() :: {size(), board()}.
-type size()  :: integer().
-type field() :: [info()].
-type info()  :: e | o | s | w | integer().
-type board() :: [[field()]].
-type ssol() :: [[integer()]].

-spec sudoku(Spec :: sspec()) -> Sols :: [ssol()].
sudoku(Spec) -> 
    {Size, _} = Spec,
    Iter = lists:seq(1, Size*Size),
    case isillegal(Spec, Iter) of
        true -> [];
        false -> 
            lists:map(
                fun clean/1,
                solutions({Size,
                    reduce({Size,
                            [[cellvals(Spec, Iter, Row, Col) || Col <- Iter] || Row <- Iter]
                        }, Iter)},Iter, 1, 1, [])
            )
    end.

-spec isillegal(Spec :: sspec(), Iter :: [integer()]) -> Res :: boolean().
isillegal(Spec, Iter) ->
    lists:any(fun({X, Y}) -> iscellillegal(Spec, X, Y) end,
                                [{A, B} || A <- Iter, B <- Iter]).

-spec iscellillegal(Spec :: sspec(), Row ::integer(), Col :: integer()) -> Res :: boolean().
iscellillegal(Spec, Row, Col) ->
    {_, Board} = Spec,
    Cell = cell(Board, Row, Col),
    CellNums = nums(Cell),
    Nums = length(CellNums),
    WCellNums = nums(cell(Board, Row, Col-1)),
    SCellNums = nums(cell(Board, Row+1, Col)),
    E = lists:member(e, Cell),
    O = lists:member(o, Cell),
    (Nums > 1) orelse
    (E andalso O) orelse
    (Nums =:= 1 andalso (
        (E andalso hd(CellNums) rem 2 =:= 1) orelse
        (O andalso hd(CellNums) rem 2 =:= 0) orelse
        (lists:member(w, Cell) andalso length(WCellNums) =:= 1
            andalso hd(WCellNums) rem 2 =:= hd(CellNums) rem 2) orelse
        (lists:member(s, Cell) andalso length(SCellNums) =:= 1
            andalso hd(SCellNums) rem 2 =:= hd(CellNums) rem 2))) orelse
    not isdistinct(colnums(Board, Col)) orelse
    not isdistinct(rownums(Board, Row)) orelse
    not isdistinct(subboardnums(Spec, Row, Col)).

-spec clean(Board :: board()) -> Formatted :: board().
clean(Board) ->
    [[hd(Field) || Field <- Row] || Row <- Board].

-spec solutions(Perms :: sspec(), Iter :: [integer()], Row :: integer(), Col :: integer(),
    Sols :: [board()]) -> Solutions :: [board()].
solutions(Perms, Iter, Row, Col, Sols) -> 
    {Size, Board} = Perms,
    Len = Size*Size,
    Cell = lists:nth(Col, lists:nth(Row, Board)),
    Nums = nums(Cell),
    case length(Nums) of
        0 -> Sols;
        1 ->
            if
                Col =:= Len andalso Row =:= Len ->
                    case (not isillegal(Perms, Iter) andalso not isdefective(Board)) of
                        true -> [Board|Sols];
                        false -> Sols
                    end;
                Col =:= Len -> solutions(Perms, Iter, Row+1, 1, Sols);
                true -> solutions(Perms, Iter, Row, Col+1, Sols)
            end;
        _ ->
            Adj = adjinfo(Cell),
            [H|T] = Nums,
            solutions(update(Perms, Iter, Row, Col, [H|Adj]), Iter, Row, Col, Sols) ++
            solutions(update(Perms, Iter, Row, Col, T ++ Adj), Iter, Row, Col, Sols)
    end.

-spec update(Spec :: sspec(), Iter ::[integer()],
    URow :: integer(), UCol :: integer(), Val :: field()) -> Updated :: sspec().
update(Spec, Iter, URow, UCol, Val) ->
    {Size, Board} = Spec,
    {Size, reduce({Size,
            [
                [updatecell(Board, Row, Col, URow, UCol, Val) || Col <- Iter]
            || Row <- Iter]},Iter)}.


-spec updatecell(Board :: board(), Row :: integer(), Col :: integer(),
    URow :: integer(), UCol :: integer(), Val :: field()) -> UCell :: field().
updatecell(Board, Row, Col, URow, UCol, Val) ->
    if
        Row =:= URow andalso Col =:= UCol -> Val;
        true -> lists:nth(Col, lists:nth(Row, Board))
    end.

-spec reduce(Spec :: sspec(), Iter :: [integer()]) -> Reduced :: board().
reduce(Spec, Iter) ->
    {Size, Board} = Spec,
    Reduced = [[reducecell(Spec, Row, Col) || Col <- Iter] || Row <- Iter],
    if
        Reduced =:= Board -> Reduced;
        true -> reduce({Size, Reduced}, Iter)
    end.

-spec reducecell(Spec :: sspec(), Row :: integer(), Col :: integer()) -> Reduced :: [info()].
reducecell(Spec, Row, Col) ->
    {Size, Board} = Spec,
    Cell = filtercell(Board, Row, Col, cell(Board, Row, Col)),
    CellNums = nums(Cell),
    L = length(CellNums),
    if
        L =:= 0 -> Cell --
                    usednums({Size,[[fixcellvals(nums(Y)) || Y <- X] || X <- Board]}, Row, Col);
        L =:= 1 -> Cell;
        true -> 
            RowNums = rownums(Board, Row) -- CellNums,
            RowUnique = [A || A <- CellNums,
                            not lists:member(A, RowNums)],
            if
                RowUnique =/= [] ->
                    RowUnique ++ adjinfo(Cell);
                true -> 
                    ColNums = colnums(Board, Col) -- CellNums,
                    ColUnique = [A || A <- CellNums,
                                    not lists:member(A, ColNums)],
                    if
                        ColUnique =/= [] ->
                            ColUnique ++ adjinfo(Cell);
                        true -> 
                            SubboardNums =
                                subboardnums(Spec, Row, Col) -- CellNums,
                            SubboardUnique =
                                [A || A <- CellNums,
                                    not lists:member(A, SubboardNums)],
                            if
                                SubboardUnique =/= [] -> SubboardUnique ++ adjinfo(Cell);
                                true -> Cell -- usednums(
                                        {Size, [[fixcellvals(nums(Y)) || Y <- X] || X <- Board]}, Row, Col)
                            end
                    end
            end
    end.

-spec cellvals(Spec :: sspec(), Iter :: [integer()], Row :: integer(), Col :: integer()) ->
    Vals :: [info()].
cellvals(Spec, Iter, Row, Col) ->
    {_, Board} = Spec,
    Cell = cell(Board, Row, Col),
    CellNums = nums(Cell),
    Even = lists:member(e, Cell),
    Odd = lists:member(o, Cell),
    if
        length(CellNums) =:= 1 -> [hd(CellNums)|adjinfo(Cell)];
        true ->
            Used = usednums(Spec, Row, Col),
            if
                Even -> [A || A <- Iter, A rem 2 =:= 0, not lists:member(A, Used)] ++ adjinfo(Cell);
                Odd -> [A || A <- Iter, A rem 2 =:= 1, not lists:member(A, Used)] ++ adjinfo(Cell);
                true -> [A || A <- Iter, not lists:member(A, Used)] ++ adjinfo(Cell)
            end
    end.

-spec fixcellvals(Nums :: [integer()]) -> Vals :: [integer()].
fixcellvals(Nums) ->
    if
        length(Nums) =:= 1 -> Nums;
        true -> []
    end.

-spec isdefective(Board :: board()) -> Res :: boolean().
isdefective(Board) ->
    lists:any(fun(X) -> nums(X) =:= [] end, [Field || Row <- Board, Field <- Row]).

-spec filtercell(Board :: board(),  Row ::integer(), Col :: integer(), Cell :: field()) ->
    Filtered :: field().
filtercell(Board, Row, Col, Cell) ->
    E = parity(cell(Board, Row, Col+1), w),
    N = parity(cell(Board, Row-1, Col), s),
    if
        E =:= 0 andalso N =:= 0 -> Cell;
        (E =:= 2 andalso N =/= 1) orelse (N =:= 2 andalso E =/= 1) ->
                [A || A <- Cell, not is_integer(A) orelse A rem 2 =:= 1];
        (E =:= 1 andalso N =/= 2) orelse (N =:= 1 andalso E =/= 2) ->
                [A || A <- Cell, not is_integer(A) orelse A rem 2 =:= 0];
        true -> []
    end.

-spec parity(Field :: field(), Type :: info()) -> Parity :: integer().
parity(Field, Type) ->
    case lists:member(Type, Field) of
        true ->
            Nums = nums(Field),
            if
                Nums =:= [] -> 0;
                true ->
                    Even = lists:all(fun(N) -> N rem 2 =:= 0 end, Nums),
                    Odd = lists:all(fun(N) -> N rem 2 =:= 1 end, Nums),
                    if
                        Even -> 2;
                        Odd -> 1;
                        true -> 0
                    end
            end;
        false -> 0
    end.


-spec usednums(Spec :: sspec(), Row :: integer(), Col :: integer()) -> Nums :: [integer()].
usednums(Spec, Row, Col) ->
    {_, Board} = Spec,
    lists:append([rownums(Board, Row), colnums(Board, Col), subboardnums(Spec, Row, Col)]).

-spec colnums(Board :: board(), Col :: integer()) -> Nums :: [integer()].
colnums(Board, Col) ->
    [A || A <- lists:flatten([lists:nth(Col, A) || A <- Board]), is_integer(A)].

-spec rownums(Board :: board(), Row :: integer()) -> Nums :: [integer()].
rownums(Board, Row) ->
    [A || A <- lists:flatten(lists:nth(Row, Board)), is_integer(A)].

-spec subboardnums(Spec :: sspec(), Row :: integer(), Col :: integer()) -> Nums :: [integer()].
subboardnums(Spec, Row, Col) ->
    {Size, Board} = Spec,
    [A || A <- lists:flatten(lists:map(fun(Q) ->
        lists:sublist(Q, (Col-1) div Size*Size+1, Size) end,
        lists:sublist(Board, (Row-1) div Size*Size+1, Size))), is_integer(A)].

-spec adjinfo(Field :: field()) -> Info :: [info()].
adjinfo(Field) ->
    [A || A <- Field, A =:= s orelse A =:= w].

-spec nums(Field :: field()) -> Nums :: [integer()].
nums(Field) ->
    [A || A <- Field, is_integer(A)].

-spec isdistinct(X :: [integer()]) -> B :: boolean().
isdistinct(X) ->
    not lists:any(fun(Q)-> lists:member(Q, lists:delete(Q, X)) end, X).

-spec cell(Board :: board(), Row::integer(), Col :: integer()) -> Field :: field().
cell(Board, Row, Col) ->
    Len = length(Board),
    if
        (Row < 1 orelse Row > Len) orelse (Col < 1 orelse Col > Len) -> [];
        true -> lists:nth(Col, lists:nth(Row, Board))
    end.
