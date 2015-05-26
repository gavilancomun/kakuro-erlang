-module(kakuro).
-export([main/0, drop/2, partition_by/2, partition_all/2, partition_all/3, permute_all/2, v/0, v/1, draw/1]).

e() -> 
  {empty}.

a(Across) ->
  {across, Across}.

d(Down) ->
  {down, Down}.

da(Down, Across) ->
  {down_across, Down, Across}.

v() ->
  {value, sets:from_list([1,2,3,4,5,6,7,8,9])}.

v(Values) ->
  {value, sets:from_list(Values)}.

draw_v(Values, X) ->
  case sets:is_element(X, Values) of
    true -> integer_to_list(X);
    _ -> "."
  end.

draw({empty}) -> "   -----  ";
draw({down, N}) -> io_lib:format("   ~2B\\--  ", [N]);
draw({across, N}) -> io_lib:format("   --\\~2B  ", [N]);
draw({down_across, D, A}) -> io_lib:format("   ~2B\\~2B  ", [D, A]);
draw({value, Values}) -> 
  case sets:size(Values) == 1 of
    true -> string:join(["     " ++ integer_to_list(X) ++ "    " || X <- sets:to_list(Values)], "");
    _ -> " " ++ string:join([ draw_v(Values, X) || X <- lists:seq(1, 9)], "")
  end.

draw_row(Row) ->
  string:join([draw(X) || X <- Row], "") ++ "~n".

draw_grid(Grid) ->
  "~n" ++ string:join([draw_row(X) || X <- Grid], "").

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

drop(_, []) -> [];
drop(N, [_ | T]) when N =< 1 -> T;
drop(N, [_ | T]) -> drop((N - 1), T).

partition_by(_, []) -> [];
partition_by(F, [H|T]) ->
    FH = F(H),
    Run = lists:takewhile(fun(Y) -> FH == F(Y) end, [H|T]),
    [Run | partition_by(F, drop(length(Run), [H|T]))].

partition_all(_, _, []) -> [];
partition_all(N, Step, [H|T]) ->
  [lists:sublist([H|T], 1, N) | partition_all(N, Step, drop(Step, [H|T]))].

partition_all(N, Coll) -> partition_all(N, N, Coll).

all_different(Nums) ->  length(Nums) == length(sets:to_list(sets:from_list(Nums))).

permute(VS, Target, SoFar) ->
  if
    Target >= 1 ->
      if
        (length(SoFar) == length(VS) - 1) -> [ lists:append(SoFar, [Target]) ];
        true -> case lists:nth(1 + length(SoFar), VS) of
          {value, Values} -> lists:flatmap(fun(V) -> permute(VS, (Target - V), lists:append(SoFar, [V])) end, sets:to_list(Values));
          _ -> []
        end
      end;
    true -> []
  end.

permute_all(VS, Total) -> permute(VS, Total, []).

is_possible({value, Values}, N) -> sets:is_element(N, Values);
is_possible(_, _) -> false.

solve_step(Cells, Total) -> 
     Final = length(Cells),
     lists:map(fun(P) -> v(P) end, 
     transpose(
     lists:filter(fun all_different/1, 
     lists:filter(fun(P) -> is_possible(lists:nth(Final, Cells), lists:nth(Final, P)) end, permute_all(Cells, Total))))).

row_target({across, N}) -> N;
row_target({down_across, _, A}) -> A;
row_target(_) -> 0.

col_target({down, N}) -> N;
col_target({down_across, D, _}) -> D;
col_target(_) -> 0.

solve_pair(_, [NVS]) -> NVS;
solve_pair(F, [NVS, VS|_]) -> lists:append(NVS, solve_step(VS, F(lists:last(NVS))));
solve_pair(_, _) -> [].

solve_pair_row(X) -> solve_pair(fun row_target/1, X).

solve_pair_col(X) -> solve_pair(fun col_target/1, X).

is_value({value, _}) -> true;
is_value(_) -> false.

solve_line(F, Cells) ->
  lists:flatmap(F, partition_all(2, partition_by(fun is_value/1, Cells))).

solve_row(Cells) -> solve_line(fun solve_pair_row/1, Cells).

solve_col(Cells) -> solve_line(fun solve_pair_col/1, Cells).

solve_grid(Grid) ->
  transpose(
  lists:map(fun solve_col/1,
  transpose(
  lists:map(fun solve_row/1, Grid)))).

solver(Grid) -> 
  G = solve_grid(Grid),
  if
    G == Grid -> Grid;
    true ->
      io:format(draw_grid(G)),
      solver(G)
  end.

grid1() ->
  [[e(), d(4), d(22), e(), d(16), d(3)],
   [a(3), v(), v(), da(16, 6), v(), v()],
   [a(18), v(), v(), v(), v(), v()],
   [e(), da(17, 23), v(), v(), v(), d(14)],
   [a(9), v(), v(), a(6), v(), v()],
   [a(15), v(), v(), a(12), v(), v()]].

main() -> 
  io:format(draw_grid(grid1())),
  solver(grid1()),
  ok.
