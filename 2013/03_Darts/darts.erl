% Erlang. Compile with erlc and execute.

% Compile:
%   erlc darts.erl
% Run:
%   erl -noshell -s darts start -s init stop


-module(darts).
-export([start/0]).

-import(string).

start() ->
  io:format("Dart optimizer for lazy dart people~n"),
  {Score, _} = string:to_integer(io:get_line("Enter current score: ")),
  {First,  FirstModifier}  = find_first_dart(Score), 
  {Second, SecondModifier} = find_second_dart(Score - 
                                 calculate_score(First,  FirstModifier)), 
  {Third,  ThirdModifier}  = find_third_dart(Score - 
                                 calculate_score(First,  FirstModifier) - 
                                 calculate_score(Second, SecondModifier)),
  RemainingScore = Score - 
                       calculate_score(First,  FirstModifier) - 
                       calculate_score(Second, SecondModifier) -
                       calculate_score(Third,  ThirdModifier),

  io:format("First Dart: ~s~n",  [format(First,  FirstModifier)]),
  io:format("Second Dart: ~s~n", [format(Second, SecondModifier)]),
  io:format("Third Dart: ~s~n",  [format(Third,  ThirdModifier)]),
  io:format("Remaining Score: ~w~n", [RemainingScore]).

% First dart always evens us up if we are odd.
find_first_dart(Score) ->
  case Score rem 2 of
    0 -> find_second_dart(Score);
    1 -> if
           Score =< 101 -> {1, 1};     % We are nearing the end. Even it up and let the rest handle it.
           true -> {19, 3}             % So very far away. Kill it as much as we can while evening out.
         end
  end.

find_second_dart(Score) ->
  if
    Score =< 40 -> { 0, 1};            % We are within range of doubling out. Ruin this dart.
    Score =< 60 -> {10, 2};            % We can put the next throw within range. Go for it!
    true -> {20, 3}                    % So very far away. Kill it as much as we can.
  end.

find_third_dart(Score) ->
  if
    Score =< 40 -> {Score div 2, 2};  % We can double out easily
    Score == 50 -> {50, 1};           % We can out on the bullseye
    Score =< 60 -> {10, 2};           % We are close but not close to out. Get us closer w/o messing it up.
    true -> {20, 3}                   % So very far away. Kill it as much as we can.
  end.

format(Area, Multiplier) ->
  string:concat(
    case Multiplier of
      1 -> "";
      2 -> "Double ";
      3 -> "Triple "
    end,

    if
      Area == 0 -> "Wall";
      Area == 25 -> "Bull";
      Area == 50 -> "Bullseye";
      true -> integer_to_list(Area)
    end).

calculate_score(Area, Multiplier) -> Area * Multiplier.
