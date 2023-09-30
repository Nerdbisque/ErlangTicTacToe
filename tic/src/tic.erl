-module(tic).
-compile(export_all).

% Tic Tac Toe game state
-record(game_state, {a1, a2, a3, b1, b2, b3, c1, c2, c3}).

% Player record
-record(player, {pid, name}).

% Possible game results
-define(tie, tie).
-define(x, x).
-define(o, o).
-define(undecided, undecided).
-define(continue, continue).

% Start the Tic Tac Toe game
start(PlayerOne, PlayerTwo) ->
    InitialGame = #game_state{a1=a1, a2=a2, a3=a3, b1=b1, b2=b2, b3=b3, c1=c1, c2=c2, c3=c3},
    spawn(tictactoe, loop, [PlayerOne, PlayerTwo, PlayerOne, InitialGame]).

% Make a move in the Tic Tac Toe game
make_move(GamePid, Player, Move) ->
    GamePid ! {make_move, Player, Move}.

% Game loop
loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState) ->
    {CurrentPid, CurrentName} = CurrentPlayer,
    {PlayerOnePid, PlayerOneName} = PlayerOne,
    {PlayerTwoPid, PlayerTwoName} = PlayerTwo,
    receive
        {make_move, CurrentName, Move} ->
            ValidMove = is_valid_move(Move, GameState),
            case ValidMove of
                true ->
                    CurrentPlayerAtom = player_atom(PlayerOne, PlayerTwo, CurrentPlayer),
                    UpdateFun = fun(Pos) ->
                        value_if_match(Pos, Move, CurrentPlayerAtom)
                    end,
                    NewGameState = update_game_state(GameState, UpdateFun),
                    NewCurrentPlayer = change_player(PlayerOne, PlayerTwo, CurrentPlayer),
                    io:format("State: ~p -> ~p~n", [GameState, NewGameState]),
                    case get_game_result(NewGameState) of
                        ?tie ->
                            gameclient:game_tie(PlayerOnePid, PlayerTwoName, NewGameState),
                            gameclient:game_tie(PlayerTwoPid, PlayerOneName, NewGameState);
                        ?x ->
                            gameclient:send_message(PlayerOnePid, get_win_message(PlayerTwoName)),
                            gameclient:send_message(PlayerTwoPid, get_loose_message(PlayerOneName));
                        ?o ->
                            gameclient:send_message(PlayerTwoPid, get_win_message(PlayerOneName)),
                            gameclient:send_message(PlayerOnePid, get_loose_message(PlayerTwoName));
                        ?continue ->
                            loop(PlayerOne, PlayerTwo, NewCurrentPlayer, NewGameState)
                    end;
                false ->
                    gameclient:send_message(CurrentPid, "Position " ++ erlang:atom_to_list(Move) ++ " is not available."),
                    loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState)
            end;

        {make_move, WrongPlayerName, _} ->
            WrongPlayerPid = get_pid_for_player_name(PlayerOne, PlayerTwo, WrongPlayerName),
            gameclient:send_message(WrongPlayerPid, "It is not your turn yet!"),
            loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState);

        Message ->
            io:format("PlayerOne: ~p~n PlayerTwo: ~p~n CurrentPlayer: ~p~n Message: ~p~n", [PlayerOne, PlayerTwo, CurrentPlayer, Message]),
            loop(PlayerOne, PlayerTwo, CurrentPlayer, GameState)
    end.

% Get the win message
get_win_message(Over) ->
    "You won over " ++ Over ++ " :)".

% Get the loose message
get_loose_message(For) ->
    "You lost over " ++ For ++ "... :(".

% Get the game result
get_game_result(GameState) ->
    case check_for_winner(GameState) of
        ?undecided ->
            GameOver = lists:all(fun(X) -> (X =:= ?x) or (X =:= ?o) end, game_state_to_list(GameState)),
            if
                GameOver ->
                    ?tie;
                true ->
                    ?continue
            end;
        X -> X
    end.

% Check for a winner in the Tic Tac Toe game
check_for_winner(GameState) ->
    Rows = [[a1, a2, a3], [b1, b2, b3], [c1, c2, c3]],
    Columns = [[a1, b1, c1], [a2, b2, c2], [a3, b3, c3]],
    Diagonals = [[a1, b2, c3], [a3, b2, c1]],
    get_winner(Rows ++ Columns ++ Diagonals).

% Get the winner in the Tic Tac Toe game
get_winner([]) -> ?undecided;
get_winner([[X, X, X] | _]) -> X;
get_winner([_ | T]) -> get_winner(T).

% Update the game state
update_game_state(GameState, UpdateFun) ->
    #game_state{a1=UpdateFun(a1), a2=UpdateFun(a2), a3=UpdateFun(a3),
                b1=UpdateFun(b1), b2=UpdateFun(b2), b3=UpdateFun(b3),
                c1=UpdateFun(c1), c2=UpdateFun(c2), c3=UpdateFun(c3)}.

% Check if a move is valid
is_valid_move(Move, GameState) ->
    lists:member(Move, game_state_to_list(GameState)).

% Convert game state to a list
game_state_to_list(GameState) ->
    [GameState#a1, GameState#a2, GameState#a3, GameState#b1, GameState#b2, GameState#b3,
     GameState#c1, GameState#c2, GameState#c3].

% Value if match
value_if_match(X, X, NewValue) -> NewValue;
value_if_match(X, _, _) -> X.

% Get the player atom
player_atom(X, _, X) -> ?x;
player_atom(_, Y, Y) -> ?o.

% Change the current player
change_player(PlayerOne, PlayerTwo, PlayerOne) -> PlayerTwo;
change_player(PlayerOne, PlayerTwo, PlayerTwo) -> PlayerOne.

% Get the PID for a player name
get_pid_for_player_name({PlayerOnePid, WrongPlayerName}, _, WrongPlayerName) -> PlayerOnePid;
get_pid_for_player_name(_, {PlayerTwoPid, WrongPlayerName}, WrongPlayerName) -> PlayerTwoPid.
