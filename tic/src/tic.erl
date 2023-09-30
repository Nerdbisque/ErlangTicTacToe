-module(tic).
-export([start/0, display_board/1, make_move/3, game_over/1]).

% Custom exception for invalid moves
-define(INVALID_MOVE, {error, "Invalid move. Try again."}).

% Start the game
start() ->
    io:format("Welcome to Tic Tac Toe!~n"),
    play_game(["1", "2", "3", "4", "5", "6", "7", "8", "9"], "X").

% Display the Tic Tac Toe board
display_board(Board) ->
    io:format("~n ~s | ~s | ~s ~n", [lists:nth(1, Board), lists:nth(2, Board), lists:nth(3, Board)]),
    io:format("-----------~n"),
    io:format(" ~s | ~s | ~s ~n", [lists:nth(4, Board), lists:nth(5, Board), lists:nth(6, Board)]),
    io:format("-----------~n"),
    io:format(" ~s | ~s | ~s ~n~n", [lists:nth(7, Board), lists:nth(8, Board), lists:nth(9, Board)]).

% Make a move on the board
make_move(Board, Position, Player) ->
    case {Position >= 1 andalso Position =< 9, lists:nth(Position, Board) =:= integer_to_list(Position)} of
        {true, true} ->
            NewBoard = lists:sublist(Board, Position - 1) ++ [Player] ++ lists:nthtail(Position, Board),
            {ok, NewBoard};
        {false, _} ->
            ?INVALID_MOVE;
        {_, false} ->
            ?INVALID_MOVE
    end.

% Check if the game is over
game_over(Board) ->
    case lists:filter(fun(X) -> X =:= "X" end, Board) of
        ["X", "X", "X"] -> "X";
        _ -> case lists:filter(fun(O) -> O =:= "O" end, Board) of
            ["O", "O", "O"] -> "O";
            _ -> if lists:member(" ", Board) -> "Continue";
                    true -> "Draw"
                 end
        end
    end.

% Play the game recursively
play_game(Board, Player) ->
    display_board(Board),
    case game_over(Board) of
        "X" ->
            io:format("Player X wins!~n");
        "O" ->
            io:format("Player O wins!~n");
        "Draw" ->
            io:format("It's a draw!~n");
        "Continue" ->
            io:format("Player ~s's turn. Enter your move (1-9): ", [Player]),
            Move = read_move(),
            case make_move(Board, Move, Player) of
                {ok, NewBoard} ->
                    play_game(NewBoard, if Player == "X" -> "O"; true -> "X" end);
                ?INVALID_MOVE ->
                    io:format("Invalid move. Try again.~n"),
                    play_game(Board, Player)
            end
    end.

% Read player's move from the console
read_move() ->
    {ok, [Move]} = io:fread("~d", ""),
    Move.
