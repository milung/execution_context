%% start your debugging session by loading 'debug' file. 

:- use_module(prolog/execution_cli).
:- use_module(prolog/execution_context).
:- ['tests/execution_context.plt'].
:- ['tests/execution_cli.plt'].

:- set_test_options([run(make)]).

:- current_prolog_flag(gui, true)
-> guitracer
; true.
