:- begin_tests(execution_cli).
:- use_module('../prolog/execution_cli').
:- use_module(library(yall)).

:- encoding(utf8).

% this tests assumes the prolog was invoked with following command line arguments: 
%    prolog debug.pl -- --cli-flag --no-negated -f --variable-long-a="long A" --flag-ex=false -s short --long long --module-default=module
%
% The environment variable EC_TEST must be set to 'my variable', and environment variable  ec_module_setting must be set to 'setting'.

:- execution_context:context_variable(
    cli_tests:t_flag, 
    bool, 
    [
        long('t-flag'), 
        is_flag(true), 
        cli_command([]),
        describe('Some very interesting flag')  
    ]).
:- execution_context:context_variable(
    cli_tests:g_short_flag, 
    atom, 
    [
        short(g), 
        is_flag(true), 
        cli_command([]),
        describe('Any short flag') 
    ]).
:- execution_context:context_variable(
    cli_tests:longex, 
    atom, 
    [
        long('long-ex'), 
        cli_command([test_a, test_b]),
        describe('Some long command to execute')
    ]).

:- execution_cli:register_cli_command(
    test_a, 
    [A, B] >> write_term(response(A,B), []), 
    [describe('Simple command with no options')]).

test('simple command', [ ]) :-    
    % given
    Arguments = ['--t-flag', test_a, something, '--long-ex', 'assigned'],
    % when
    with_output_to( atom(Response), execution_cli:execute_cli(Arguments) ),
    % then
    read_term_from_atom(Response, response(Positional, Options), []),
    Positional == [something], Options == [].
        
test('simple command', []) :-    
    % given
    Arguments = ['--t-flag', test_a, something, '--long-ex', 'assigned'],
    % when
    with_output_to(atom(Response), execution_cli:execute_cli(Arguments) ),
    % then
    read_term_from_atom(Response, response(Positional, Options), []),
    Positional == [something], Options == [].

test('unknown option', [throws(cli_option(unknown, _))]) :-    
    % given
    Arguments = ['--t-flag', test_a, something, '--unknown', 'assigned'],
    % when
    with_output_to(atom(_), execution_cli:execute_cli(Arguments) ).

:- execution_cli:register_cli_command(
    test_b, [A, B] >> write_term(response(A,B), []), 
    [
        context(cli_tests:t_flag, my_option),
        describe('Lorem ipsum dorem sulpur natrum getrdum faber norda dru xaver sepsum rad dru fertum waklo dom futrum madza lentra')
    ]).

test('context as option', []) :-    
    % given    
    Arguments = ['--t-flag', test_b, something, '--long-ex', 'assigned'],
    set_prolog_flag(os_argv, Arguments),
    % when
    with_output_to(atom(Response), execution_cli:execute_cli(Arguments) ),
    % then
    read_term_from_atom(Response, response(Positional, Options), [my_option(true)]),
    Positional == [something], 
    Options == [my_option(true)].
    
:- execution_cli:register_cli_command(
    test_c, 
    [A, B] >> write_term(response(A,B), []), 
    [context(cli_tests:t_flag, my_option)]).

test('missing option', [throws(cli_option(required, _))]) :-    
    % given
    Arguments = [test_c, something, 'assigned'],
    set_prolog_flag(os_argv, Arguments),
    % when
    with_output_to(atom(_), execution_cli:execute_cli(Arguments) ).

:- end_tests(execution_cli).


