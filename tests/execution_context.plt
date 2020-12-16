:- begin_tests(execution_context).
:- use_module('../prolog/execution_context').

:- encoding(utf8).

% this tests assumes the prolog was invoked with following command line arguments: 
%    prolog debug.pl -- --cli-flag --no-negated -f --variable-long-a="long A" --flag-ex=false -s short --long long --module-default=module
%
% The environment variable EC_TEST must be set to 'my variable', and environment variable  ec_module_setting must be set to 'setting'.

:- execution_context:context_variable(cli_flag, atom, [long('cli-flag'), is_flag(true)]).
:- execution_context:context_variable(negated, atom, [long(negated), is_flag(true)]).
:- execution_context:context_variable(short_flag, atom, [short(f), is_flag(true)]).
:- execution_context:context_variable(assigned_flag, atom, [long('flag-ex'), is_flag(true)]).
:- execution_context:context_variable(short_value, atom, [short('S'), long(short)]).
:- execution_context:context_variable(long_value, atom, [short(l), long(long) ]).
:- execution_context:context_variable(long_assigned, atom, [short(a), long('variable-long-a') ]).
:- execution_context:context_variable(module:default, atom, []).

test(cli, 
    [
        forall(member(Var=Value, 
            [
                long_value=long,
                cli_flag=true,
                negated=false,
                short_flag=true,
                assigned_flag=false,
                short_value=short,                
                long_assigned='long A',
                module:default=module
            ]))
    ]) :-
    execution_context:context_variable_value(Var, Value).

:- execution_context:context_variable(environment_var, atom, [env('EC_TEST')]).
:- execution_context:context_variable('ec-module':setting, atom, []).

test(environment, 
    [
        forall(member(Var=Value, 
            [
                ec_module:config=prod_env,
                ec_module:dev='developer endpoint',
                ec_module:test_config='42',
                ec_module:user_env='passwd-user',
                ec_module:'last-line'=last,
                ec_module:dev_only=just_my_code
            ]))
    ]) :-
    execution_context:context_variable_value(Var, Value).

:- execution_context:context_variable(ec_module:config, atom, []).
:- execution_context:context_variable(ec_module:dev, atom, [env('AAA_DONT')]).
:- execution_context:context_variable(ec_module:test_config, atom, [env('ENV_TEST_CONFIG')]).
:- execution_context:context_variable(ec_module:'flag-config', atom, []).
:- execution_context:context_variable(ec_module:user_env, atom, []).
:- execution_context:context_variable(ec_module:'last-line', atom, []).
:- execution_context:context_variable(ec_module:dev_only, atom, []).

test(config, 
    [
        forall(member(Var=Value, 
            [
                ec_module:config=prod_env,
                ec_module:dev='developer endpoint',
                ec_module:test_config='42',
                ec_module:'flag-config'=true,
                ec_module:user_env='passwd-user',
                ec_module:'last-line'=last,
                ec_module:dev_only=just_my_code
            ]))
    ]) :-
    execution_context:context_variable_value(Var, Value).

:- execution_context:context_variable(ec_module:default_value, atom, [default(some_default)]).
:- execution_context:context_variable(ec_module:forward_value, atom, [default(context(ec_module:default_value))]).
:- execution_context:context_variable(ec_module:forward_module_value, atom, [default(context(default_value))]).

test(defaults, 
    [
        forall(member(Var=Value,
            [
                ec_module:default_value=some_default,
                ec_module:forward_value=some_default,
                ec_module:forward_module_value=some_default
            ]))
    ]) :-
    execution_context:context_variable_value(Var, Value).

test(options, 
    [
        forall(member(Option=Options,
            [
                ec_module:default_value(some_option)=[ec_module:default_value(some_option)],
                ec_module:default_value(some_option)=[default_value(some_option)],
                ec_module:default_value(some_default)=[]
            ]))
    ]) :-
    execution_context:option_or_context_variable(Option, Options).

:- execution_context:context_variable(ec_module:bool_true_value, bool, [default(tRue)]).
:- execution_context:context_variable(ec_module:bool_yes_value, bool, [default(y)]).
:- execution_context:context_variable(ec_module:bool_no_value, bool, [default(no)]).
:- execution_context:context_variable(ec_module:bool_integer_value, number, [default(42)]).
:- execution_context:context_variable(ec_module:bool_float_value, number, [default('3.14')]).
:- execution_context:context_variable(ec_module:bool_string_value, string, [default(hallo)]).
:- execution_context:context_variable(ec_module:bool_list1_value, list, [default('[ 1;2, 3 ; 5]')]).
:- execution_context:context_variable(ec_module:bool_list2_value, list(atom), [default(' a, b ; c ;d')]).
:- execution_context:context_variable(ec_module:bool_list3_value, list(number), [default(' ( 7, 8 ; 9, 10 )' )]).

test(adapt_type, 
    [
        forall(member(Var=Value,
            [
                ec_module:bool_true_value=true,
                ec_module:bool_yes_value=true,
                ec_module:bool_no_value=false,
                ec_module:bool_integer_value=42,
                ec_module:bool_float_value=3.14,
                ec_module:bool_string_value="hallo",
                ec_module:bool_list1_value=['1', '2', '3', '5'],
                ec_module:bool_list2_value=[a,b,c,d],
                ec_module:bool_list3_value=[7,8,9,10]
            ]))
    ]) :-
    execution_context:context_variable_value(Var, Value).

:- end_tests(execution_context).


