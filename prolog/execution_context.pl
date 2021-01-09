:- module(execution_context, [
    context_variable/3,             % +Name:atom, +Type, +Options:list
    context_variable_value/2,       % +Variable, -Value
    option_or_context_variable/2    % @Option, +Options
]).
%! <module> Execution Context predicate
%  Execution context provides means to define environment variable in variaty of ways.
%  Similar to setting/4 you have to define register your context variable a priori by calling context_variable/3.
%  The context variable can be then provided e.g. through command line arguments, environment variable or configuration env file.
%  The value of the variable is resolved on the first call and then cached durring the runtime.

:- use_module(library(dcg/basics)).
:- use_module(library(option)).
:- use_module(library(pure_input)).

:- dynamic
    configuration/2,
    context_variable_def/3,
    variable_cache/2.

:- meta_predicate     
    context_variable(:, +, +),
    context_variable_value(:,-),    
    option_or_context_variable(:,+).

:- multifile 
    context_variable/3.

:- asserta(user:file_search_path(params, '.')).
:- asserta(user:file_search_path(params, './config')).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%

%! context_variable(+Name:atom, +Type, +Options:list) is det.
%  Declares new contextual variable with the module specific name. Repeated declarations
%  for the same `Name` are possible, the last call is taken into account but not after the
%  value was queried (which would be served from the cache.
% 
%  Type can be one of the _atom_, _bool_, _number_, _string_, _list_, _list(atom)_, _list(number)_.
%  in case of list types, the list can be specified as a list of atoms or numbers separate by comma (,)
%  or semicolon, and optionally enclosed into parenthesis or brackets (e.g. `[ a, b,c, e]`, 
%  or `(1;2; 3 ; 4)`, or `o, p ; q, r `) will work. Elements in the list are trimmed of left and right spaces. 
%  
%  Options can be one of: 
%  * `describe(Message)` - provides human readable information about the command that is printed when
%    the CLI help is invoked (for cli options). 
%  * `long(Long)` - specifies the long name of the command line parameter prefixed with two dashes. If not provided then the 
%        `Long` name is assumed to be based on the name with characters ' ', '_', ':' replaced by the dash. Value of the 
%        variable can be either concatenated into argument with '=' character, or separated by space. For example if the `long(some_option)`
%        is given then the argument `--some-option=value`, or `--some-option value` will both assign `value` to context variable of the `Name`. 
%  * `short(Short)` - specifies short variant of the command line argument prefixed by '-'. Value must be provided after the space. For 
%        example if the option short(s) is given then the argument `-s value` will assign 'value' to context variable of the Name
%  * `is_flag(Bool)` - if Bool is true, then the system recognize the options with out value and `--no-long` options, where `long` correspnds to the 
%        `long(Atom)` option value. Variables of type `bool` are implicitly considered to be flags
%  * `env(EnvName)` - specifies the name of the environment variable to check for the value of the context variable. If not provided then the 
%        EnvName name is assumed to be based on the name with characters ' ', '_', ':' replaced by the undescore.
%  * `default(Value)` - Specifies the default values if no other means provides a value. If the `Value` is of the form `context(Variable)`, then 
%        context variable `Variable` is evaluated for the default value. Infinite dependency cycles are not resolved, so be carefull here.
%  * `cli_command(Commands)` - `Commands` is list of CLI commands (see `execute_cli/1`), for which the variable is intended. This information
%    is only used to describe usage of the command. Either option `short` or `long˙, and option `describe˙ must be provided to provide any
%    help information about the variable. The option `cli_command([])` is interpreted as the option for all CLI commands. 
context_variable(Name, bool, Options) :-
    retractall(context_variable_def(Name,_,_)),
    assert(context_variable_def(Name, bool, [is_flag(true)|Options])),
    !.
context_variable(Name, Type, Options) :-
    retractall(context_variable_def(Name,_,_)),
    assert(context_variable_def(Name, Type, Options)).

%! context_variable_value(+Variable, -Value) is semidet
%  Unifies Value with the contextual variable. The Variable must be declared using
%  context_variable/3 call.
%
%  To resolve the contextual variable following steps are executed. The first step
%  that succed will determine the Value
%  * Check for short or long variant of command line arguments name associated with the variable
%  * Check for the environment variable name
%  * Check if the Variable name or environment variable name is assigned in the config.user.env file
%  * Check if the Variable name or environment variable name is assigned in the config.dev.env file
%  * Check if the Variable name or environment variable name is assigned in the config.env file
%  * Check if the default value is provided.
%
%  The `config.env`, `config.dev.env`, and `config.env` files are looked up in  the file search paths defined as
%  `params(config.dev)`. The params path resolves to current working directory or into the `config` directory of the current working 
%  directory. The name variants are provided by the options of the predicate `context_variable/2` or derived from the context variable 
%  name. 
context_variable_value(Variable, Value) :-
    context_variable_def(Variable, Type, Options),
    once(resolve_context_variable(Variable, ValueAtom, Options)),
    once(adapt_type(ValueAtom, Type, Value)).

%! option_or_context_variable(Option, Options) is semidet
%  As `option/2`, but if that fails then it behaves as `context_variable(OptionFunctor, OptionArg)`. 
%  `Options` are checked for the module specific or local name of the option. 
option_or_context_variable(Option, Options):-
    option(Option, Options), 
    !.
 option_or_context_variable( Option, Options):-
    Option =.. [ ':', _, LocalOption ],
    option( LocalOption, Options), 
    !.
option_or_context_variable( Option, _) :-
    Option =.. [':', Module, LocalOption],
    LocalOption =.. [ Var, Value],
    context_variable_value(Module:Var, Value).

%%%  PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%% 

adapt_type(Value, atom, Value).
 adapt_type(Value, bool, true) :-
    downcase_atom(Value, Lower),
    memberchk(Lower, [true, yes, '1', t, y, ok, positive]), 
    !.
 adapt_type(_, bool, false).
 adapt_type(Atom, number, Value) :-
    atom_number(Atom, Value).
 adapt_type(Atom, string, Value) :-
    atom_string(Atom, Value).
 adapt_type(Atom, list, Value) :-
    adapt_type(Atom, list(atom), Value).
 adapt_type(Atom, list(atom), Value) :-
    atom_codes(Atom, Codes),
    phrase(value_list_grammar(Value), Codes).
 adapt_type(Atom, list(number), Value) :-
    adapt_type(Atom, list(atom), Atoms),
    maplist(atom_number, Atoms, Value).


comment -->
    "#", 
    string(_),
    nl.

config([], Variable, Value) -->
    {        
        atom_codes(VariableName, Variable),
        atom_codes(ValueName, Value),        
        (
            retract(configuration(VariableName,_)),      
            fail
        ;
            true
        ),
        assertz(configuration(VariableName, ValueName))
    }.
 config(Module, Variable, Value) -->
    {   
        atom_codes(ModuleName, Module),     
        atom_codes(VariableName, Variable),
        (
            Value == true 
        ->  ValueName = true
        ;   atom_codes(ValueName, Value)
        ),          
        (
            retract(configuration(ModuleName:VariableName,_)),      
            fail
        ;
            true
        ),
        assertz(configuration(ModuleName:VariableName, ValueName))
    }.

empty_line -->
    whites,
    nl.

env_grammar --> eos.
env_grammar -->
    env_line,
    env_grammar.

env_line -->
    comment,
    !.
 env_line -->
    empty_line,
    !.
 env_line -->
    module(Module),
    string_without("=\r\n", Variable),
    "=",
    string(Value),
    whites,
    nl,
    config(Module, Variable, Value),
    !.
 env_line -->   
    module(Module),
    string_without("\r\n", Variable),
    whites,
    nl,
    config(Module, Variable, true),
    !.
 
default_codes([], _, []) :- !.
 default_codes([I|In], C, [O|Out]) :-
    (
        member(I, [0' ,0'-, 0'_, 0'.])
    ->  O = C
    ;   O = I
    ),
    default_codes(In, C, Out).
    
default_name(Module:Variable, Separator, DefaultName) :-
    atomic_list_concat([Module, Variable], ' ', Default0),
    atom_codes(Default0, Codes0),
    atom_codes(Separator, [CodesSep|_]),
    default_codes(Codes0, CodesSep, Codes1),
    atom_codes(DefaultName, Codes1).

find_argv([Arg| _], Long, _, true, false) :-
    atom_concat('--no-', Long, Arg).
 find_argv([Arg| _], Long, _, true, true) :-
    atom_concat('--', Long, Arg).
 find_argv([Arg| _], _, Short, true, true) :-
    Short \= [],
    atom_codes( Arg, ArgCodes),
    ArgCodes \= [ 0'-, 0'- | _ ],
    ArgCodes = [ 0'- | _ ],    
    atom_codes(Short, [Code|_]),
    memberchk(Code, ArgCodes).   
 find_argv([Arg| _], Long, _, _, Value) :-
    atomic_list_concat(['--', Long, '='], ArgPrefix),
    atom_concat(ArgPrefix, Value, Arg).
 find_argv([Arg, Value| _], Long, _, false, Value) :-
    atom_concat('--', Long, Arg).
 find_argv([Arg, Value| _], _, Short, false, Value) :-
    Short \= [],
    atom_concat('-', Shorts, Arg),
    \+ atom_concat('-', _, Shorts),
    atom_concat(_, Short, Shorts).
 find_argv([_| Args], Long, Short, IsFlag, Value) :-
    find_argv(Args, Long, Short, IsFlag, Value).

load_configurations :-
    retractall(configuration(_,_)),
    load_configuration('config.env'),
    load_configuration('config.dev.env'),
    load_configuration('config.user.env'),
    !.

load_configuration(File) :-    
    absolute_file_name(params(File), AbsolutePath, [file_errors(fail)]),
    exists_file(AbsolutePath),
    setup_call_cleanup(
        open(AbsolutePath, read, Stream, [encoding(utf8)]),
        phrase_from_stream(env_grammar, Stream),
        close(Stream)).
 load_configuration(_).

module(Module) -->
    string_without("\n\r:=", Module),
    ":", 
    !.
 module([]) --> [].

nl --> "\r\n".
 nl --> "\n".
 nl --> eos.

resolve_default(Module:_, Value, Options) :-
    option(default(context(ForwardVariable)), Options),
    ForwardVariable \= :(_,_),
    context_variable_value(Module:ForwardVariable, Value).
 resolve_default(_, Value, Options) :-
    option(default(context(Module:ForwardVariable)), Options),
    context_variable_value(Module:ForwardVariable, Value).
 
 resolve_default(_, Value, Options) :-
    option(default(Default), Options),
    Default \= context(_),
    format(atom(Value), '~w', Default).


resolve_command_line(Variable, Value, Options):-
    current_prolog_flag(os_argv, Argv),
    option(is_flag(IsFlag), Options, false),    
    option(short(Short), Options, []),
    default_name(Variable, '-', DefaultLong),
    option(long(Long), Options, DefaultLong),
    find_argv(Argv, Long, Short, IsFlag, Value).

resolve_configuration(Variable, Value, _) :-
    configuration(Variable, Value).
 resolve_configuration(_, Value, Options) :-
    option(env(EnvName), Options),
    configuration(EnvName, Value).

resolve_context_variable(Variable, Value, Options) :-
    variable_cache(Variable, Value)
    ->  true
    ;   (                
            resolve_command_line(Variable, Value, Options)
        ; 
            resolve_environment(Variable, Value, Options)
        ;
            resolve_configuration(Variable, Value, Options)
        ; 
            resolve_default(Variable, Value, Options)
        ),
        asserta(variable_cache(Variable, Value)).

resolve_environment(Variable, Value, Options) :-
    default_name(Variable, '_', DefaultName),
    option(env(EnvName), Options,  DefaultName),
    getenv(EnvName, Value).


value_list_close --> whites, ("]" ; ")" ; []), whites, !.


value_list_element(Element) -->
    whites,
    string(String),
    whites,
    { atom_codes(Element, String)}.

value_list_elements([Element|List]) -->    
    value_list_element(Element),
    value_list_separator,
    value_list_elements(List).
 value_list_elements([Element]) -->    
    value_list_element(Element).


value_list_grammar(List) -->
    value_list_open,
    value_list_elements(List),
    value_list_close.
 value_list_grammar([]) -->
    value_list_open,    
    value_list_close.

value_list_open --> whites, ("[" ; "(" ; []), !.

value_list_separator --> ",".
value_list_separator --> ";".

:- load_configurations.