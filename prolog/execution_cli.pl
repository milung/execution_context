:- module(execution_cli, [
  execute_cli/0,
  execute_cli/1,            % +Arguments:list(atom)
  register_cli_command/3    % +Command:atom, :Goal, +ArgumentsSpec:list
]). 

:- use_module(library(dcg/basics)).
:- use_module(execution_context).

:- meta_predicate 
  register_cli_command(+, 2, +).

:- dynamic
    command_spec/3.

:- multifile
  prolog:message//1.

%%%%%%%%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%

%! execute_cli is det
%  Same as `current_prolog_flag(argv, Arguments), execute_cli(Arguments)`
execute_cli :-
  current_prolog_flag(argv, Arguments),
  execute_cli(Arguments).

%! execute_cli(+Arguments:list(atom)) is det
%  Interprets `Arguments` which are assumed to be unified  by `current_prolog_flag(argv, Arguments)` call
%  and calls goal registered by `register:cli_command/2` directive. The name of the command is determined 
%  by the first positional argument. Options are removed from the `Arguments` list by checking against 
%  registered context variables - see `execution_context:context_variable/3` predicate. 
%  
%  Throws `cli_option(Type, OptionName)` exception if required arguments are not provided 
%  (see `register_cli_command/3`), or when the option that cannot be associated with any context variable 
%  is found on command line arguments. The exception can be used for print_message call. 
execute_cli(Arguments) :-  
  (   phrase(positional_args([ Command | Positional]), Arguments)
  ->  true
  ;   Command = help
  ),
  (   Command == help 
  ->  portray_command_help(help, [])
  ;   ( command_spec(Command, Goal, ArgumentSpec)
      ->  true
      ;   print_message(error, format('Unknown command `~w`', [Command])),
          halt(42)
      ),
      (   Positional = [ help | _ ]
      ->  portray_command_help(Command, ArgumentSpec)
      ;   verify_arguments(ArgumentSpec, Positional, Options),
          retractall(execution_context:variable_cache),
          call(Goal, Positional, Options)
      )
  ), !.

%! register_cli_command(+Command:atom, :Goal, +ArgumentsSpec:list) is det
%  Registers new command for CLI. The `Goal` will be invoked when the first positional argument 
%  unifies with `Command`. The `Goal` is invoked as `call(Goal, PositionalArguments, Options)`
%  where `PositionalArguments` are all arguments that are not an command line option, and `Options`
%  maps some of the context variables to the goal options. 
%  `ArgumentSpec` can contain some of the following terms: 
%  * `describe(Message)` - provides human readable information about the command that is printed when
%    the CLI command `help` or `command help` is invoked. 
%  * `context(ContextVariable, OptionName)` - if call to `context_variable_value(ContextVariable, Value)` 
%    succeeds, then `Goal` second argument will contain option named `OptionName` with the value `Value`. 
%    If the context_variable is not resolved then the exception is raised and user informed that the required
%    option is missing
%  * `optional(context(ContextVariable, OptionName))` - similar to above, but the c ontext variable is not
%    required to be resolved
%  * `positional(Nth1, OptionName, Describe)` - associates `Nth1` element of positional arguments with option `OptionName`
%    when calling `Goal`. If there are not enough positional argumentsthen the exception is raised and user is 
%    informed that argument is missing 
%  * `optional(positional(Nth1, OptionName, Describe))` - similar to above, but the positional argument is not
%    required to be resolved
register_cli_command(Command, Goal, ArgumentsSpec) :-
  retractall(command_spec(Command, _,  _)),
  assert(command_spec(Command, Goal, ArgumentsSpec) ).


%%%%%%%%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%% 
is_variable(Option, Type) :-
  execution_context:context_variable_def(_, _, Spec),
  memberchk(Option, Spec),
  ( Type == bool
  ->  memberchk(is_flag(true), Spec)
  ;   true
  ).
is_variable(long(Negated), bool) :-
  atom_concat('no-', OptionName, Negated),
  execution_context:context_variable_def(_, _, Spec),
  memberchk(long(OptionName), Spec),
  memberchk(is_flag(true), Spec).
is_variable(long(OptionName), Type) :-
  Type \= true,
  execution_context:context_variable_def(Var, _, Spec),
  execution_context:default_name(Var, '-', DefaultName ),
  (   Type == bool
  ->  memberchk(is_flag(true), Spec),
      ( OptionName == DefaultName
      ; atom_concat('no-', DefaultName, OptionName)
      )
  ;   OptionName == DefaultName
  ).

portray_arguments([]) --> [], !.
  portray_arguments([Name-Description|Args]) -->
    {   split_long_description(Description, 60, [Line|Lines]),
        format(atom(OutLine), '  ~w~20|~w', [Name, Line]),
        maplist([Descr, Out] >>format(atom(Out), '~20|~w', [Descr]), Lines, OutLines)
    },
    [OutLine| OutLines ],
    !,
    portray_arguments(Args).
 
portray_command_help(Command, Spec) :-
  phrase(portray_command_help(Command, Spec), Lines),
  atomic_list_concat(Lines, '\n', Message),
  print_message(help, format(Message, [])),
  !.

portray_command_help(help, _) -->
  {   program_name(File),  
      format(atom(Line0), 'Usage: ~w <command> [common options] [command options]', [File]), 
      findall(Command-Spec, command_spec(Command, _, Spec), Commands)
  },
  [Line0, 'Where <command> is one of:'],
  portray_command_infos(Commands),
  portray_command_options([], [ '', 'Options common for all commands:']),
  !.
 portray_command_help(Command, Spec) --> 
  {   program_name(File)   
  },    
  portray_command_usage(File, Command, Spec),
  portray_command_info(Command, Spec),
  portray_positional_arguments(Spec),
  portray_command_options(Command,['', 'Options:' ]),
  portray_command_options([],[ '', 'Options common for all commands:']),
  ['', 'Use "<command> help" for additional details' ],
  !.

portray_command_info(Command, Spec) -->
    {   memberchk(describe(Description), Spec),
        split_long_description(Description, 60, [Line|Lines]),
        format(atom(OutLine), '  ~w~20|~w', [Command, Line]),
        maplist([Descr, Out] >>format(atom(Out), '~20|~w', [Descr]), Lines, OutLines)
    },
    [OutLine| OutLines],
    !.
 portray_command_info(_, _) --> [].

portray_command_infos([]) --> [].
 portray_command_infos([Command-Spec| Commands]) -->
    portray_command_info(Command, Spec),
    portray_command_infos(Commands).
  

portray_command_options(Command, Header) -->
  {   findall(
          Long-Short-Spec-Type, 
          (   execution_context:context_variable_def(_, Type, Spec),
              (
                  memberchk(cli_command(Command), Spec)
              ->  true
              ;   memberchk(cli_command(Commands), Spec),
                  memberchk(Command, Commands)
              ),
              (   memberchk(long(Long0), Spec)
              ->  atom_concat('--', Long0, Long)                
              ;   Long = ''
              ), 
              (   memberchk(short(Short0), Spec)
              ->  atom_concat('-', Short0, Short)              
              ;   Short = '' 
              ),
              \+ (Short == '', Long == '')
          ),
          Specs),
      sort(Specs, Sorted),
      \+ length(Sorted,0)
  },
  Header,
  portray_options(Sorted).
portray_command_options(_, _) --> [].

portray_command_usage(File, Command, Spec) -->
  {
    findall(Index-Name, member(positional(Index, Name, _), Spec), Positionals),    
    findall(Index-Name, member(optional(positional(Index, Name, _)), Spec), Optionals0),
    (   length(Optionals0, 0)
    ->  Arguments0 = Positionals
    ;   maplist([Index-Name, Index-OptName] >> atomic_list_concat(['[', Name, ']'], OptName), Optionals ),
        append(Positionals, Optionals, Arguments0)
    ),
    sort(Arguments0, Arguments1),
    pairs_values(Arguments1, Arguments2),
    atomic_list_concat(Arguments2, ' ', Arguments),        
    format(atom(Usage), 'Usage: ~w ~w <Common options...> <Options...> ~w', [File, Command, Arguments])
  },
  [Usage].

portray_option(Long, Short, Type, Spec) -->
  {   memberchk(describe(Description0), Spec),   
      (   memberchk(default(Default), Spec)
      ->  format(atom(DefaultText), 'Defaults to `~w`. ', [ Default])
      ;   DefaultText = ''
      ),   
      (   portray_type_text(Type, Text)
      ->  atomic_list_concat(['(', Text, ') '], TypeText)
      ;   TypeText = ''
      ),      
      (   memberchk(env(Env), Spec)
      ->  atomic_list_concat(['. Can be set by environment variable ', Env, '. '], EnvText)      
      ;   EnvText = ''
      ),      
      format(atom(Description), '~w~w~w~w', [TypeText, DefaultText, Description0, EnvText]),
      split_long_description(Description, 56, [Line|Lines]),      
      format(atom(OutLine), '  ~w ~20|~w ~24|~w', [Long, Short, Line ] ),
      maplist([Descr, Out] >>format(atom(Out), '~24|~w', [Descr]), Lines, OutLines)
  },
  [OutLine| OutLines],
  !.
 portray_option(_, _, _, _) --> [].

portray_options([]) --> [], !.
 portray_options([Long-Short-Spec-Type|Specs]) -->
    portray_option(Long, Short, Type, Spec), 
    portray_options(Specs).

portray_positional_arguments(Spec) -->
  {
    findall(Name-Description, member(positional(_, Name, Description), Spec), Positionals0),
    sort(Positionals0, Positionals),
    (   length(Positionals, 0)
    ->  PositionalsHeader = []
    ;    PositionalsHeader = ['', 'Mandatory arguments:']
    ),
    findall(Name-Description, member(optional(positional(_, Name, Description)), Spec), Optionals0),
    sort(Optionals0, Optionals),
    (   length(Optionals, 0)
    ->  OptionalsHeader = []
    ;    OptionalsHeader = ['', 'Optional arguments:']
    ) 
  },
  PositionalsHeader,
  portray_arguments(Positionals),
  OptionalsHeader,
  portray_arguments(Optionals).

portray_type_text(bool, flag).
 portray_type_text(number, number).
 portray_type_text(atom, value).
 portray_type_text(list, 'comma separated list of values').
 portray_type_text(list(number), 'comma separated list of numbers').
 portray_type_text(list(_), 'comma separated list of values').
 portray_type_text(Type, Type).



positional_args([]) --> [].
 positional_args(Positional) -->
   [Option],
   { atom_concat('--', Long, Option),
     atomic_list_concat([_, _], '=', Long)
   },
   !,
   positional_args(Positional).
 positional_args(Positional) -->
   [Option],
   { atom_concat('--', Long, Option),
     is_variable(long(Long), bool)
   },
   !,
   positional_args(Positional).
 positional_args(Positional) -->
   [Option],
   { atom_concat('--', Long, Option),
     is_variable(long(Long), _)
   },
   [_],
   !,
   positional_args(Positional).
 positional_args(_)  -->
   [Option],
   {  atom_concat('--', _, Option),
      throw(cli_option(unknown, Option))
   }.
 positional_args(Positional) -->
   [Option],
   {  atom_concat('-', Short, Option),
      atom_length(Short, 1),
      is_variable(short(Short), bool)
   },
   !,
   positional_args(Positional).
 positional_args(Positional) -->
   [Option],
   {  atom_concat('-', Short, Option),
      atom_length(Short, 1),
      is_variable(short(Short), _)
   },
   [_],
   !,
   positional_args(Positional).
 positional_args(Positional) -->
    positional_mixed_flags,
    !,
    positional_args(Positional).
 positional_args(_)  -->
   [Option],
   {  atom_concat('-', _, Option),
      throw(cli_option(unknown, Option))
   }.
 positional_args([Arg | Positional])  -->
   [Arg],
   positional_args( Positional).

positional_mixed_flags, Flags -->
    [Option],
    {  atom_concat('-', Short, Option),
       atom_length(Short, L),
       L > 1,
       atom_codes(Short, Codes),
       maplist([C, F] >> atom_codes(F, [0'-, C]), Codes, Flags)
    },
    !.
program_name(Name) :-
  current_prolog_flag(os_argv, [Exe | _]),
  atomic_list_concat(Segments, '\\', Exe),
  atomic_list_concat(Segments, '/', Path),
  directory_file_path(_, File, Path),
  file_name_extension(Name, _, File).

prolog:message(cli_option(unknown, Option)) -->
    [ 'Unknown command line option ~w' - [Option] ].
 prolog:message(cli_option(unknown, Option)) -->
    [ 'Missing required command line option \'~w\' ' - [Option] ].
 prolog:message(cli_option(positional, Option)) -->
    [ 'Required argument \'~w\' is missing ' - [Option] ].

split_long_description(Long, Length, Lines) :-
  atom_codes(Long, Codes),
  phrase(split_to_lines(Length, Lines), Codes).

split_take_word(L, L1, []) -->
    [C],
    {   is_white(C),
        L1 is L - 1
    },
    !.
 split_take_word( L, _,  _) -->
    { L =< 0, !,  fail}.
 split_take_word( L, L2, [C|Codes]) -->     
    [C],
    { L1 is  L -1 },
    !,
    split_take_word(L1, L2, Codes).
 split_take_word(L, L, []) --> [], !.

split_take_line(_, []) --> eos, !.
 split_take_line(Length, [Word|Words]) -->
    split_take_word(Length, Remaining, WordCodes),
    { atom_codes(Word, WordCodes) },
    !,
    split_take_line(Remaining, Words),
    !.
 split_take_line(_, []) --> [], !.

split_to_lines(_, []) --> eos, !.
 split_to_lines(Length, [Line|Lines]) --> 
    split_take_line(Length, LineWords), 
    { atomic_list_concat(LineWords, ' ', Line)},
    !,
    split_to_lines(Length, Lines).

verify_arguments([], _, []).
 verify_arguments([optional(Element) | Spec], Argv, Options ) :-
    !,
    catch(  
        (   verify_arguments([Element | Spec], Argv, Options ) 
        ;   verify_arguments( Spec, Argv, Options )
        ),
        cli_option(_, _),
        verify_arguments( Spec, Argv, Options )
    ).
 verify_arguments([describe(_) | Spec], Argv, Options ) :-
    !,
    verify_arguments( Spec, Argv, Options ).
 verify_arguments([context(ContextVariable, OptionName)| Spec], Argv, [Option|Options] ) :-
    context_variable_value(ContextVariable, Value),
    Option =.. [OptionName, Value],
    !,
    verify_arguments( Spec, Argv, Options ).
 verify_arguments([context(ContextVariable, _) | _], _, _ ) :-
    execution_context:context_variable_def(ContextVariable, _, Spec),
    (   memberchk(long(OptionName), Spec)
    ->  true
    ;   execution_context:default_name(ContextVariable, '-', OptionName )
    ),
    throw(cli_option(required, OptionName)).
 verify_arguments([positional(Index, OptionName, _)| Spec], Positional, [Option|Options] ) :-
      nth1(Index, Positional, Value),
      Option =.. [OptionName, Value],
      !,
      verify_arguments( Spec, Positional, Options ).
 verify_arguments([positional(_, OptionName, _) | _], _, _ ) :-    
    throw(cli_option(positional, OptionName)).
    
