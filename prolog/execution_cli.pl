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
  phrase(positional_args([ Command | Positional]), Arguments),  
  (   Command == help 
  ->  portray_command_help(help, [])
  ;   command_spec(Command, Goal, ArgumentSpec),
      (   Positional = [ help | _ ]
      ->  portray_command_help(Command, ArgumentSpec)
      ;   verify_arguments(ArgumentSpec, Positional, Options),
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
%  * `optional(context(ContextVariable, OptionName))` - similar to above, but the context variable is not
%    required to be resolved
%  * `positional(Nth1, OptionName)` - associates `Nth1` element of positional arguments with option `OptionName`
%    when calling `Goal`. If there are not enough positional argumentsthen the exception is raised and user is 
%    informed that argument is missing 
%  * `optional(positional(Nth1, OptionName))` - similar to above, but the positional argument is not
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

portray_command_help(Command, Spec) :-
  phrase(portray_command_help(Command, Spec), Lines),
  atomic_list_concat(Lines, '\n', Message),
  print_message(help, format(Message, [])),
  !.

portray_command_help(help, _) -->
  {   current_prolog_flag(os_argv, [Exe | _]),
      directory_file_path(_, File, Exe),  
      format(atom(Line0), 'Usage: ~w <command> [common options] [command options]', [File]), 
      findall(Command-Spec, command_spec(Command, _, Spec), Commands)
  },
  [Line0, 'Where <command> is one of:'],
  portray_command_infos(Commands),
  [ '', 'Options common for all commands:'],
  portray_command_options([]),
  !.
 portray_command_help(Command, Spec) --> 
  {   current_prolog_flag(os_argv, [Exe | _]),
      directory_file_path(_, File, Exe),  
      format(atom(Line0), 'Usage: ~w ~w [common options] [options]', [File, Command])   
  },  
  [Line0],
  portray_command_info(Command, Spec),
  ['', 'Options:' ],
  portray_command_options(Command),
  [ '', 'Options common for all commands:'],
  portray_command_options([]),
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
  

portray_command_options(Command) -->
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
      sort(Specs, Sorted)
  },
  portray_options(Sorted).

portray_option(Long, Short, Type, Spec) -->
  {   memberchk(describe(Description0), Spec),      
      (   portray_type_text(Type, Text)
      ->  atomic_list_concat(['(', Text, ') ', Description0], Description1)
      ;   Description1 =  Description0
      ),
      (   memberchk(env(Env), Spec)
      ->  atomic_list_concat([Description1, ' Can be set by environment variable ', Env], Description)      
      ;   Description = Description1
      ),      
      split_long_description(Description, 56, [Line|Lines]),      
      format(atom(OutLine), '  ~w ~20|~w ~24|~w', [Long, Short, Line ] ),
      maplist([Descr, Out] >>format(atom(Out), '~24|~w', [Descr]), Lines, OutLines)
  },
  [OutLine| OutLines],
  !.
 portray_option(_, _) --> [].

portray_options([]) --> [], !.
 portray_options([Long-Short-Spec-Type|Specs]) -->
    portray_option(Long, Short, Type, Spec), 
    portray_options(Specs).

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

prolog:message(cli_option(unknown, Option)) -->
    [ 'Unknown command line option ~w' - [Option] ].
 prolog:message(cli_option(unknown, Option)) -->
    [ 'Missing required command line option \'~w\' ' - [Option] ].

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
 verify_arguments([positional(Index, OptionName)| Spec], Positional, [Option|Options] ) :-
      nth1(Index, Positional, Value),
      Option =.. [OptionName, Value],
      !,
      verify_arguments( Spec, Positional, Options ).
 verify_arguments([context(ContextVariable, _) | _], _, _ ) :-    
    throw(cli_option(positional, OptionName)).
    
