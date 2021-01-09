# Execution Context

Prolog module for reading execution context variables. Enables to retrieve execution specific variables from 
command line options, environment variables, configuration file or fallback to some predefined default value. 

## USAGE

Use module `execution_context` and declare your context variables by invoking `context_variable/3` predicate.
After that you can use the predicate `context_variable/2` or `option_or_context_variable/2` predicates to read
value of the context variable. 

Example: 

```prolog
:- use_module(library(execution_context)).

:- context_variable(my_variable, atom, [long('cli-arg-name'), short(m), env(MY_VARIABLE), default(some_value)]).

hallo :-
    context_variable_value(my_variable, Value),
    writeln(Value).
```

## Details

The variable names are module scoped. The resolution is done in the following order: 
* options if the `option_or_context_variable/2` is used
* command line arguments
* environment variables
* `config.user.env` file
* `config.dev.env` file
* `config.env` file
* default value if specified durring declaration. 

The values are resolved only once, then cached in local database

## Exported predicates from the module `execution_context`

### `context_variable(+Name:atom, +Type, +Options:list)` is det.

Declares new contextual variable with the module specific name. Repeated declarations
for the same `Name` are possible, the last call is taken into account but not after the
value was queried (which would be served from the cache.
Type can be one of the _atom_, _bool_, _number_, _string_, _list_, _list(atom)_, _list(number)_.
in case of list types, the list can be specified as a list of atoms or numbers separate by comma (,)
or semicolon, and optionally enclosed into parenthesis or brackets (e.g. `[ a, b,c, e]`, 
or `(1;2; 3 ; 4)`, or `o, p ; q, r `) will work. Elements in the list are trimmed of left and right spaces. 

Options can be one of: 
* `describe(Message)` - provides human readable information about the command that is printed when
  the CLI help is invoked (for cli options). 
* `long(Long)` - specifies the long name of the command line parameter prefixed with two dashes. If not provided then the 
      `Long` name is assumed to be based on the name with characters ' ', '_', ':' replaced by the dash. Value of the 
      variable can be either concatenated into argument with '=' character, or separated by space. For example if the `long(some_option)`
      is given then the argument `--some-option=value`, or `--some-option value` will both assign `value` to context variable of the `Name`. 
* `short(Short)` - specifies short variant of the command line argument prefixed by '-'. Value must be provided after the space. For 
      example if the option short(s) is given then the argument `-s value` will assign 'value' to context variable of the Name
* `is_flag(Bool)` - if Bool is true, then the system recognize the options with out value and `--no-long` options, where `long` correspnds to the 
      `long(Atom)` option value. Variables of type `bool` are implicitly considered to be flags
* `env(EnvName)` - specifies the name of the environment variable to check for the value of the context variable. If not provided then the 
      EnvName name is assumed to be based on the name with characters ' ', '_', ':' replaced by the undescore.
* `default(Value)` - Specifies the default values if no other means provides a value. If the `Value` is of the form `context(Variable)`, then 
      context variable `Variable` is evaluated for the default value. Infinite dependency cycles are not resolved, so be carefull here.
* `cli_command(Commands)` - `Commands` is list of CLI commands (see `execute_cli/1`), for which the variable is intended. This information
  is only used to describe usage of the command. Either option `short` or `long˙, and option `describe˙ must be provided to provide any
  help information about the variable. The option `cli_command([])` is interpreted as the option for all CLI commands. 

### `context_variable_value(+Variable, -Value)` is semidet

Unifies Value with the contextual variable. The Variable must be declared using
`context_variable/3` call.
To resolve the contextual variable following steps are executed. The first step
that succed will determine the Value

* Check for short or long variant of command line arguments name associated with the variable
* Check for the environment variable name
* Check if the Variable name or environment variable name is assigned in the `config.user.env` file
* Check if the Variable name or environment variable name is assigned in the `config.dev.env` file
* Check if the Variable name or environment variable name is assigned in the `config.env` file
* Check if the default value is provided.

The `config.env`, `config.dev.env`, and `config.env` files are looked up in  the file search paths defined as
`params(config.dev)`. The `params` path resolves to current working directory or into the `config` directory of the current working
directory. The name variants are provided by the options of the predicate context_variable/2 or derived from the context variable
name.

### `option_or_context_variable(Option, Options)` is semidet

As `option/2`, but if that fails then it behaves as `context_variable(OptionFunctor, OptionArg)`.
`Options` are checked for the module specific or local name of the option.

## Exported predicates from the module `execution_cli`

### `execute_cli` is det

Same as `current_prolog_flag(argv, Arguments), execute_cli(Arguments)`

### `execute_cli(+Arguments:list(atom))` is det

Interprets `Arguments` which are assumed to be unified  by `current_prolog_flag(argv, Arguments)` call
and calls goal registered by `register:cli_command/2` directive. The name of the command is determined 
by the first positional argument. Options are removed from the `Arguments` list by checking against 
registered context variables - see `execution_context:context_variable/3` predicate. 

Throws `cli_option(Type, OptionName)` exception if required arguments are not provided 
(see `register_cli_command/3`), or when the option that cannot be associated with any context variable 
is found on command line arguments. The exception can be used for print_message call. 

### `register_cli_command(+Command:atom, :Goal, +ArgumentsSpec:list)` is det

Registers new command for CLI. The `Goal` will be invoked when the first positional argument 
unifies with `Command`. The `Goal` is invoked as `call(Goal, PositionalArguments, Options)`
where `PositionalArguments` are all arguments that are not an command line option, and `Options`
maps some of the context variables to the goal options.

`ArgumentSpec` can contain some of the following terms:

* `describe(Message)` - provides human readable information about the command that is printed when
  the CLI command `help` or `command help` is invoked. 
* `context(ContextVariable, OptionName)` - if call to `context_variable_value(ContextVariable, Value)` 
  succeeds, then `Goal` second argument will contain option named `OptionName` with the value `Value`. 
  If the context_variable is not resolved then the exception is raised and user informed that the required
  option is missing
* `optional(context(ContextVariable, OptionName))` - similar to above, but the context variable is not
  required to be resolved
* `positional(Nth1, OptionName)` - associates `Nth1` element of positional arguments with option `OptionName`
  when calling `Goal`. If there are not enough positional argumentsthen the exception is raised and user is
  informed that argument is missing 
* `optional(positional(Nth1, OptionName))` - similar to above, but the positional argument is not
  required to be resolved

## Testing

The tests requires some command line options and environment variables, also the availability of config files. The script run-tests.ps1 prepares the environment.

## Development

To debug the module, load the `debug.pl` file into prolog top.