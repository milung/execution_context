$env:EC_TEST='my variable'
$env:ec_module_setting='setting'

prolog -s execution_context.plt -g run_tests,halt -t 'halt(1)' --  --cli-flag --no-negated -f --variable-long-a="long A"  --flag-ex=false -S short --long long --module-default=module