-define(OPT_SPEC, [
    {config, $c, "config",  string,               "Config file"},
    {env,    $e, "env",     {string, "default"},  "Env name in config"},
    {dir,    $d, "dir",     {string, "scripts"},  "Migration script directory"},
    {title,  $t, "title",   {string, "untitled"}, "Migration title"},
    {all,    $a, "all",     {boolean, false},     "Apply/Unapply all"},
    {dryrun, $n, "dry-run", {boolean, false},     "Dry-run"},
    {help,   $h, "help",    {boolean, false},     "Print this help"}
]).
