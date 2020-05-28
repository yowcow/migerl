-define(OPT_SPEC, [
                   {config,  $c, "config",  string,               "Config file"},
                   {env,     $e, "env",     {string, "default"},  "Env name in config"},
                   {dir,     $d, "dir",     {string, "scripts"},  "Migration script directory"},
                   {title,   $t, "title",   {string, "untitled"}, "Migration title"},
                   {all,     $a, "all",     {boolean, false},     "Apply/Unapply all"},
                   {version, $v, "version", {boolean, false},     "Print version"},
                   {help,    $h, "help",    {boolean, false},     "Print this help"}
                  ]).

-define(ORDER_FILE, "order.yml").

-define(UTF8_CHECK, [226, 156, 147]).
-define(UTF8_CROSS, [226, 156, 152]).
-define(UTF8_CIRCL, "_").
