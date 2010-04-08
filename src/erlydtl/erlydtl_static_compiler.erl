-module(erlydtl_static_compiler).

-export([compile_js/4]).

compile_js(Files, Dir, CompileCmd, _Enabled) ->
    io:format("compile_js", []),
    CompressedJs = case CompileCmd of
        undefined -> <<>>;
        Cmd -> os:cmd(io_lib:format(Cmd, [string:join([filename:join(Dir, F) || F <- Files], " ")]))
    end,
    Output = list_to_binary([
        <<"<script type=\"text/javascript\"><!--">>, $\n,
        CompressedJs, $\n,
        <<"--></script>">>
    ]),
    Output.
