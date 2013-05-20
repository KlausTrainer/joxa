-module(jxat_hello_world).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,calls,
       'io:format',on,an,argument], _State, _) ->
    Source = <<"(ns helloworld
                 (use (io :only (format/2) :rename ((format/2 print)))))
               (defn+ hello-world (arg)
                 (print \"~p\" [arg]))">>,
    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
    {Ast, _Path} = 'joxa-compiler':'do-parse'(Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast).

then([a,beam,binary,is,produced], Beam, _) ->
    ?assertMatch({module, 'helloworld'},
                 code:load_binary('helloworld', "helloworld.jxa", Beam)),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'hello-world',1},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort(helloworld:module_info(exports))),
    {ok, Beam};
then([the,described,function,can,be,called,'and',works,correctly],
     State, _) ->
    ?assertMatch(ok, helloworld:'hello-world'("Hello World")),
    {ok, State}.
