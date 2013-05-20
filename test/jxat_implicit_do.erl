-module(jxat_implicit_do).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,an,anonymous,function], _State, _) ->
        Source = <<"(ns jxat-implicit-do-test
                       (require io))

                (defn t1 ()
                      (let* (a 1)
                         (io/format \"~p\" [a])
                         :booha))

                (defn+ do-test ()
                    (case (t1)
                      (:booha
                          (io/format \"did it\")
                           :return-it)))
                (defn+ do-test2 ()
                    (t1)
                    (do-test))">>,
    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
    {Ast, _Path} = 'joxa-compiler':'do-parse'(Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast).

then([a,beam,binary,is,produced], Beam, _) ->
    ?assertMatch({module, 'jxat-implicit-do-test'},
                 code:load_binary('jxat-implicit-do-test', "jxat-implicit-do-test.jxa", Beam)),
    {ok, Beam};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
      ?assertMatch([{'--joxa-info',1},
                    {'--joxa-info',2},
                    {'do-test',0},
                    {'do-test2',0},
                    {module_info,0},
                    {module_info,1}],
                 lists:sort('jxat-implicit-do-test':module_info(exports))),
    {ok, State}.
