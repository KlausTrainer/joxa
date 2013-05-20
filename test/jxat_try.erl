-module(jxat_try).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,contains,'\'try\''], _State, _) ->
     Source = <<"(ns jxat-try-test
                    (require erlang io
                             (joxa-core :as core)))


                (defn+ try-test1()
                    (core/try
                      :ok
                      (erlang/throw :something)
                      (catch
                        ({:throw :something}
                              :got-it))))

                (defn+ try-test2()
                    (core/try
                      :ok
                      (erlang/exit :something)
                      (catch
                        ({:exit :something}
                              :got-it))))

                (defn+ try-test3()
                    (core/try
                      :ok1
                      :ok2
                      :ok3
                      (catch
                        ({:exit :something}
                              :got-it))))
">>,

    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    {ok, Ctx}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch({module, 'jxat-try-test'},
                 code:load_binary('jxat-try-test', "jxat-try-test.jxa", 'joxa-cmp-ctx':'result-ctx'(Ctx))),
    ?assertMatch(false, 'joxa-compiler':'has-errors?'(Ctx)),
    {ok, Ctx};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch('got-it', 'jxat-try-test':'try-test1'()),
    ?assertMatch('got-it', 'jxat-try-test':'try-test2'()),
    ?assertMatch(ok3, 'jxat-try-test':'try-test3'()),
    {ok, State}.
