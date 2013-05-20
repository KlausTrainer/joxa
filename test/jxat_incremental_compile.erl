%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric B Merritt
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2011 by Eric B Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(jxat_incremental_compile).

%% API
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================
incremental_test() ->
      Source = <<"(ns jxat-incremental-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))

                (defn post-test ()
                    (fn (foo) foo))">>,
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    ?assertMatch({module, 'jxat-incremental-test'}, code:load_binary('jxat-incremental-test', "jxat-incremental-test.jxa", Beam)),
    ?assertMatch(1, 'jxat-incremental-test':'do-test'(1)),
    ?assertMatch(foo, 'jxat-incremental-test':'do-test'(foo)),
    ?assertMatch(bar, 'jxat-incremental-test':'do-test'(bar)).


incremental_fail_test() ->
      Source = <<"(ns jxat-incremental-fail-test
                    (require io)
                    (use (erlang :only (==/2 and/2))))

                (defspec post-test () (erlang/term))

                (defn+ do-test (a)
                    ((post-test) a))">>,
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    ?assertMatch(true, 'joxa-compiler':'has-errors?'(Ctx)),
    ?assertMatch([{{'undefined-functions',[{'post-test',0}]},
                   {[],{0,0}}}],
                 'joxa-cmp-ctx':'errors-ctx'(Ctx)),
    'joxa-cmp-ctx':'stop-context'(Ctx).




%%%===================================================================
%%% Support Functions
%%%===================================================================
