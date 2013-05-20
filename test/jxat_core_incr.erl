-module(jxat_core_incr).

%% This is a set of tests that, in other iterations of the compiler
%% have caused erlang to segfault. They are in the enuit tests as
%% guard to protect against these kinds of problems creeping back
%% in. At first glance they seem like usless tests. However, that is
%% not the case at all.
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    Source = <<" (ns jxat-core-incr-test
                        (require (joxa-core :as core)))


                  (defn+ test-incr (val)
                      (core/incr val))

                 (defn+ test-decr (val)
                      (core/decr val))">>,

    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    ?assertMatch(false, 'joxa-compiler':'has-errors?'(Ctx)),
    ?assertMatch({module, 'jxat-core-incr-test'}, code:load_binary('jxat-core-incr-test', "jxat-core-incr-test.jxa", Beam)),
    ?assertMatch(3, 'jxat-core-incr-test':'test-incr'(2)),
    ?assertMatch(20056, 'jxat-core-incr-test':'test-incr'(20055)),
    ?assertMatch(1, 'jxat-core-incr-test':'test-decr'(2)),
    ?assertMatch(20054, 'jxat-core-incr-test':'test-decr'(20055)),
    'joxa-cmp-ctx':'stop-context'(Ctx).
