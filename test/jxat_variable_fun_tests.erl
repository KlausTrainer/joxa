-module(jxat_variable_fun_tests).

-include_lib("eunit/include/eunit.hrl").

fun_var_test() ->
        Source = <<"
(ns jxat-fun-var-test )

  (defn get-it0 ()
     (let* (f (fn (a b) {a b}))
        f))

  (defn get-it1 ()
     (let* (f (fn (a b) {a b}))
        f/2))

  (defn+ test-case0 ()
      ((get-it0) 1 2))

  (defn+ test-case1 ()
      ((get-it1) 1 2))">>,

    {Ast, _Path} = 'joxa-compiler':'do-parse'(Source),
    {ok, Result} = 'joxa-compiler':forms(Ast),
    ?assertMatch({module, 'jxat-fun-var-test'}, code:load_binary('jxat-fun-var-test', "jxat-fun-var-test.jxa", Result)),
    ?assertMatch({1, 2},
                 'jxat-fun-var-test':'test-case0'()),
    ?assertMatch({1, 2},
                 'jxat-fun-var-test':'test-case1'()).
