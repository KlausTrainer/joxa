-module(jxat_rest_args).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,rest,arguments], _State, _) ->
     Source1 = <<"(ns jxat-rest-test1
                    (require erlang io))

                  (defn+ do-test0 (one &rest two)
                       {one two})

                 (defn+ do-test1 ()
                       do-test0/2)

                 (defn+ do-test2 ()
                       do-test0/5)

                 (defn do-t (a)
                     (a 1 2 3))

                 (defn+ do-test3 ()
                  (do-t do-test0/3))
                 ">>,

     Source2 = <<"(ns jxat-rest-test2
                    (require erlang io jxat-rest-test1)
                    (use (jxat-rest-test1 :only (do-test0/2) :rename ((do-test0/2 rest-test)))))

                  (defspec do-test0 () (erlang/term))

                  (defn+ call-t()
                       (do-test0))

                  (defn+ do-test0()
                    (jxat-rest-test1/do-test0 1 2 3 4 5 6 7 9 10))

                (defspec internal-test0 ((erlang/term) &rest (erlang/term)) (erlang/term))

                (defn+ do-test0.5 ()
                    (internal-test0 1 2 3 4 5 6 7 9 10))

                (defn+ internal-test0 (one &rest two)
                    {one two})

                 (defn+ do-test1()
                    (internal-test0 1 2 3 4 5 6 7 9 10))

                 (defn+ do-test2()
                     jxat-rest-test1/do-test0/2)

                 (defn+ do-test3()
                     jxat-rest-test1/do-test0/5)

                 (defn+ internal-test1 (a)
                    (a 1 2 3 4 5 6 7 8 9 0))

                 (defn+ do-test4 ()
                   (internal-test1 internal-test0/10))

                 (defn+ do-test5 ()
                   (rest-test 1 2 3 4 5 6 7 8 9 0))


">>,

    {ok, {Source1, Source2}}.

'when'([joxa,is,called,on,this,module], {Source1, Source2}, _) ->
    {Ast1, _Path1} = 'joxa-compiler':'do-parse'(Source1),
    {ok, Beam1} = 'joxa-compiler':forms(Ast1),
    {Ast2, _Path2} = 'joxa-compiler':'do-parse'(Source2),
    {ok, Beam2} = 'joxa-compiler':forms(Ast2),
    {ok, {Beam1, Beam2}}.

then([a,beam,binary,is,produced], {Beam1, Beam2}, _) ->
    ?assertMatch({module, 'jxat-rest-test1'},
                 code:load_binary('jxat-rest-test1', "jxat-rest-test1.jxa", Beam1)),
    ?assertMatch({module, 'jxat-rest-test2'},
                 code:load_binary('jxat-rest-test2', "jxat-rest-test2.jxa", Beam2)),
    {ok, {Beam1, Beam2}};
then([the,described,function,can,be,called,'and',works,correctly], State, _) ->
    ?assertMatch(2, 'jxat-rest-test1':'--joxa-info'(rest, 'do-test0')),
    ?assertMatch({1,[2,3,4,5,6,7,9,10]}, 'jxat-rest-test2':'do-test1'()),
    ?assertMatch({1,[2]}, ('jxat-rest-test1':'do-test1'())(1, 2)),
    ?assertMatch({1,[2, 3]}, 'jxat-rest-test1':'do-test3'()),
    ?assertMatch({1,[2,3,4,5]}, ('jxat-rest-test1':'do-test2'())(1, 2, 3, 4, 5)),
    ?assertMatch({1,[2]}, ('jxat-rest-test2':'do-test2'())(1, 2)),
    ?assertMatch({1,[2,3,4,5]}, ('jxat-rest-test2':'do-test3'())(1, 2, 3, 4, 5)),
    ?assertMatch({1,[2,3,4,5,6,7,8,9,0]}, 'jxat-rest-test2':'do-test4'()),
    Fun = fun(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ->
                  {A1, A2, A3, A4, A5, A6, A7, A8, A9, A10}
          end,
    ?assertMatch({1,2,3,4,5,6,7,8,9,0}, 'jxat-rest-test2':'internal-test1'(Fun)),
    ?assertMatch({1,[2,3,4,5,6,7,8,9,0]}, 'jxat-rest-test2':'do-test5'()),
    {ok, State}.
