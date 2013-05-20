-module(jxat_module_fun_line_support).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,module,that,has,a,function,that,calls,
       module], _State, _) ->
    Source = <<"
(ns jxat-module-fun)

   (defn+ get-namespace ()
     ($namespace))

   (defn+ get-fun()
     ($function-name))

   (defn+ get-line()
     ($line-number))

   (defn+ test-case ()
     (case (get-namespace)
       (:jxat-module-fun
           :ok))
     (case (get-fun)
       (:get-fun
           :ok))
     (case (get-line)
       (11
        :ok)))
">>,
    {ok, Source}.


'when'([joxa,is,called,on,this,module], Source, _) ->
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    {ok, Ctx}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch(false, 'joxa-compiler':'has-errors?'(Ctx)),
    ?assertMatch({module, 'jxat-module-fun'},
                 code:load_binary('jxat-module-fun', "jxat-module-fun.jxa", 'joxa-cmp-ctx':'result-ctx'(Ctx))),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {'get-fun',0},
                  {'get-line',0},
                  {'get-namespace',0},
                  {module_info,0},
                  {module_info,1},
                  {'test-case',0}],
                 lists:sort('jxat-module-fun':module_info(exports))),
    'joxa-cmp-ctx':'stop-context'(Ctx),
    {ok, Ctx};
then([the,described,function,returns,the,name,'of',the,module],
     State, _) ->
    ?assertMatch('jxat-module-fun', 'jxat-module-fun':'get-namespace'()),
    ?assertMatch('get-fun', 'jxat-module-fun':'get-fun'()),
    ?assertMatch(11, 'jxat-module-fun':'get-line'()),
    'jxat-module-fun':'test-case'(),
    {ok, State}.
