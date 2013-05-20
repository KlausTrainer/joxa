-module(jxat_bare_module).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a,bare,module], _State, _) ->
    Source = <<"(ns my-module ; Simple module test
                 )">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    {ok, Ctx}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch({module, 'my-module'},
                 code:load_binary('my-module', "my-module.jxa", 'joxa-cmp-ctx':'result-ctx'(Ctx))),
    ?assertMatch([{'--joxa-info',1},
                  {'--joxa-info',2},
                  {module_info,0},
                  {module_info,1}],
                 lists:sort('my-module':module_info(exports))),
    ?assertMatch([],
                 'my-module':module_info(imports)),

    {ok, Ctx};
then([the,joxa,context,for,a,bare,module,is,correctly,formed], Ctx, _) ->
    ?assertMatch('my-module', 'joxa-cmp-ctx':'namespace-name-ctx'(Ctx)),
    {ok, Ctx}.
