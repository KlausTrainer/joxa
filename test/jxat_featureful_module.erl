-module(jxat_featureful_module).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").
given([a,featureful,module], _State, _) ->
    Source = <<"(ns jxat-featureful
              (use string code)
               (attr sfoo 123)
               (use (lists :only (member/2 all/2)
                     :rename ((member/2 mbr))))
               (use (file
                     :exclude (delete/1)
                     :rename ((change_group/2 chgrp)
                                (change_mode/2 chmod))))
               (attr super_duper \"Hello World\")
              (require (proplists :as props))
                (require erlang code)
                  (use (filename :exclude (flatten/1 append/2 join/2
                                           absname/1 absname_join/2))))">>,
    {ok, Source}.

'when'([joxa,is,called,on,this,module], Source, _) ->
    {ok, Ctx} = 'joxa-cmp-ctx':'start-context'(),
    {Ast, Path} = 'joxa-compiler':'do-parse'(Ctx, Source),
    {ok, _Beam} = 'joxa-compiler':forms(Ast, [], Path, Ctx),
    {ok, Ctx}.

then([a,beam,binary,is,produced], Ctx, _) ->
    ?assertMatch({module, 'jxat-featureful'},
                 code:load_binary('jxat-featureful', "jxat-featureful", 'joxa-cmp-ctx':'result-ctx'(Ctx))),
    {ok, Ctx};
then([the,joxa,context,for,a,featureful,module,is,correctly,formed], Ctx, _) ->
    validate_module(string, Ctx, [{join,2}]),
    validate_module(code, Ctx, []),
    validate_lists(Ctx),
    validate_file(Ctx),
    validate_filename(Ctx),
    Required = 'joxa-cmp-ctx':'requires-ctx'(Ctx),
    Alias = 'joxa-cmp-ctx':'aliases-ctx'(Ctx),
    _Attrs = 'joxa-cmp-ctx':'attrs-ctx'(Ctx),
    ?assertMatch(true, ec_dictionary:has_key({proplists, split, 2}, Required)),
    ?assertMatch(true, ec_dictionary:has_key({erlang, integer_to_list, 2}, Required)),
    ?assertMatch(true, ec_dictionary:has_key({code, which, 1}, Required)),
    ?assertMatch(proplists, ec_dictionary:get(props, Alias)),
    ?assertMatch("Hello World",
                 proplists:get_value(super_duper,
                                     'jxat-featureful':module_info(attributes))),

    ?assertMatch(123,
                 proplists:get_value(sfoo,
                                     'jxat-featureful':module_info(attributes))),
    'joxa-cmp-ctx':'stop-context'(Ctx),
    {ok, Ctx}.

validate_module(Module, Ctx, Exclude) ->
    %% module_info causes problems and is mostly ignored
    Exports = [El || El={Fun, _}
                         <- Module:module_info(exports),
                     Fun =/= module_info],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity,
                                                    Exclude)],
    Used = 'joxa-cmp-ctx':'uses-ctx'(Ctx),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, Module},
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports).

validate_lists(Ctx) ->
    Required = [{all, 2}],
    Exports = [El || El={Fun, _}
                         <- lists:module_info(exports),
                     Fun =/= module_info],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity, Required)],
    Used = 'joxa-cmp-ctx':'uses-ctx'(Ctx),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, lists},
                                       ec_dictionary:get(Export, Used))
                  end, Required),

    ?assertMatch({member, lists},
                 ec_dictionary:get({mbr, 2}, Used)),
    RemovedConflicts = [Export || Export <- FilteredExports,
                                  not lists:member(Export, [{flatten,1},
                                                            {append,2}])],
    lists:foreach(fun(Export) ->
                          ?assertThrow(not_found,
                                       ec_dictionary:get(Export, Used))
                  end,
                  %% Append/2 actually exists in file name and gets imported from there.
                  RemovedConflicts).

validate_file(Ctx) ->
    DescUsed = [{{chgrp, 2}, change_group},
                {{chmod, 2}, change_mode}],
    Exports = [El || El={Fun, _}
                         <- file:module_info(exports),
                     Fun =/= module_info],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity,
                                                    [{delete, 1},
                                                     {change_group, 2},
                                                     {change_mode, 2}])],
    Used = 'joxa-cmp-ctx':'uses-ctx'(Ctx),
    lists:foreach(fun({Export, Target}) ->
                          ?assertMatch({Target, file},
                                       ec_dictionary:get(Export, Used))
                  end, DescUsed),
    lists:foreach(fun(Export={Fun, _}) ->
                          ?assertMatch({Fun, file},
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports).

validate_filename(Ctx) ->
    Exports = [El || El={Fun, _}
                         <- filename:module_info(exports),
                     Fun =/= module_info],
    DescExclude = [{absname, 1},
                   {append, 2},
                   {join,2},
                   {flatten, 1},
                   {absname_join, 2}],
    FilteredExports = [FunArity || FunArity <- Exports,
                                   not lists:member(FunArity,
                                                    DescExclude)],
    Used = 'joxa-cmp-ctx':'uses-ctx'(Ctx),
    lists:foreach(fun(Export={Target, _}) ->
                          ?assertMatch({Target, filename},
                                       ec_dictionary:get(Export, Used))
                  end, FilteredExports),

    lists:foreach(fun(Export) ->
                          ?assertThrow(not_found,
                                       ec_dictionary:get(Export, Used))
                  end, [{absname, 1}, {absname_join, 2}]).
