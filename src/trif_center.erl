%% -*- erlang -*-
%%
%% trif: Parallel, dynamically typed functional programming language with a
%% Lisp syntax
%%
%% Copyright 2019 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.0
%% @copyright 2019
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( trif_center ).
-behaviour( gen_server ).

-export( [] ).

-include( "trif.hrl" ).

-type center_state() :: {Prog   :: e(),
                         Stall  :: #{closure() => closure()},
                         Value  :: #{closure() => closure()},
                         Active :: [{closure(), closure()}]}.

-spec step( center_state() ) => {ok, center_state()} | norule.

step( {Prog,
       Stall,
       Value,
       [{C1, {{cons, _, {symbol, _, <<"lambda">>}, _}, Env2}}|Active]} ) ->

  #{ C1 := C0 } = Stall,
  Stall1 = maps:remove( C1, Stall ),

  {Prog, Stall1, Value, Active1}

step( {Prog,
       Stall,
       Value,
       [{C0, C1 = {E1 = {cons, _, _, _}, Env1}}|Active]} ) ->

  Stall1 = Stall#{ C1 => C0 },
  Active1 = Active++[{{E, Env1}, {E, Env1}} || E <- cons_to_list( E1 )],

  {Prog, Stall1, Value, Active1};




cons_to_list( {null, _} )       -> [];
cons_to_list( {cons, _, H, T} ) -> [H|cons_to_list( T )].