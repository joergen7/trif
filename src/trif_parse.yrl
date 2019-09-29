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

%%====================================================================
%% Symbol Declaration
%%====================================================================

Nonterminals
  seq expr.

Terminals
  lparen rparen true false id.


%%====================================================================
%% Syntax Definition
%%====================================================================

Rootsymbol seq.

seq  -> expr              : ['$1'].
seq  -> expr seq          : ['$1'|'$2'].

expr -> true              : true( '$1' ).
expr -> false             : false( '$1' ).
expr -> id                : id( '$1' ).
expr -> lparen rparen     : list( '$1', [] ).
expr -> lparen seq rparen : list( '$1', '$2' ).

%%====================================================================
%% Erlang Code
%%====================================================================

Erlang code.

-include( "trif.hrl" ).

-spec true( {true, pos_integer(), string()} ) -> {true, pos_integer()}.

true( {true, TokenLine, _} ) ->
  {true, TokenLine}.


-spec false( {false, pos_integer(), string()} ) -> {false, pos_integer()}.

false( {false, TokenLine, _} ) ->
  {false, TokenLine}.


-spec id( {id, pos_integer(), string()} ) -> {symbol, pos_integer(), binary()}.

id( {id, TokenLine, TokenChars} ) ->
  {symbol, TokenLine, list_to_binary( TokenChars )}.


-spec list( {lparen, pos_integer(), string()}, [e()] ) -> e().

list( {lparen, TokenLine, _}, [] ) ->
  {null, TokenLine};

list( I = {lparen, TokenLine, _}, [H|T] ) ->
  {cons, TokenLine, H, list( I, T )}.
