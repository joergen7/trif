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

%% ==================================================================
%% Definitions
%% ==================================================================

Definitions.

LPAREN = \(
RPAREN = \)
TRUE   = #t
FALSE  = #f

ID          = [A-Za-z][A-Za-z0-9\.\-_\?!/\*]*

COMMENT     = ;.*
WS          = [\000-\s]


%% ==================================================================
%% Rules
%% ==================================================================

Rules.

{LPAREN} : {token, {lparen, TokenLine, TokenChars}}.
{RPAREN} : {token, {rparen, TokenLine, TokenChars}}.
{TRUE}   : {token, {true, TokenLine, TokenChars}}.
{FALSE}  : {token, {false, TokenLine, TokenChars}}.
{ID}     : {token, {id, TokenLine, TokenChars}}.

{COMMENT}     : skip_token.
{WS}          : skip_token.


%% ==================================================================
%% Erlang Code
%% ==================================================================

Erlang code.

