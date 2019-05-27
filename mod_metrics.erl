%% Copyright 2019 Maas-Maarten Zeeman
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

-module(mod_metrics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Store Metrics").
-mod_description("Stores metrics in Postgres, and lets the db handle downsampling and expiration").
-mod_prio(1000).

-export([
    init/1
]).

init(Context) ->
    case z_db:table_exists(metrics, Context) of
        true -> ok;
        false ->
            install_metric_table(Context)
    end.

install_metric_table(Context) ->
    z_db:q("
        CREATE TABLE metrics (
           id SERIAL NOT NULL,
           
           rsc_id INTEGER NOT NULL,

           timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
           value INTEGER NOT NULL,

           constraint metrics_pkey primary key (id)
           )
    ", Context),

    ok.






