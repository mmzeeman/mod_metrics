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

-include_lib("zotonic.hrl").

-export([
    init/1,

    install_tables/1
]).

init(Context) ->
    ok = ensure_tables(Context),
    ok = start_reporter(Context),

    %% 
    %%
    %?DEBUG(exometer_report:subscribe(
    %         reporter_name(Context),
    %         [zotonic, z_context:site(Context), db, duration],
    %         [n, min, max, median],
    %         10000)),

    %% All histograms

    Site = z_context:site(Context),

    ok = exometer_report:subscribe(
             reporter_name(Context),
             {select, 
              [{ {[zotonic, Site| '_'], histogram, enabled}, [], ['$_'] }]},
             datapoints(histogram),
             10000),

    ok = exometer_report:subscribe(
             reporter_name(Context),
             {select, 
              [{ {[zotonic, Site| '_'], gauge, enabled}, [], ['$_'] }]},
             datapoints(gauge),
             10000),

    ok.

%% How to figure out which metrics to listen to?

start_reporter(Context) ->
    case exometer_report:add_reporter(
           reporter_name(Context),
           [{report_bulk, true},
            {module, exometer_report_mod_metrics},
            {status, enabled},
            {context, Context}
           ]
          ) of
        ok -> ok;
        {error, already_running} -> ok
    end.

reporter_name(Context) ->
    z_utils:name_for_host(?MODULE, Context).
    
ensure_tables(Context) ->
    case z_db:table_exists(metrics, Context) of
        true -> ok;
        false -> install_tables(Context)
    end.


install_tables(Context) ->
    F = fun(Ctx) ->
                z_db:q("
                    CREATE TABLE metrics (
                        id BIGSERIAL NOT NULL,
                        created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),

                        CONSTRAINT metrics_pkey PRIMARY KEY (id)
                    )", Ctx),

                z_db:q("
                    CREATE TABLE datapoint (
                        report_id int NOT NULL,

                        metric jsonb NOT NULL,
                        datapoint jsonb NOT NULL,

                        CONSTRAINT datapoint_pkey PRIMARY KEY (report_id, metric),
                        CONSTRAINT fk_report_id FOREIGN KEY (report_id)
                            REFERENCES metrics(id)
                                ON DELETE CASCADE
                                ON UPDATE CASCADE
                    )", Ctx)
        end,

    ?DEBUG(z_db:transaction(F, Context)),
    ok.

%%
%% Helpers
%%

datapoints(counter) -> [value];
datapoints(gauge) -> [value];
datapoints(histogram) -> [mean, min, max, 50, 95, 99, 999];
datapoints(meter) -> [count, one, five, fifteen, day, mean].
