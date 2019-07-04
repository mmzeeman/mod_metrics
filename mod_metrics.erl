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
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    init/1,

    install_tables/1,

    observe_admin_menu/3
]).

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_metrics_,
                parent=admin_system,
                label=?__("Metrics", Context),
                url={admin_metrics},
                visiblecheck={acl, use, mod_metrics}}
    | Acc].

init(Context) ->
    ok = ensure_tables(Context),
    ok = start_reporter(Context),

    Site = z_context:site(Context),

    %% Register to all datapoints of the site.
    [ ok = exometer_report:subscribe(
             reporter_name(Context),
             {select, 
              [{ {[zotonic, Site| '_'], DataPoint, enabled}, [], ['$_'] }]},
             datapoints(DataPoint),
             10000) || DataPoint <- datapoints()],

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
    case z_db:table_exists(report, Context) of
        true -> ok;
        false -> install_tables(Context)
    end.


install_tables(Context) ->
    F = fun(Ctx) ->
                z_db:q("
                    CREATE TABLE report (
                        id BIGSERIAL NOT NULL,
                        created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),

                        CONSTRAINT report_pkey PRIMARY KEY (id)
                    )", Ctx),

                z_db:q("
                    CREATE TABLE metric (
                       id SERIAL NOT NULL,
                       metric jsonb NOT NULL,
                       type varchar(15),

                       CONSTRAINT metric_pkey PRIMARY KEY (id)
                    )", Ctx),

                z_db:q("
                    CREATE TABLE datapoint (
                        report_id int NOT NULL,
                        metric_id int NOT NULL,

                        datapoint jsonb NOT NULL,

                        CONSTRAINT datapoint_pkey PRIMARY KEY (report_id, metric_id),

                        CONSTRAINT fk_metric_id FOREIGN KEY (metric_id)
                            REFERENCES metric(id)
                                ON DELETE CASCADE
                                ON UPDATE CASCADE,

                        CONSTRAINT fk_report_id FOREIGN KEY (report_id)
                            REFERENCES report(id)
                                ON DELETE CASCADE
                                ON UPDATE CASCADE
                    )", Ctx)
        end,

    ?DEBUG(z_db:transaction(F, Context)),
    ok.

%%
%% Helpers
%%

datapoints() ->
    [counter, spiral, gauge, histogram, meter].

datapoints(counter) ->[value];
datapoints(spiral) -> [count, one];
datapoints(gauge) -> [value];
datapoints(histogram) -> [mean, min, max, 50, 95, 99, 999];
datapoints(meter) -> [count, one, five, fifteen, day, mean].
