%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.n>
%% @copyright 2019 Maas-Maarten Zeeman
%% @doc Overview of metrics.

-module(controller_admin_metrics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_admin_controller_helper:is_authorized(mod_metrics, ReqData, Context).        

html(Context) ->
    Selected = z_context:get(selected, Context, "metrics"),
    Vars = [
            {selected, Selected}
           ],
    Template = z_context:get(template, Context, "admin_metrics.tpl"),
    Html = z_template:render(Template, Vars, Context),
    z_context:output(Html, Context).

