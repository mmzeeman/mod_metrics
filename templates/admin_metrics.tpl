{% extends "admin_base.tpl" %}

{% block title %}{_ Metrics _}{% endblock %}

{% block content %}
<div class="admin_header">
    <h2>{_ Metrics _}</h2>
</div>

<div {% include "_language_attrs.tpl" language=`en` %}
     {% for metric in m.metrics.all %}
         {{ metric | pprint }}
     {% endfor %}
</div>

{% endblock %}
