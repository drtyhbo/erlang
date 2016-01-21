-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behavior(supervisor).

from django.conf.urls import patterns, url

urlpatterns = patterns('dictionary.views',
    url(r'words/$', 'words', name='words'),
)