FROM abinnewies/erlang

RUN mkdir -p /usr/src/app
COPY rebar.* run.sh /usr/src/app/
COPY src/ /usr/src/app/src/
COPY config/ /usr/src/app/config/

RUN cd /usr/src/app \
    && rebar3 release

RUN cd /usr/src/app/_build/default/lib/jiffy/ \
    && make \
    && mkdir /usr/src/app/_build/default/rel/secure_chat/lib/jiffy-0.14.7/priv/ \
    && cp /usr/src/app/_build/default/lib/jiffy/priv/jiffy.so /usr/src/app/_build/default/rel/secure_chat/lib/jiffy-0.14.7/priv/

EXPOSE 49165 4369 9001 9002 9003 9004 9005

RUN mkdir -p /var/data/mnesia
RUN chmod 0777 /var/data/mnesia
VOLUME [ "/var/data/mnesia" ]
