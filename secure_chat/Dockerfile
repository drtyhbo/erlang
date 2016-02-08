FROM abinnewies/erlang

RUN mkdir -p /usr/src/app
COPY . /usr/src/app

RUN cd /usr/src/app \
    && rebar3 release

EXPOSE 49165 4369 9001 9002 9003 9004 9005

RUN mkdir -p /var/data/mnesia
RUN chmod 0777 /var/data/mnesia
VOLUME [ "/var/data/mnesia" ]