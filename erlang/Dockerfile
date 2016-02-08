FROM erlang:latest

ENV REBAR_VERSION beta-4

ADD https://github.com/rebar/rebar3/archive/${REBAR_VERSION}.tar.gz /usr/src/rebar-${REBAR_VERSION}.tar.gz
RUN cd /usr/src \
    && tar zxf rebar-${REBAR_VERSION}.tar.gz \
    && ls \
    && cd rebar3-${REBAR_VERSION} \
    && ./bootstrap \
    && cp rebar3 /usr/local/bin \
    && cd /usr/src \
    && rm -Rf /usr/src/rebar3-${REBAR_VERSION} \
    && rm /usr/src/rebar-${REBAR_VERSION}.tar.gz
