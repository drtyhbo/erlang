-module(secure_chat_mongo).
-behaviour(gen_server).

-record(mongo_state, {topo}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([find_one/2, find_one/3]).
-export ([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

find_one(Collection, Selector) ->
  gen_server:call(?MODULE, {find_one, Collection, Selector, #{}}).

find_one(Collection, Selector, Projector) ->
  gen_server:call(?MODULE, {find_one, Collection, Selector, Projector}).

start_link(Args) ->
  gen_server:start_link({local, secure_chat_mongo}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{seed, [Seed]}, {option, Option}, {woption, WOption}] = _) ->
  {ok, Topology} = mongoc:connect(Seed, Option, WOption),
  {ok, #mongo_state{topo = Topology}}.

handle_call({find_one, Collection, Selector, Projector}, _From, #mongo_state{topo = Topology} = State) ->
  Fun = fun (Worker) ->
          mongoc:find_one(Worker, Collection, Selector, Projector, 0)
        end,
  Res = mongoc:transaction_query(Topology, Fun),
  {reply, {ok, Res}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
