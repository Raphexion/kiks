-module(ssh_executor).

%% -behaviour(gen_server).
%%
%% -export([init/1,
%% 	 handle_call/3,
%% 	 handle_cast/2,
%% 	 handle_info/2,
%% 	 terminate/2,
%% 	 code_change/3]).
%%
%% http://erlang.org/doc/apps/ssh/using_ssh.html
%%
%% init(#{host := Host, port := Port, cmd := Cmd}) ->
%%     ssh:start(),
%%     {ok, ConnectionRef} = ssh:connect("tarlop", 22, []),
%%     {ok, ChannelId} =  ssh_connection:session_channel(ConnectionRef, infinity),
%%     {ok, #{ref => Ref, cid => ChannelId}}.
%%
%% handle_call({exec, Cmd}, _From, State=#{ref := Ref, cid := Cid}) ->
%%     ssh_connection:exec(ConnectionRef, ChannelId, "pwd", infinity),
%%     {reply,
%%       5>  flush().
%%       Shell got {ssh_cm,<0.57.0>,{data,0,0,<<"/home/otptest\n">>}}
%%       Shell got {ssh_cm,<0.57.0>,{eof,0}}
%%       Shell got {ssh_cm,<0.57.0>,{exit_status,0,0}}
%%       Shell got {ssh_cm,<0.57.0>,{closed,0}}
%%       ok
%%
