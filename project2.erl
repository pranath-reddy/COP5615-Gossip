%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-2

%% Usage:
%% c(project2).
%% project2:main(500, "full", "gossip", 0.9).
%% Restart shell after each execution

-module(project2).
-export([main/4]).
-export([actor/10]).
-export([counter/1,increment/1,value/1]).

% To count the number of dead actors
increment(Counter) ->
  Counter ! increment.
value(Counter) ->
  Counter ! {self(),value},
  receive
    {Counter,Value} ->
      Value
  end.
counter(Val) ->
  receive
    increment ->
      counter(Val + 1);
    {From,value} ->
      From ! {self(),Val},
      counter(Val)
  end.

% Spawns the required number of actors
spawner(0, _N, _Sum, _Weight, _A, _T, _Start_time, _Counter, _C) -> io:fwrite("Spawned All Actors\n");
spawner(ID, N, _Sum, _Weight, A, _T, _Start_time, Counter, _C) ->
  % Args:
  % ID: ID of an actor
  % N: Total Number of Actors
  % Sum: for push-sum
  % Weight: for push-sum
  % A: algorithm
  % T: topology
  % Start_time: start time of program
  % Counter: keep track of converged nodes
  % C: required rate of convergence for gossip
  if
    A == "gossip" ->
      PID = spawn(project2, actor, [10, ID, N, _Sum, _Weight, A, _T, _Start_time, Counter, _C]);
    A == "push-sum" ->
      PID = spawn(project2, actor, [3, ID, N, _Sum, _Weight, A, _T, _Start_time, Counter, _C])
  end,
  register(list_to_atom("actor" ++ integer_to_list(ID)), PID),
  spawner(ID-1, N, _Sum-1, _Weight, A, _T, _Start_time, Counter, _C).

main(N, T, A, C) ->
  % Args:
  % N: Total Number of Actors
  % T: topology
  % A: algorithm
  % C: required rate of convergence for gossip
  io:fwrite("numNodes: ~p\n", [N]),
  io:fwrite("Required Gossip Rate of Convergence: ~p\n", [C]),
  if
    T == "full" ->
      if
        A == "gossip" ->
          io:fwrite("topology: Full Network\n"),
          io:fwrite("algorithm: Gossip\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          spawner(N, N, 0, 0, "gossip", "full", Start_time, Counter, C),
          gossip(N);
        A == "push-sum" ->
          io:fwrite("topology: Full Network\n"),
          io:fwrite("algorithm: Push-Sum\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          spawner(N, N, N, 1, "push-sum", "full", Start_time, Counter, C),
          push_sum(N)
      end;

    T == "2D" ->
      if
        A == "gossip" ->
          io:fwrite("topology: 2D grid\n"),
          io:fwrite("algorithm: Gossip\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          NS = round(math:pow(math:ceil(math:sqrt(N)),2)),
          io:fwrite("Rounded numNodes: ~p\n", [NS]),
          spawner(NS, NS, 0, 0, "gossip", "2D", Start_time, Counter, C),
          gossip(NS);
        A == "push-sum" ->
          io:fwrite("topology: 2D grid\n"),
          io:fwrite("algorithm: Push-Sum\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          NS = round(math:pow(math:ceil(math:sqrt(N)),2)),
          io:fwrite("Rounded numNodes: ~p\n", [NS]),
          spawner(NS, NS, NS, 1, "push-sum", "2D", Start_time, Counter, C),
          push_sum(NS)
      end;

    T == "line" ->
      if
        A == "gossip" ->
          io:fwrite("topology: Line\n"),
          io:fwrite("algorithm: Gossip\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          spawner(N, N, 0, 0, "gossip", "line", Start_time, Counter, C),
          gossip(N);
        A == "push-sum" ->
          io:fwrite("topology: Line\n"),
          io:fwrite("algorithm: Push-Sum\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          spawner(N, N, N, 1, "push-sum", "line", Start_time, Counter, C),
          push_sum(N)
      end;

    T == "imp2D" ->
      if
        A == "gossip" ->
          io:fwrite("topology: Imperfect 2D Grid\n"),
          io:fwrite("algorithm: Gossip\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          NS = round(math:pow(math:ceil(math:sqrt(N)),2)),
          io:fwrite("Rounded numNodes: ~p\n", [NS]),
          spawner(NS, NS, 0, 0, "gossip", "imp2D", Start_time, Counter, C),
          gossip(NS);
        A == "push-sum" ->
          io:fwrite("topology: Imperfect 2D Grid\n"),
          io:fwrite("algorithm: Push-Sum\n"),
          Start_time = erlang:system_time(millisecond),
          Counter = spawn(project2, counter, [0]),
          NS = round(math:pow(math:ceil(math:sqrt(N)),2)),
          io:fwrite("Rounded numNodes: ~p\n", [NS]),
          spawner(NS, NS, NS, 1, "push-sum", "imp2D", Start_time, Counter, C),
          push_sum(NS)
      end
    end.

% Gossip
gossip(N) ->
  Random_ID = whereis(list_to_atom("actor" ++ integer_to_list(rand:uniform(N)))),
  if
    Random_ID == undefined ->
      gossip(N);
    Random_ID /= undefined ->
      Random_ID ! {fact}
  end.

% Push Sum
push_sum(N) ->
  io:fwrite("Started Push Sum\n"),
  whereis(list_to_atom("actor" ++ integer_to_list(rand:uniform(N)))) ! {fact, 0, 0}.

% Checks if all Neighbors of a participant are dead
master(0, A, N, C, _Neighbors, Start_time, Counter) ->
  Curr_Count = value(Counter),
  Thresh = round(math:ceil(N*C)),
  if
    A == "gossip" ->
      if
        Curr_Count < Thresh ->
          gossip(N);
        true ->
          io:fwrite("Number Of Dead Nodes: ~p\n", [value(Counter)]),
          io:fwrite("Total Time: ~p\n", [erlang:system_time(millisecond) - Start_time]),
          erlang:halt()
      end;
    true ->
      io:fwrite("Number Of Dead Nodes: ~p\n", [value(Counter)]),
      io:fwrite("Total Time: ~p\n", [erlang:system_time(millisecond) - Start_time]),
      erlang:halt()
  end;
master(Count, _A, _N, _C, Neighbors, _Start_time, _Counter) ->
  N = lists:nth(Count, Neighbors),
  NID = whereis(list_to_atom("actor" ++ integer_to_list(N))),
  if
    NID == undefined ->
      master(Count-1, _A, _N, _C, Neighbors, _Start_time, _Counter);
    true ->
      done
  end.

actor(0, _ID, _N, _Sum, _Weight, _A, _T, _Start_time, Counter, _C) ->
  increment(Counter),
  done;
actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C) ->
  % Args:
  % Count: count for convergence (10 for Gossip, 3 for push-sum)
  % ID: ID of the actor
  % N: Total Number of Actors
  % Sum: for push-sum
  % Weight: for push-sum
  % A: algorithm
  % Start_time: start time of program
  % T: topology
  % Counter: keep track of converged nodes
  % C: required rate of convergence for gossip
  if
    A == "gossip" ->
      receive
        {fact} ->
          if
            T == "full" ->
              Neighbors = [X || X <- lists:seq(1, N), X /= ID],
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact},
                  actor(Count-1, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C)
              end;

            T == "line" ->
              if
                ID == N ->
                  Neighbors = [ID-1];
                ID == 1 ->
                  Neighbors = [ID+1];
                true ->
                  Neighbors = [ID+1, ID-1]
              end,
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact},
                  actor(Count-1, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C)
              end;

            T == "2D" ->
              Side = erlang:trunc(math:sqrt(N)),
              if
                % get Neighbors along row
                ID rem Side == 1 ->
                  % actor in first column
                  Row_Neighbors = [ID+1];
                ID rem Side == 0 ->
                  % actor in last column
                  Row_Neighbors = [ID-1];
                true ->
                  % all other actors
                  Row_Neighbors = lists:append([[ID-1], [ID+1]])
              end,

              if
                % get Neighbors along column
                ID + Side > N ->
                  % actor in last row
                  Neighbors = lists:append([Row_Neighbors, [ID-Side]]);
                ID - Side < 1 ->
                  % actor in first row
                  Neighbors = lists:append([Row_Neighbors, [ID+Side]]);
                true ->
                  % all other actors
                  Neighbors = lists:append([Row_Neighbors, [ID-Side], [ID+Side]])
              end,

              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact},
                  actor(Count-1, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C)
              end;

            T == "imp2D" ->
              Side = erlang:trunc(math:sqrt(N)),
              if
                % get Neighbors along row
                ID rem Side == 1 ->
                  % actor in first column
                  Row_Neighbors = [ID+1];
                ID rem Side == 0 ->
                  % actor in last column
                  Row_Neighbors = [ID-1];
                true ->
                  % all other actors
                  Row_Neighbors = lists:append([[ID-1], [ID+1]])
              end,

              if
                % get Neighbors along column
                ID + Side > N ->
                  % actor in last row
                  Col_Neighbors = lists:append([Row_Neighbors, [ID-Side]]);
                ID - Side < 1 ->
                  % actor in first row
                  Col_Neighbors = lists:append([Row_Neighbors, [ID+Side]]);
                true ->
                  % all other actors
                  Col_Neighbors = lists:append([Row_Neighbors, [ID-Side], [ID+Side]])
              end,

              Neighbors = lists:append([Col_Neighbors, [rand:uniform(N)]]),
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact},
                  actor(Count-1, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C)
              end

          end
      end;

    A == "push-sum" ->
      receive
        {fact, S, W} ->
          Threshold = math:pow(10, -10),
          if
            T == "full" ->
              if
                W == 0 ->
                  Ratio = (Sum + S)/(Weight + W);
                true ->
                  Ratio = (Sum + S)/(Weight + W) - S/W
              end,
              Neighbors = [X || X <- lists:seq(1, N), X /= ID],
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact, S, W},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact, (Sum + S)/2, (Weight + W)/2},
                  if
                      abs(Ratio) < Threshold ->
                        actor(Count-1, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C);
                      true ->
                        actor(Count, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C)
                  end
              end;

            T == "line" ->
              if
                W == 0 ->
                  Ratio = (Sum + S)/(Weight + W);
                true ->
                  Ratio = (Sum + S)/(Weight + W) - S/W
              end,
              if
                ID == N ->
                  Neighbors = [ID-1];
                ID == 1 ->
                  Neighbors = [ID+1];
                true ->
                  Neighbors = [ID+1, ID-1]
              end,
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact, S, W},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact, (Sum + S)/2, (Weight + W)/2},
                  if
                      abs(Ratio) < Threshold ->
                        actor(Count-1, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C);
                      true ->
                        actor(Count, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C)
                  end
              end;

            T == "2D" ->
              Side = erlang:trunc(math:sqrt(N)),
              if
                W == 0 ->
                  Ratio = (Sum + S)/(Weight + W);
                true ->
                  Ratio = (Sum + S)/(Weight + W) - S/W
              end,
              if
                % get Neighbors along row
                ID rem Side == 1 ->
                  % actor in first column
                  Row_Neighbors = [ID+1];
                ID rem Side == 0 ->
                  % actor in last column
                  Row_Neighbors = [ID-1];
                true ->
                  % all other actors
                  Row_Neighbors = lists:append([[ID-1], [ID+1]])
              end,

              if
                % get Neighbors along column
                ID + Side > N ->
                  % actor in last row
                  Neighbors = lists:append([Row_Neighbors, [ID-Side]]);
                ID - Side < 1 ->
                  % actor in first row
                  Neighbors = lists:append([Row_Neighbors, [ID+Side]]);
                true ->
                  % all other actors
                  Neighbors = lists:append([Row_Neighbors, [ID-Side], [ID+Side]])
              end,

              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact, S, W},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact, (Sum + S)/2, (Weight + W)/2},
                  if
                      abs(Ratio) < Threshold ->
                        actor(Count-1, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C);
                      true ->
                        actor(Count, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C)
                  end
              end;

            T == "imp2D" ->
              Side = erlang:trunc(math:sqrt(N)),
              if
                W == 0 ->
                  Ratio = (Sum + S)/(Weight + W);
                true ->
                  Ratio = (Sum + S)/(Weight + W) - S/W
              end,
              if
                % get Neighbors along row
                ID rem Side == 1 ->
                  % actor in first column
                  Row_Neighbors = [ID+1];
                ID rem Side == 0 ->
                  % actor in last column
                  Row_Neighbors = [ID-1];
                true ->
                  % all other actors
                  Row_Neighbors = lists:append([[ID-1], [ID+1]])
              end,

              if
                % get Neighbors along column
                ID + Side > N ->
                  % actor in last row
                  Col_Neighbors = lists:append([Row_Neighbors, [ID-Side]]);
                ID - Side < 1 ->
                  % actor in first row
                  Col_Neighbors = lists:append([Row_Neighbors, [ID+Side]]);
                true ->
                  % all other actors
                  Col_Neighbors = lists:append([Row_Neighbors, [ID-Side], [ID+Side]])
              end,

              Neighbors = lists:append([Col_Neighbors, [rand:uniform(N)]]),
              RandN = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
              NID = whereis(list_to_atom("actor" ++ integer_to_list(RandN))),
              if
                NID == undefined ->
                  master(length(Neighbors), A, N,  _C, Neighbors, Start_time, _Counter),
                  whereis(list_to_atom("actor" ++ integer_to_list(ID))) ! {fact, S, W},
                  actor(Count, ID, N, Sum, Weight, A, T, Start_time, _Counter, _C);
                true ->
                  whereis(list_to_atom("actor" ++ integer_to_list(RandN))) ! {fact, (Sum + S)/2, (Weight + W)/2},
                  if
                      abs(Ratio) < Threshold ->
                        actor(Count-1, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C);
                      true ->
                        actor(Count, ID, N, (Sum + S)/2, (Weight + W)/2, A, T, Start_time, _Counter, _C)
                  end
              end

          end
      end
    end.
