(* ::Package:: *)

$Package = FileNameJoin[{DirectoryName @ $InputFileName, "gym_http_client.wl"}]
Set
SetDirectory[NotebookDirectory[]]
Get["gym_http_client.wl"]


(* This script will run an agent with random actions *)

env = EnvCreate["Pong-ram-v0"];

$numEpisodes = 1;
$maxSteps = 10000;
i = 0;
states = {};
Do[
	state["observation"] = EnvReset[env]; (* reset every episode *)
	
	Do[
  		action = 2;
  		Print[action];
  		state = EnvStep[env, action, True];
  		If[state["done"], (Break[];)],
  		{step, $maxSteps}
  	];
  	Print[state],
  	{episode, $numEpisodes}
 ]
 (* close the environment when done *)
 EnvClose[env]





























