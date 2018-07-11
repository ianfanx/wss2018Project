(* ::Package:: *)

$Package = FileNameJoin[{DirectoryName @ $InputFileName, "gym_http_client.wl"}]
Get[$Package]


(* This script will run an agent with random actions *)

env = EnvCreate["CartPole-v0"]

$numEpisodes = 1;
$maxSteps = 20;
i = 0;
Do[
	Print[EnvReset[env]]; (* reset every episode *)
	
	Do[
  		action = EnvActionSpaceSample[env];
  		state = EnvStep[env, action, True];
  		Print[action];
  		Print[state];
  		If[state["done"], (i = i+step;Break[];)],
  		{step, $maxSteps}
  	],
  	{episode, $numEpisodes}
 ]
 Print[i]
 (* close the environment when done *)
 EnvClose[env]









