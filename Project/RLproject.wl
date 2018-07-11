(* ::Package:: *)

BeginPackage["RLporject`"]


randomgame::usage =
        "play Ep number of Cart pole game with random agent, return states";

game::usage =
        "play Ep number of Cart pole game with given network, return states"

calculateReward::usage = 
		"calculate the discounted reward of list of reward" 

preprocess::usage = 
		"preocess data to required format"

generator::usage = 
		"generate processed data"
        
Get["/Users/ianfan/Documents/gym-http-api/binding-wl/gym_http_client.wl"];
Begin["`Private`"]
(*---------------------*)
randomgame[ep_Integer,st_Integer]:= Module[{
		env,states,
		state,ob,ac,re,action
	},
	env = EnvCreate["CartPole-v0"];
	states = <|"observation"->{},"action"->{},"reward"->{}|>;
	Do[
		state["observation"] = EnvReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		Do[
			action =EnvActionSpaceSample[env];
			AppendTo[ob,state["observation"]];
			AppendTo[ac,action];
			state = EnvStep[env, action, True];
			AppendTo[re,state["reward"]];
			If[state["done"],Break[]]
			,
			{step, st}];
		AppendTo[states["observation"],ob];
		AppendTo[states["action"],ac];
		AppendTo[states["reward"],re];
		,
		{episode,ep}
	];
	(* close the environment when done *)
	EnvClose[env];
	states
]
(*---------------------*)
game[ep_Integer,st_Integer,net_]:= Module[{
		env,states,
		state,ob,ac,re,action
	},
	env = EnvCreate["CartPole-v0"];
	states = <|"observation"->{},"action"->{},"reward"->{}|>;
	Do[
		state["observation"] = EnvReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		Do[
			action =net[state["observation"]];
			AppendTo[ob,state["observation"]];
			AppendTo[ac,action];
			state = EnvStep[env, action, True];
			AppendTo[re,state["reward"]];
			If[state["done"],Break[]]
			,
			{step, st}];
		AppendTo[states["observation"],ob];
		AppendTo[states["action"],ac];
		AppendTo[states["reward"],re];
		,
		{episode,ep}
	];
	(* close the environment when done *)
	EnvClose[env];
	states
]
(*---------------------*)
calculateReward[x_] := Module[{i,h},(
	i=0;
	Reverse[ReplaceList[x,{h__,___}:>(i=i+1;0.99*i*h)]]
)]
(*---------------------*)
preprocess[x_] := Module[{result},(
	result = <||>;
	result["action"] = Flatten[x["action"]];
	result["observation"] = Flatten[x["observation"],1];
	result["reward"] = Normalize[Flatten[calculateReward/@x["reward"]]];
	result
)]
(*---------------------*)
random = 0
generator := Function[(
	If[#AbsoluteBatch == 0, random = 0];
	If[And[Mod[#AbsoluteBatch,400]==0,random <= 2],(random = random + 1;processed = preprocess[randomgame[30,200]]),If[Mod[#AbsoluteBatch,400]==0 ,(processed = preprocess[game[10,200,#Net]]),0];];
	pos = RandomInteger[{1,Length[processed["action"]]},#BatchSize];
	result = <||>;
	result["Input"] = processed["observation"][[pos]];
	result["Output"] = processed["action"][[pos]];
	result["Advantage"] = processed["reward"][[pos]];
	result
)];
End[ ]

EndPackage[ ]






