(* ::Package:: *)

BeginPackage["RLporject-atair`"]


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
		state,ob,ac,re,action, next
	},
	env = EnvCreate["Pong-ram-v0"];
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["observation"] = EnvReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
			action =EnvActionSpaceSample[env];
			(*Print[state["observation"]]*);
			AppendTo[ob,state["observation"]];
			AppendTo[ac,action];
			state = EnvStep[env, action, True];
			AppendTo[re,state["reward"]];
			AppendTo[next,state["observation"]];
			If[state["done"],Break[]]
			,
			{step, st}];
		AppendTo[states["observation"],ob];
		AppendTo[states["action"],ac];
		AppendTo[states["reward"],re];
		AppendTo[states["next"],next];
		,
		{episode,ep}
	];
	(* close the environment when done *)
	EnvClose[env];
	states
]
(*---------------------*)
game[ep_Integer,st_Integer,net_]:= Module[{
		env,states, list,next,
		state,ob,ac,re,action
	},
	env = EnvCreate["Pong-ram-v0"];
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["observation"] = EnvReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
		    If[RandomInteger[{0,100}]<=5,
		    action = EnvActionSpaceSample[env],
		    (*Print[state["observation"]];*)
		    list =net[state["observation"]];
			action = Position[list,Max[list]]-1;
			action = action[[1,1]]];
			(*Print[action];*)
			AppendTo[ob,state["observation"]];
			AppendTo[ac,action];
			state = EnvStep[env, action, True];
			AppendTo[re,state["reward"]];
			AppendTo[next,state["observation"]];
			If[state["done"],Break[]]
			,
			{step, st}];
		AppendTo[states["observation"],ob];
		AppendTo[states["action"],ac];
		AppendTo[states["reward"],re];
		AppendTo[states["next"],next];
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
	list = x;
	list = list*10;
	Print[list];
	list = list+ 1;
	Print[list];
	Reverse[ReplaceList[list,{h__,___}:>(i=i+1;Plus[0.99*i*h)]]
)]
(*---------------------*)
preprocess[x_] := Module[{result},(
	result = <||>;
	result["action"] = Flatten[x["action"]];
	result["observation"] = N[Normalize/@Flatten[x["observation"],1]];
	result["next"] = N[Normalize/@Flatten[x["next"],1]];
	result["reward"] = N[Normalize[Flatten[calculateReward/@x["reward"]]]];
	result
)]
(*---------------------*)
random = 0
generator := Function[(
	If[Mod[#AbsoluteBatch,800]==0 ,(processed = preprocess[game[2,10000,#Net]])];
	pos = RandomInteger[{1,Length[processed["action"]]},#BatchSize];
	result = <||>;
	result["Input"] = processed["observation"][[pos]];
	out = #Net[processed["observation"][[pos]]];
	temp = processed["reward"][[pos]]+0.99*Max/@#Net[processed["next"][[pos]]];
	MapIndexed[
		(out[[First@#2,(#1+1)]]=temp[[First@#2]])&,processed["action"][[pos]]
	];x
	result["Output"] = out;
	result
)];
End[ ]

EndPackage[ ]






























