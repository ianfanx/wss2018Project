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
PacletDirectoryAdd[{"/Users/ianfan/Downloads/WolframSummerSchoolRL-master"}]
<<WolframSummerSchoolRL`
Begin["`Private`"]
(*---------------------*)
randomgame[name_,ep_Integer,st_Integer,render_]:= Module[{
		env,states,
		state,ob,ac,re,action, next
	},
	env = RLEnvironmentCreate[name];
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["Observation"] = RLEnvironmentReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
			action =RLEnvironmentSampleAction[env];
			(*Print[state]*);
			AppendTo[ob,state["Observation"]];
			AppendTo[ac,action];
			state = RLEnvironmentStep[env, action, render];
			AppendTo[re,state["Reward"]];
			AppendTo[next,state["Observation"]];
			If[state["Done"],Break[]]
			,
			{step, st}];
		AppendTo[states["observation"],ob];
		AppendTo[states["action"],ac];
		AppendTo[states["reward"],re];
		AppendTo[states["next"],{0,0,0,0}];
		,
		{episode,ep}
	];
	(* close the environment when done *)
	EnvClose[env];
	states
]
(*---------------------*)
game[name_,ep_Integer,st_Integer,net_,render_, rand_]:= Module[{
		env,states, list,next,
		state,ob,ac,re,action
	},
	env = RLEnvironmentCreate[name];
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["Observation"] = RLEnvironmentReset[env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
		    If[RandomInteger[{0,100}]<=Max[rand*100,10],
		    action = RLEnvironmentSampleAction[env],
		    (*Print[state["observation"]];*)
		    list =net[state["Observation"]];
			action = Position[list,Max[list]]-1;
			action = action[[1,1]]];
			(*Print[action];*)
			AppendTo[ob,state["Observation"]];
			AppendTo[ac,action];
			state = RLEnvironmentStep[env, action, render];
			AppendTo[re,state["Reward"]];
			AppendTo[next,state["Observation"]];
			If[state["Done"],
			AppendTo[ob,state["Observation"]];
			AppendTo[ac,action];
			punish = -Max[net[state["Observation"]]]-1;
			AppendTo[re,punish];
			AppendTo[next,state["Observation"]];
			Break[]]
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
	Reverse[ReplaceList[x,{h__,___}:>(i=i+1;Power[0.99,i]*i*h)]];
	x
)]
(*---------------------*)
preprocess[x_] := Module[{result},(
	result = <||>;
	result["action"] = Flatten[x["action"]];
	result["observation"] = Flatten[x["observation"],1];
	result["next"] = Flatten[x["next"],1];
	result["reward"] = Flatten[x["reward"]];
	result
)]
(*---------------------*)
random = 0
generator := Function[(
	If[#AbsoluteBatch == 0,
	processed = <|"action"->{},"observation"->{},"next"->{},"reward"->{}|>];
	
	experience = preprocess[game["WLCartPole",1,10000,#Net, False, Power[0.98,#AbsoluteBatch]]];
	
	AppendTo[processed["action"],#]&/@experience["action"];
	AppendTo[processed["observation"],#]&/@experience["observation"];
	AppendTo[processed["next"],#]&/@experience["next"];
	AppendTo[processed["reward"],#]&/@experience["reward"];
	
	len = Length[processed["action"]]-1000;
	If[len > 0, 
	processed["action"] = processed["action"][[len;;]];
	processed["observation"] = processed["observation"][[len;;]];
	processed["next"] = processed["next"][[len;;]];
	processed["reward"] = processed["reward"][[len;;]];];
	
	pos = RandomInteger[{1,Length[processed["action"]]},#BatchSize];
	(*pos = Range[Length[processed["action"]+1]];*)
	
	result = <||>;
	result["Input"] = processed["observation"][[pos]];
	out = #Net[processed["observation"][[pos]]];
	temp = processed["reward"][[pos]]+0.95*Max/@#Net[processed["next"][[pos]]];
	MapIndexed[
		(out[[First@#2,(#1+1)]]=temp[[First@#2]])&,processed["action"][[pos]]
	];
	result["Output"] = out;
	
	result
)];
End[ ]

EndPackage[ ]

































