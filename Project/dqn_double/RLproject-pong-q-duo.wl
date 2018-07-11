(* ::Package:: *)

BeginPackage["RLprojectAtari`", {"WolframSummerSchoolRL`"}]


randomgame::usage =
        "play Ep number of Cart pole game with random agent, return states";

game::usage =
        "play Ep number of Cart pole game with given network, return states"

calculateReward::usage = 
		"calculate the discounted reward of list of reward" 

preprocess::usage = 
		"name of the game, number of the games startwith, max Epsoide per game, if rendering, random Discount factor, 
		replay buffer size, gamma of the Q function"

creatGenerator::usage = 
		"generate processed data"
quitEnv::usage = 
		"quit environment"
getBest::usage =
		"good net"
getList::usage = 
		"get reward list"
$rewardList;

Begin["`Private`"]
(*---------------------*)
randomgame[env_,ep_Integer,st_Integer,render_,$env_]:= Module[{
		states,observation,
		state,ob,ac,re,action, next
	},
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["Observation"] = RLEnvironmentReset[$env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
			
			action =RLEnvironmentSampleAction[$env];
			(*Print[state]*);
			observation = {};
			AppendTo[observation,state["Observation"]];
			If[
			ob=={},
			AppendTo[observation,state["Observation"]],
			AppendTo[observation,ob[[-1]]];];
			AppendTo[ob,observation];
			AppendTo[ac,action];
			state = RLEnvironmentStep[$env, action, render];
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
	(* close the $environment when done *)
	states
]
(*---------------------*)
game[ep_Integer,st_Integer,net_NetChain,render_, rand_, $env_]:= Module[{
		states, list,next,observation, punish,choiceSpace,
		state,ob,ac,re,action
	},
	choiceSpace = NetExtract[net,"Output"][["Labels"]];
	states = <|"observation"->{},"action"->{},"reward"->{},"next"->{}|>;
	Do[
		state["Observation"] = RLEnvironmentReset[$env]; (* reset every episode *)
		ob = {};
		ac = {};
		re = {};
		next = {};
		Do[
			observation = {};
			observation = Join[observation,state["Observation"]];
			
			If[ob=={},
				observation = Join[observation,state["Observation"]]
				,
				observation = Join[observation, Last[ob][[;;Length[state["Observation"]]]]]
			];
			
		    action = If[RandomReal[]<=Max[rand,0.1],
		        RandomChoice[choiceSpace]
		        ,
		        net[observation]
		     ];
		     
			(*Print[action];*)
			AppendTo[ob, observation];
			AppendTo[ac, action];
			
			state = RLEnvironmentStep[$env, action, render];
			
			If[Or[state["Done"], state["Reward"]<0],
				punish = - Max[Values[net[observation,"Probabilities"]]] - 1;
				AppendTo[re, punish];
				AppendTo[next, observation];
				,
				AppendTo[re, state["Reward"]];
				observation = state["Observation"];
				observation = Join[observation, ob[[-1]][[;;Length[state["Observation"]]]]];
				AppendTo[next, observation];
			]\:ff1b
			
			If[Or[state["Done"],state["Reward"]<0],
			Break[]]
			,
			{step, st}];
		AppendTo[states["observation"], ob];
		AppendTo[states["action"], ac];
		AppendTo[states["reward"], re];
		AppendTo[states["next"], next];
		,
		{episode,ep}
	];
	(* close the $environment when done *)
	states
]
(*---------------------*)
calculateReward[x_] := Module[{i,h},(
	i=0;(*
	Reverse[ReplaceList[x,{h__,___}:>(i=i+1;Power[0.99,i]*i*h)]];*)
	x
)];
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
creatGenerator[env_, start_, maxEp_, render_, randomDiscount_, replaySize_, gamma_] := Module[
	{processed, best, experience, reward, len, pos, result, out, temp, temp1, temp2
	},generator := Function[(
		If[#AbsoluteBatch == 0, 
			processed = <|"action"->{},"observation"->{},"next"->{},"reward"->{}|>;
			$rewardList = {};
			$env=env;
			best = 0;
		];
		If[#AbsoluteBatch == 0, 
			experience = preprocess[game[start,maxEp,#Net, render, Power[randomDiscount,#AbsoluteBatch], $env]]
			,
			experience = preprocess[game[1,maxEp,#Net, render, Power[randomDiscount,#AbsoluteBatch],$env]]
		];
		
		NotebookDelete[temp];
		reward = Length[experience["action"]];
		AppendTo[$rewardList,reward];
		temp=PrintTemporary[reward];
	
		If[reward>best,best = reward;bestNet = #Net];
	
		If[reward>0,
			AppendTo[processed["action"],#]&/@experience["action"];
			AppendTo[processed["observation"],#]&/@experience["observation"];
			AppendTo[processed["next"],#]&/@experience["next"];
			AppendTo[processed["reward"],#]&/@experience["reward"];
		];

		len = Length[processed["action"]] - replaySize;
		If[len > 0, 
			processed["action"] = processed["action"][[len;;]];
			processed["observation"] = processed["observation"][[len;;]];
			processed["next"] = processed["next"][[len;;]];
			processed["reward"] = processed["reward"][[len;;]];
		];
	
		pos = RandomInteger[{1,Length[processed["action"]]},#BatchSize];
		(*pos = Range[Length[processed["action"]+1]];*)
	
		result = <||>;
		result["Input"] = processed["observation"][[pos]];
		out = Values[#Net[processed["observation"][[pos]],"Probabilities"]];
		
		temp1 = processed["reward"][[pos]];
		temp2 = gamma*Max[Values[#]]&/@#Net[processed["next"][[pos]],"Probabilities"];
		temp = temp1 + temp2;

		(*hardcoded for pong*)
		MapIndexed[
			(out[[First@#2,(#1+1)]]=temp[[First@#2]])&,(processed["action"][[pos]])
		];
		result["Output"] = out;
		result
	)];
	generator
]



quitEnv := Function[
	RLEnvironmentClose[$env];
];

getBest := Function[
	bestNet
]

getList := Function[
	$rewardList
]
End[ ];

EndPackage[ ]













































