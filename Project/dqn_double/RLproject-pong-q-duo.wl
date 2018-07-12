(* ::Package:: *)

BeginPackage["RLprojectAtari`", {"WolframSummerSchoolRL`"}]


randomgame::usage =
        "play Ep number of game with random agent, return states";

game::usage =
        "play Ep number of game with given network, return states
		takes numer of epsoide, number of step, neural network, if rendering, Decaying random factor, environment, function of dertermin if ending"

calculateReward::usage = 
		"calculate the discounted reward of list of reward" 

preprocess::usage = 
		"flattern the result and option on if normalizing the observation"

creatGenerator::usage = 
		"takes name of the game, number of the games startwith, max Epsoide per game, if rendering, random Discount factor, 
		replay buffer size, gamma of the Q function"
		
quitEnv::usage = 
		"quit environment"
		
getBest::usage =
		"net with highest peak performance"
		
getList::usage = 
		"get reward list of previous trainning"
		
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
game[ep_Integer,st_Integer,net_NetChain,render_, rand_, $env_, end_:Function[False]]:= Module[{
		states, list,next,observation, punish,choiceSpace,
		state,ob,ac,re,action
	},
	(*get the action space from net decoder*)
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
			
			(*decide if choosing random action*)
		    action = If[RandomReal[]<=Max[rand,0.1],
		        RandomChoice[choiceSpace]
		        ,
		        net[observation]
		     ];
		     
			AppendTo[ob, observation];
			AppendTo[ac, action];
			
			state = RLEnvironmentStep[$env, action, render];
			
			(*if the game ended, add negative reward*)
			If[Or[state["Done"], end[state]],
				punish = - Max[Values[net[observation,"Probabilities"]]] - 1;
				AppendTo[re, punish];
				AppendTo[next, observation];
				Break[]
				,
				AppendTo[re, state["Reward"]];
				observation = state["Observation"];
				observation = Join[observation, ob[[-1]][[;;Length[state["Observation"]]]]];
				AppendTo[next, observation];
			]\:ff1b

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
preprocess[x_, nor_:False] := Module[{result},(
	result = <||>;
	result["action"] = Flatten[x["action"]];
	If[nor,
		result["observation"] = N[Normalize/@Flatten[x["observation"],1]];
		result["next"] = N[Normalize/@Flatten[x["next"],1]];
		,
		result["observation"] = Flatten[x["observation"],1];
		result["next"] = Flatten[x["next"],1];
	];
	result["reward"] = Flatten[x["reward"]];
	result
)]
(*---------------------*)
creatGenerator[env_, start_:20, maxEp_:10000, render_:False, randomDiscount_:0.98, replaySize_:1000, gamma_:0.95, nor_:False] := Module[
	{processed, best, experience, reward, len, pos, result, out, temp, temp1, temp2
	},generator := Function[(
		If[#AbsoluteBatch == 0, 
			processed = <|"action"->{},"observation"->{},"next"->{},"reward"->{}|>;
			$rewardList = {};
			$env=env;
			best = 0;
		];
		
		(*generate data, if replay buffer is empty, fill the buffer*)
		If[#AbsoluteBatch == 0, 
			experience = preprocess[game[start,maxEp,#Net, render, Power[randomDiscount,#AbsoluteBatch], $env], nor]
			,
			experience = preprocess[game[1,maxEp,#Net, render, Power[randomDiscount,#AbsoluteBatch],$env], nor]
		];
		
		(*print reward and record it*)
		NotebookDelete[temp];
		reward = Length[experience["action"]];
		AppendTo[$rewardList,reward];
		temp=PrintTemporary[reward];
		
		(*records net with best peak performance*)
		If[reward>best,best = reward;bestNet = #Net];
		
		(*add to the replay buffer, possible improvement: experience with performance lower than threshold will not be added to the buffer*)
		If[reward>0,
			AppendTo[processed["action"],#]&/@experience["action"];
			AppendTo[processed["observation"],#]&/@experience["observation"];
			AppendTo[processed["next"],#]&/@experience["next"];
			AppendTo[processed["reward"],#]&/@experience["reward"];
		];
		
		(*limits the size of the buffer*)
		len = Length[processed["action"]] - replaySize;
		If[len > 0, 
			processed["action"] = processed["action"][[len;;]];
			processed["observation"] = processed["observation"][[len;;]];
			processed["next"] = processed["next"][[len;;]];
			processed["reward"] = processed["reward"][[len;;]];
		];
	
		pos = RandomInteger[{1,Length[processed["action"]]},#BatchSize];
		result = <||>;
		result["Input"] = processed["observation"][[pos]];
		
		(*Q(s,a)*)
		out = Values[#Net[processed["observation"][[pos]],"Probabilities"]];
		
		(*R(s,a)*)
		temp1 = processed["reward"][[pos]];
		
		(*Q(s',a')*)
		temp2 = gamma*Max[Values[#]]&/@#Net[processed["next"][[pos]],"Probabilities"];
		
		(*Q(s,a) = R(s,a) + Q(s',a')*)
		temp = temp1 + temp2;
		MapIndexed[
			(out[[First@#2,(#1+1)]]=temp[[First@#2]])&,(processed["action"][[pos]]-First[NetExtract[#net,"Output"][["Labels"]]])
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













































