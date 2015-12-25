(* ::Package:: *)

(*SQQW Ver 1.0.3*)
(*2015 Dec 18*)

(*
				Second Quantization Quantum Walk
*)
(*
	This Mathematica package contains a set of useful tools 
	and functionalities which can be used to perform numeric 
	calculations of Quantum Walk in the form of Second Quanti-
	zation. This package works for a wide range of Hamitonians 
	and Initial Conditions.
	This package utilizes the official Mathematica package
	'Notation' to make the input and display of operators the
	same as them in writing. So please check if 'Notation' package
	is available in your environment.
	This package is still under construction. Please contact
	me if you have any problem or find bugs.
*)
(*
	Email: yuluyan1995@gmail.com
*)

(*Copyright 2015 *)
(*Luyan Yu*)


BeginPackage["SQQW`",{"Notation`"}];


$FineDisplayCondition=False;
EnableFineDisplay::usage="Create some shortcuts for Creation and Annihilation operators.";

WickExpand::usage="Wick expand of operators.";
NormalOrder::usage="Wrapper for operators in noraml order.";
InnerContraction::usage="Wrapper for labeled contraction.";

SymmetricExpand::usage="Symmetically extract contractions from Normal Order.";
AntisymmetricExpand::usage="Antisymmetically extract contractions from Normal Order.";
Contraction::usage="Wrapper for extracted contractions.";

$ContractionReductionCondition=False;
EnableContractionReduction::usage="Reduce the contractions of Creation and Annihilation operators.";

VacuumMean::usage="Calculate the vacuum mean of operators.";
SQQWInitialize::usage="Enable Fine Dispaly and Contraction Reduction.";

InfiniteSubstitute::usage="Substitute one delta-function variable in a product.";
InfiniteSubstituteList::usage="Substitute a list of delta-function variables in a product.";
InfiniteSumDelta::usage="Substitute a list of delta-function variables in a sum of products.";
HInfiniteSum::usage="Wrapper for infinite sum in Hamiltonian.";
HTerm::usage="Wrapper for term in Hamiltonian.";
SQHamiltonian::usage="Wrapper for Hamiltonian.";
SQHCalculate::usage="Calculate algebraic expression of Hamiltonian.";

SQInitialCalculate::usage="Calculate vector of Initial.";
SQInitial::usage="Wrapper for Initial.";
InitialTerm::usage="Wrapper for term in Initial.";
InitialInfiniteSum::usage="Wrapper for infinite sum in Initial.";

SQHamiltonialEvolve::usage="Evolve the Hamiltonian accordingly.";

ToSymbolicDelta::usage="Convert delta function to C code.";
SQHamiltonialEvolveCUDAKernel::usage="Generate Hamiltonian Matrix using CUDA.";

LinearMappingEncodeFunction::usage="Return an encoding function for multiple indexes.";
LinearMappingDecodeFunction::usage="Return a decoding function for multiple indexes.";

progressBar::usage="Labeled dynamic progress bar.";
MonitorMap::usage="Map with dynamic monitoring progress bar.";
MonitorParallelMap::usage="ParallelMap with dynamic monitoring progress bar.";


Begin["`Private`"];


EnableFineDisplay::off="Fine Display is off.";
EnableFineDisplay::on="Fine Display is on.";
SyntaxInformation[EnableFineDisplay]={"ArgumentsPattern"->{_}};

EnableFineDisplay[switch_?BooleanQ]:=
Module[{},
	Which[
		!switch && $FineDisplayCondition,
		$FineDisplayCondition=False;
		ClearNotations[];,
		(*
		RemoveNotation[ParsedBoxWrapper[SubscriptBox["a", "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"SQQW`Private`OP", "[", RowBox[{"SQQW`Private`Annihilation", "[", "state_", "]"}], "]"}]]];
		RemoveNotation[ParsedBoxWrapper[SubscriptBox[SuperscriptBox["a", "\[Dagger]"], "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"SQQW`Private`OP", "[", RowBox[{"SQQW`Private`Creation", "[", "state_", "]"}], "]"}]]];
		RemoveNotation[ParsedBoxWrapper[SubscriptBox["n", "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{SubscriptBox[SuperscriptBox["a", "\[Dagger]"], "state_"], SubscriptBox["a", "state_"]}]]];
		RemoveNotation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "symmetric"}], "|", "operators_", "|", RowBox[{"symmetric", SubscriptBox[">", "d_"]}]}]]\[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"VacuumMean", "[", RowBox[{RowBox[{"List", "@@", "operators_"}], ",", "SymmetricExpand",",","d_"}], "]"}]]];
		RemoveNotation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "antisymmetric"}], "|", "operators_", "|", RowBox[{"antisymmetric", SubscriptBox[">", "d_"]}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"VacuumMean", "[", RowBox[{RowBox[{"List", "@@", "operators_"}], ",", "AntisymmetricExpand",",","d_"}], "]"}]]];,
		*)
		switch && !$FineDisplayCondition,
		$FineDisplayCondition=True;
		Notation[ParsedBoxWrapper[SubscriptBox["a", "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"SQQW`Private`OP", "[", RowBox[{"SQQW`Private`Annihilation", "[", "state_", "]"}], "]"}]]];
		Notation[ParsedBoxWrapper[SubscriptBox[SuperscriptBox["a", "\[Dagger]"], "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"SQQW`Private`OP", "[", RowBox[{"SQQW`Private`Creation", "[", "state_", "]"}], "]"}]]];
		Notation[ParsedBoxWrapper[SubscriptBox["n", "state_"]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{SubscriptBox[SuperscriptBox["a", "\[Dagger]"], "state_"],"**", SubscriptBox["a", "state_"]}]]];
		Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "symmetric"}], "|", "operators_", "|", RowBox[{"symmetric", SubscriptBox[">", "d_"]}]}]]\[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"VacuumMean", "[", RowBox[{RowBox[{"List", "@@", "operators_"}], ",", "SymmetricExpand",",","d_"}], "]"}]]];
		Notation[ParsedBoxWrapper[RowBox[{RowBox[{"<", "antisymmetric"}], "|", "operators_", "|", RowBox[{"antisymmetric", SubscriptBox[">", "d_"]}]}]] \[DoubleLongRightArrow] ParsedBoxWrapper[RowBox[{"VacuumMean", "[", RowBox[{RowBox[{"List", "@@", "operators_"}], ",", "AntisymmetricExpand",",","d_"}], "]"}]]];,
		!switch && !$FineDisplayCondition,
		Message[EnableFineDisplay::off];$Failed,
		switch && $FineDisplayCondition,
		Message[EnableFineDisplay::on];$Failed
		]
];


EnableContractionReduction::off="Contraction Reduction is off.";
EnableContractionReduction::on="Contraction Reduction is on.";
SyntaxInformation[EnableContractionReduction]={"ArgumentsPattern"->{_}};

EnableContractionReduction[switch_?BooleanQ]:=
Module[{},
	Which[
		!switch && $ContractionReductionCondition,
		$ContractionReductionCondition=False;
		Clear[Contraction];,
		switch && !$ContractionReductionCondition,
		$ContractionReductionCondition=True;
		Contraction[OP[Creation[state1_]],OP[Creation[state2_]]]=0;
		Contraction[OP[Annihilation[state1_]],OP[Annihilation[state2_]]]=0;
		Contraction[OP[Creation[state1_]],OP[Annihilation[state2_]]]=0;,
		(*Contraction[OP[Annihilation[state1_]],OP[Creation[state2_]]]=d[state1,state2];*)
		!switch && !$ContractionReductionCondition,
		Message[EnableContractionReduction::off];$Failed,
		switch && $ContractionReductionCondition,
		Message[EnableContractionReduction::on];$Failed
		]
];


mapContraction[operators__OP,posS_]:=
Module[{$CTag=0},
	Fold[
		($CTag+=1;MapAt[InnerContraction[#,$CTag]&,#1,#2])&,
		NormalOrder[operators],
		posS
	]
];
WickExpand[operators__OP]:=
NormalOrder[operators]+Sum[
	Plus@@(mapContraction[operators,#]&/@Select[Subsets[Transpose[{Subsets[Range[Length[{operators}]],{2}]},{3,1,2}],{$loop}],DuplicateFreeQ[Flatten[#]]&]),
	{$loop,Quotient[Length[{operators}],2]}
];


SymmetricExpand[NormalOrder[operators__]]:=
First[
{NormalOrder[operators]}//.{
							NormalOrder[]->1,
							NormalOrder[a_SQQW`Private`OP]:>a,
							NormalOrder[A___,InnerContraction[a_SQQW`Private`OP,Tag_],B___,InnerContraction[b_SQQW`Private`OP,Tag_],CC___]:>Contraction[a,b]*NormalOrder[A,B,CC]
							}
];
SymmetricExpand[HoldPattern[Plus[norm__NormalOrder]]]:=Plus@@(SymmetricExpand/@{norm});

AntisymmetricExpand[NormalOrder[operators__]]:=
First[
{NormalOrder[operators]}//.{
							NormalOrder[]->1,
							NormalOrder[a_SQQW`Private`OP]->a,
							NormalOrder[A___,InnerContraction[a_SQQW`Private`OP,Tag_],B___,InnerContraction[b_SQQW`Private`OP,Tag_],CC___]:>(-1)^(Length[{B}])*Contraction[a,b]*NormalOrder[A,B,CC]
							}
];
AntisymmetricExpand[HoldPattern[Plus[norm__NormalOrder]]]:=Plus@@(AntisymmetricExpand/@{norm});


VacuumMean[{operators__},expandFunction_,deltaFunction_]:=
Module[{d},
d[state1_List,state2_List]/;Length[state1]==Length[state2]:=Inner[d,state1,state2,Times];
d[state_,state_]=1;
d[state1_,state2_]/;Unequal[state1,state2]=0;
(expandFunction[
	WickExpand[operators]
]/.{
	NormalOrder[___]->0,
	Contraction[SQQW`Private`OP[SQQW`Private`Annihilation[state1_]],SQQW`Private`OP[SQQW`Private`Creation[state2_]]]:>d[state1,state2]
	})/.{d->deltaFunction}

];


SQQWInitialize[]:=
Module[{},
	EnableFineDisplay[True];
	EnableContractionReduction[True];
];


InfiniteSubstitute::nosol="No solution for some delta-functions.";
SyntaxInformation[InfiniteSubstitute]={"ArgumentsPattern"->{_,_,_},"LocalVariables"->{"Solve",{2,3}}};

InfiniteSubstitute[expr_,deltaFunction_,var_]:=
If[Head[expr]==deltaFunction,
	If[!MemberQ[Level[expr,1],var],expr,1]
];

InfiniteSubstitute[expr_Times,deltaFunction_,var_]:=
Module[{$termList=List@@expr,$containingList,$identicalList,$targetTerm,$substitue,$sol},
	$containingList=Select[$termList,MemberQ[Level[#,Infinity],var]&];
	If[$containingList==={},
		expr,
		$identicalList=Select[$termList,MemberQ[Level[#,1],var]&];
		If[$identicalList==={},
			$targetTerm=First[$containingList];
			$sol=Solve[Equal@@Level[$targetTerm,1],var];
			If[$sol=={{}},
				Message[InfiniteSubstitute::nosol],
				$substitue=$sol[[1,1,2]];
			],
			$targetTerm=First[$identicalList];
			$substitue=First@Complement[Level[$targetTerm,1],{var}];
			];
	(expr/.{var->$substitue})/.{deltaFunction[a_,a_]->1}
	]
];

SyntaxInformation[InfiniteSubstituteList]={"ArgumentsPattern"->{_,_,_},"LocalVariables"->{"Solve",{2,3}}};
InfiniteSubstituteList[expr_Times,deltaFunction_,vars_List]:=
	Fold[InfiniteSubstitute[#1,deltaFunction,#2]&,expr,vars];


InfiniteSumDelta::exprtype="Unknown type of expression: `1` in (`2`).";
SyntaxInformation[InfiniteSumDelta]={"ArgumentsPattern"->{_,_,_},"LocalVariables"->{"Solve",{2,3}}};

InfiniteSumDelta[expr_,deltaFunction_,vars_List]:=
Module[{$productList},
	Switch[Head[expr],
		Times,
		InfiniteSubstituteList[expr,deltaFunction,vars],
		Plus,
		$productList=List@@expr;
		Plus@@(InfiniteSubstituteList[#,deltaFunction,vars]&/@$productList),
		Integer,
		If[expr==0,
			0,
			Message[InfiniteSumDelta::nonzero,expr];$Failed
		],
		_,
		Message[InfiniteSumDelta::exprtype,Head[expr],expr];$Failed
	]
];


SyntaxInformation[HInfiniteSum]={"ArgumentsPattern"->{_,_},"LocalVariables"->{"Solve",{2}}};
HInfiniteSum[HoldPattern@Plus[pro__],vars_List]:=
	If[Length[{pro}]>1,Plus@@(HInfiniteSum[#,vars]&/@{pro}),HInfiniteSum[pro,vars]];

SyntaxInformation[HTerm]={"ArgumentsPattern"->{_}};
HTerm[HoldPattern@Plus[pro__]]:=
	If[Length[{pro}]>1,Plus@@(HTerm[#]&/@{pro}),HTerm[pro]];

SQHamiltonian[]=0;


SQHCalculate::exprtype="Unknown type of expression: `1`.";
SyntaxInformation[SQHCalculate]={"ArgumentsPattern"->{_,{_,_},_,_}};

SQHCalculate[SQHamiltonian[expr_],{baseL_,baseR_},"Boson",deltaFunction_]:=
Module[{d},
	d[state1_List,state2_List]/;Length[state1]==Length[state2]:=Inner[d,state1,state2,Times];
	d[state_,state_]=1;
	d[state1_,state2_]/;Unequal[state1,state2]=0;
	Switch[Head[expr],
		Symbol,expr,
		Real,expr,
		Integer,expr,
		Rational,expr,
		Complex,expr,
		HTerm,
		VacuumMean[List@@(Evaluate[baseL**expr[[1]]**baseR]),SymmetricExpand,d],
		HInfiniteSum,
		InfiniteSumDelta[VacuumMean[List@@(Evaluate[baseL**expr[[1]]**baseR]),SymmetricExpand,d],d,expr[[2]]],
		Times,
		Times@@(SQHCalculate[SQHamiltonian[#],{baseL,baseR},"Boson",deltaFunction]&/@(List@@expr)),
		Plus,
		Plus@@(SQHCalculate[SQHamiltonian[#],{baseL,baseR},"Boson",deltaFunction]&/@(List@@expr)),
		_,
		Message[SQHCalculate::exprtype,Head[expr]];$Failed
	]/.{d->deltaFunction}
];

SQHCalculate[SQHamiltonian[expr_],{baseL_,baseR_},"Fermion",deltaFunction_]:=
Module[{d},
	d[state1_List,state2_List]/;Length[state1]==Length[state2]:=Inner[d,state1,state2,Times];
	d[state_,state_]=1;
	d[state1_,state2_]/;Unequal[state1,state2]=0;
	Switch[Head[expr],
		Symbol,expr,
		Real,expr,
		Integer,expr,
		Rational,expr,
		Complex,expr,
		HTerm,
		VacuumMean[List@@(Evaluate[baseL**expr[[1]]**baseR]),AntisymmetricExpand,d],
		HInfiniteSum,
		InfiniteSumDelta[VacuumMean[List@@(Evaluate[baseL**expr[[1]]**baseR]),AntisymmetricExpand,d],d,expr[[2]]],
		Times,
		Times@@(SQHCalculate[SQHamiltonian[#],{baseL,baseR},"Fermion",deltaFunction]&/@(List@@expr)),
		Plus,
		Plus@@(SQHCalculate[SQHamiltonian[#],{baseL,baseR},"Fermion",deltaFunction]&/@(List@@expr)),
		_,
		Message[SQHCalculate::exprtype,Head[expr]];$Failed
	]/.{d->deltaFunction}
];


(*
LinearMappingEncodeFunction[varList_List,ranges__List]:=
Module[{$varNum=Length[varList],$rangeList={ranges},$eachNumber},
	If[Length[$rangeList]!=$varNum,
		Message[LinearMappingEncodeFunction::nomatch];Return[$Failed]
	];
	$eachNumber=IntegerPart[(#[[3]]-#[[2]])/#[[4]]]&/@$rangeList+1;
	Function[Evaluate@varList,
		Evaluate[
			Simplify[1+Plus@@((varList-$rangeList[[All,2]])*FoldList[Times,1,$eachNumber][[;;-2]])]
		]
	]
];

LinearMappingDecodeFunction[varList_List,ranges__List]:=
Module[{$varNum=Length[varList],$rangeList={ranges},$eachNumber},
	If[Length[$rangeList]!=$varNum,
		Message[LinearMappingDecodeFunction::nomatch];Return[$Failed]
	];
	$eachNumber=IntegerPart[(#[[3]]-#[[2]])/#[[4]]]&/@$rangeList+1;
	Function[c,
		Evaluate[
			Simplify@Reverse@(FoldList[{Quotient[#1[[2]],#2],#1[[2]]-Quotient[#1[[2]],#2]*#2}&,{0,c-1},Reverse@FoldList[Times,1,$eachNumber][[;;-2]]][[2;;,1]])+$rangeList[[All,2]]
		]
	]
];
*)


(*These functions are not safe since they do not perform range check*)
Options[LinearMappingEncodeFunction]={ValidationCheck->False};
SetOptions[LinearMappingEncodeFunction,ValidationCheck->False];
LinearMappingEncodeFunction::nomatch="Numbers of variables and ranges do not match.";
LinearMappingEncodeFunction::outbound="Input indexes are out of bounds.";
SyntaxInformation[LinearMappingEncodeFunction]={"LocalVariables"->{"Integrate",{2,Infinity}}};

LinearMappingEncodeFunction[varList_List,ranges__List,OptionsPattern[]]:=
Module[{$varNum=Length[varList],$rangeList={ranges},$table},
	If[Length[$rangeList]!=$varNum,
		Message[LinearMappingEncodeFunction::nomatch];Return[$Failed]
	];
	$table=Flatten[Table[varList,ranges],$varNum-1];
	If[OptionValue[ValidationCheck],
		Function[Evaluate@varList,
			If[True,
				Position[Evaluate@$table,varList][[1,1]],
				Message[LinearMappingEncodeFunction::outbound];$Failed
			]
		],
		Function[Evaluate@varList,
			Position[Evaluate@$table,varList][[1,1]]
		]
	]
];

LinearMappingDecodeFunction::nomatch="Numbers of variables and ranges do not match.";
SyntaxInformation[LinearMappingDecodeFunction]={"LocalVariables"->{"Integrate",{2,Infinity}}};

LinearMappingDecodeFunction[varList_List,ranges__List]:=
Module[{$varNum=Length[varList],$rangeList={ranges},$table},
	If[Length[$rangeList]!=$varNum,
		Message[LinearMappingDecodeFunction::nomatch];Return[$Failed]
	];
	$table=Flatten[Table[varList,ranges],$varNum-1];
	Function[c,
		($table[[c]])
	]
];


SQInitialCalculate::noimpl="This type has not been implemented.";
SQInitialCalculate::exprtype="Unknown type of expression: `1`.";
SyntaxInformation[SQInitialCalculate]={"ArgumentsPattern"->{_,_,_,_}};

SQInitialCalculate[SQInitial[expr_],HEncodeFunction_,HBase_,dim1_]:=
Module[{},
	Switch[Head[expr],
		Symbol,expr,
		Real,expr,
		Integer,expr,
		Rational,expr,
		Complex,expr,
		InitialTerm,
		Normal@SparseArray[
			{{Position[HEncodeFunction@@#&/@HBase,HEncodeFunction@@(expr[[1]])][[1,1]]}->1},
			dim1
			],
		InitialInfiniteSum,
		Message[SQInitialCalculate::noimpl];$Failed,
		Times,
		Times@@(SQInitialCalculate[SQInitial[#],HEncodeFunction,HBase,dim1]&/@(List@@expr)),
		Plus,
		Plus@@(SQInitialCalculate[SQInitial[#],HEncodeFunction,HBase,dim1]&/@(List@@expr)),
		_,
		Message[SQInitialCalculate::exprtype,Head[expr]];$Failed
	]
];

SyntaxInformation[InitialInfiniteSum]={"ArgumentsPattern"->{_,_},"LocalVariables"->{"Solve",{2}}};


Options[SQHamiltonialEvolve]={Method->"CPU"};
SetOptions[SQHamiltonialEvolve,Method->"CPU"];
SQHamiltonialEvolve::nocuda="CUDA is not working.";
SyntaxInformation[SQHamiltonialEvolve]={"ArgumentsPattern"->{_,_,_,_,_,_,OptionsPattern[]},"LocalVariables"->{"Solve",{5,6}}};

SQHamiltonialEvolve[H_SQHamiltonian,particalType_String,initialState_SQInitial,base_,vars_List,{varRanges__List},OptionsPattern[]]:=
Module[{
	$tempVars=ToExpression["$$$$"<>ToString[#]]&/@vars,
	HEncodeFunction,HDecodeFunction,HFunction,HBase,HMatrix,HInitial,Hs,Hj,HEvolution,HWaveFunction,HFunctionSymbolic,delta
	},
	HEncodeFunction=LinearMappingEncodeFunction[Evaluate@vars,varRanges];
	HDecodeFunction=LinearMappingDecodeFunction[Evaluate@vars,varRanges];
	HFunctionSymbolic=SQHCalculate[H,Evaluate@{NonCommutativeMultiply@@Reverse[List@@((Hold[base]/.Thread[Rule[vars,$tempVars]])/.{SQQW`Private`Creation->SQQW`Private`Annihilation})],base},particalType,delta];
	HFunction=Function[
				Evaluate@(List@@Flatten[{Evaluate@vars,Evaluate@$tempVars}]),
				Evaluate[HFunctionSymbolic/.{delta->(If[#1==#2,1,0]&)}]
				];
	HBase=Flatten[Table[vars,varRanges],Length[vars]-1];
	Switch[OptionValue[Method],
	"Parallel",
	If[Length[Kernels[]]==0,LaunchKernels[]];
	HMatrix=MonitorParallelMap[
				"Constructing Hamiltonian Matrix (" <> ToString@Length[Kernels[]] <> "-Kernel Parallel): ",
				HFunction@@Flatten[#]&,
				Evaluate@Outer[List,HBase,HBase,1],
				{2}
				][[2]];
	CloseKernels[];,
	"CUDA",
	CellPrint[
	CellGroup[{
		Row[{
			TextCell["                ","Text"],
			TextCell["Constructing Hamiltonian Matrix (CUDA):","Subsection"],
			ExpressionCell[ProgressIndicator[Appearance->"Percolate"]]
		}]
	}]];
	If[!TrueQ[CUDALink`CUDAQ[]],
		Message[SQHamiltonialEvolve::nocuda];Return[$Failed];,
		HMatrix=SQHamiltonialEvolveCUDAKernel[N@HBase,N@HFunctionSymbolic,vars,$tempVars,delta];
	];,
	_,
	HMatrix=MonitorMap[
				"Constructing Hamiltonian Matrix: ",
				HFunction@@Flatten[#]&,
				Evaluate@Outer[List,HBase,HBase,1],
				{2}
				][[2]];
	];
	
	NotebookFind[SelectedNotebook[],Uncompress["1:eJxTTMoPCpZnYGBwzs8rLikqTS7JzEtX8EjMzcwpyc/LTMxT8E0sKcqsAAAERA3j"],All,CellContents];
	NotebookDelete[];
	CellPrint[
		CellGroup[{
			Row[{
				TextCell["                ","Text"],
				TextCell["Constructing Hamiltonian Finished. Decomposing Matrix...","Subsection"]
			}]
		}]
	];
	
	HInitial=SQInitialCalculate[initialState,HEncodeFunction,HBase,Dimensions[HMatrix][[1]]];
	{Hs,Hj}=SchurDecomposition[N@HMatrix];
	NotebookFind[SelectedNotebook[],Uncompress["1:eJxTTMoPChZiYGBwSU3Ozy3IL87MS1fwTSwpyqwAAGs+CLc="],All,CellContents];
	NotebookDelete[];
	HEvolution[t_]=MapIndexed[If[Equal@@#2,Exp[-I #1 t],#1]&,Hj,{2}];
	HWaveFunction[t_]:=Hs.(HEvolution[t].Evaluate[(Conjugate[Transpose[Hs]].(N@HInitial))]);
	CellPrint[
		CellGroup[{
			Row[{
				TextCell["                ","Text"],
				TextCell["Succeeded!","Subsection"]
			}]
		}]
	];
	{
		{Function[Evaluate@vars,HEncodeFunction@@vars],Function[c,HDecodeFunction@c]},
		HBase,
		Function[t,HWaveFunction[t]]
	}
];


SQHamiltonialEvolveCUDAKernel[HBase_,HFunctionSymbolic_,vars_,tempVars_,delta_]:=
Module[{varNum=Length[vars]*2,CDelta,CHFunction,CUDAHKernel,HBaseCUDA,dumpAns},
	CDelta="__device__ float _d(float _x, float _y) {return (abs(_x-_y)<=(1e-5)?(1.):(0.));}";
	CHFunction=SymbolicC`ToCCodeString@CUDALink`SymbolicCUDAFunction["CUDAHKernel",
	{
		Sequence@@Table[{SymbolicC`CPointerType["float"],"in"<>ToString[i]},{i,1,varNum}],
		{SymbolicC`CPointerType["float"],"out"},
		{"mint","len"}
	},
	SymbolicC`CBlock[{
		CUDALink`SymbolicCUDADeclareIndexBlock[1],
		SymbolicC`CIf[SymbolicC`COperator[Less,{"index","len"}],
			SymbolicC`CBlock[{
				SymbolicC`CAssign[
					SymbolicC`CArray["out","index"],
					Evaluate[ToSymbolicDelta[HFunctionSymbolic/.Thread[Rule[Join[vars,tempVars],Table["in"<>ToString[i]<>"[index]",{i,1,varNum}]]],delta,"res"]]
				]
			}]]
		}]
	];
	CUDAHKernel=CUDALink`CUDAFunctionLoad[
			CDelta<>"\n"<>CHFunction,
			"CUDAHKernel",
			Evaluate@{Sequence@@Table[{"Float",_,"Input"},{i,1,varNum}],{"Float",_,"Output"},_Integer},
			256
		];
	HBaseCUDA=Transpose@Flatten[Map[Flatten,Outer[List,HBase,HBase,1],{2}],1];
	dumpAns=ConstantArray[0,Length[HBaseCUDA[[1]]]];
	Partition[CUDAHKernel[Sequence@@HBaseCUDA,dumpAns,Length[HBaseCUDA[[1]]]][[1]],Length[HBase]]
];


ToSymbolicDelta::exprtype="Unknown type of expression: `1`.";
ToSymbolicDelta::cpexpr="Complex number is not supported for delta function.";
ToSymbolicDelta[expr_,delta_,resVar_]:=
Switch[Head[expr],
	Integer,ToString[N@expr],
	Rational,ToString[N@expr],
	Real,ToString@expr,
	Complex,Message[ToSymbolicDelta::cpexpr];$Failed,
	delta,
	"_d"<>"("<>ToString[expr[[1]]]<>", "<>ToString[expr[[2]]]<>")",
	Times,
	StringRiffle[List@@(ToSymbolicDelta[#,delta,resVar]&/@(expr)),{"(",")*(",")"}],
	Plus,
	StringRiffle[List@@(ToSymbolicDelta[#,delta,resVar]&/@(expr)),{"(",")+(",")"}],
	_,
	Message[ToSymbolicDelta::exprtype,Head[expr]];
];


progressBar[dyn:Dynamic[var_],total_,label_String]:=
CellPrint[
	CellGroup[{
		Row[{
			TextCell["                ","Text"],
			TextCell[label,"Subsection"],
			ExpressionCell[ProgressIndicator[dyn,{0,total}]],
			TextCell[" ","Subsection"],
			TextCell[Dynamic@NumberForm[100. var/total,{\[Infinity],2}],"Subsection"],
			TextCell["%","Subsection"]
		}]
	}]
];

MonitorMap[progressLabel_String,f_,expr_,levelspec_:{1}]:=
DynamicModule[{$monitor=0,$total=Length[Level[expr,levelspec]]},
	progressBar[Dynamic[$monitor],$total,progressLabel];
	Map[($monitor++;f[#])&,expr,levelspec]
];

MonitorParallelMap[progressLabel_String,f_,expr_,levelspec_:{1}]:=
DynamicModule[{$total=Length[Level[expr,levelspec]],$monitor=0,$localmonitor,$monitorstep},
	progressBar[Dynamic[$monitor],$total,progressLabel];
	$monitorstep=IntegerPart[$total/4/100];
	SetSharedVariable[$monitor];
	ParallelEvaluate[$localmonitor=1;];
	ParallelMap[
		(If[$localmonitor>=100,$monitor+=$localmonitor;$localmonitor=1,$localmonitor++];f[#])&,
		expr,
		levelspec
	]
];


End[];


EndPackage[];
