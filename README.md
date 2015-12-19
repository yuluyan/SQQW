# SQQW - Second Quantization Quantum Walk
Still under construction.

##Development environment
Windows 10 Home

Wolfram Mathematica 10.2

##Functionality
* Wick expand of operators
  * Symetric expand
  * Antisymetric expand
  * User-defined (Not implemented)
* Vacuum average of creation and annihilation operators
* Simulate arbitrary Hamiltonian and initial state
  * Hamiltonian
    * Single term
    * Basic arithmetics
    * Infinite sum
    * Periodic boundary condition (Not implemented)
  * Initial state
    * Single term
    * Basic arithmetics
    * Infinite sum  (Not implemented)
  * Partical type
    * Boson
    * Fermion
    * Hard-core Boson (Not implemented)

##Example - Nearest interaction
###Initialization
Assume the package file (SQQW.wl) is in the same category of your notebook.
```Mathematica
Clear["Global`*"];
Get[NotebookDirectory[] <> "SQQW.wl"]
SQQWInitialize[];
```

###Define Quantum Walk
####Hamiltonian
```Mathematica
H1 = SQHamiltonian[
  -HInfiniteSum[Subscript[SuperDagger[a], l + 1] ** Subscript[a, l] + Subscript[SuperDagger[a], l] ** Subscript[a, l + 1], {l}]
  +5 HInfiniteSum[Subscript[n, l + 1] ** Subscript[n, l], {l}]
   ];
```
This may seems odd here but in Mathematica it is displayed in the way of writing phyiscs equations.
![](http://luyan.in/snippet.png)

Note that the double stars mean non-commutable multiplication.

####Initial state
```Mathematica
Initial = SQInitial[0.5 InitialTerm[{0, 1}] + 0.5 InitialTerm[{2, 3}]];
```
This means the initial state is a superposition.

####Partical type
```Mathematica
particalType = "Boson";
```
Currently, Boson and Fermion are supported.

####Matrix base
```Mathematica
base = Subscript[SuperDagger[a], l1] ** Subscript[SuperDagger[a], l2];
```

###Simulation
```Mathematica
{{fe, fd}, H1Base, H1WaveFunction} = 
  SQHamiltonialEvolve[
    H1, 
    particalType, 
    Initial, 
    base, 
    {l1, l2}, {{l1, -10, 10}, {l2, l1, 10}}
  ];
```
A progress bar will show when constructing Hamiltonian matrix.

After calculation, ```fe``` and ```fd``` will be assigned to a encoding and decoding function which can be ignored.
```H1Base``` is assigned to a vector of bases.
```H1WaveFunction``` is assigned to the coresponding wave function under that base. Note that ```H1WaveFunction``` is a function of time.

###Visualization
####Correlation
```Mathematica
H1CorrelationHalf[t_] := 
  Abs[Normal[
    SparseArray[
      Thread[Rule[
        Map[# + {10 + 1, 10 + 1} &, H1Base], 
        H1WaveFunction[t]
        ]],
    {2 10 + 1, 2 10 + 1}
  ]]]^2;
H1Correlation[t_] := (# + Transpose[#]) &[H1CorrelationHalf[t]];
```

####Plot Correlation
```Mathematica
Manipulate[
 MatrixPlot[
  Chop@H1Corelation[t],
  DataReversed -> {True, False},
  ColorFunction -> (Hue[(1 - #)^0.23*0.83] &),
  FrameTicks -> {Table[{i + 10 + 1, i}, {i, -10, 10, 5}], Table[{i + 10 + 1, i}, {i, -10, 10, 5}]}
  ],
{t, 0, 4}]
```
