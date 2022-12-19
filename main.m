(*sortF[a_,b_]:=Module[
	{uA,uB},
	uA=If[Im[a]!=0,a,Abs[a]];
	uB=If[Im[b]!=0,b,Abs[b]];
	uA<uB
]*)

solveLinear[a_,b_]:=-b/a;
solveQuadratic[a_,b_,c_]:=Module[
	{k,r},
	k=-b/(2*a);
	r=Sqrt[k^2-c/a];
	k+{-r,r}
];
solveCubic[a_,b_,c_,d_]:=Module[
	{k,p,q,r,m0,m1},
	k=-b/(3*a);
	p=(c+2*b*k+3*a*k^2)/(3*a);
	q=-(d+k*(c+k*(b+a*k)))/(2*a);
	r=Sqrt[p^3+q^2];
	
	m0=If[Im[r]!=0,(q+r)^(1/3),If[q+r<0,-CubeRoot[-q-r],CubeRoot[q+r]]];
	m1=If[Im[r]!=0,(q-r)^(1/3),If[q-r<0,-CubeRoot[r-q],CubeRoot[q-r]]];
	
	Sort[k+{
		m0+m1,
		Exp[2*Pi*I/3]*m0+Exp[4*Pi*I/3]*m1,
		Exp[4*Pi*I/3]*m0+Exp[2*Pi*I/3]*m1
	}]
];
solveQuartic[a_,b_,c_,d_,e_]:=Module[
	{k,p,q,r,z0,z1,y,m0,m1,n0,n1},
	k=-b/(4*a);
	p=(c+3*k*(b+2*a*k))/a;
	q=(d+k*(2*c+k*(3*b+4*a*k)))/a;
	r=(e+k*(d+k*(c+k*(b+a*k))))/a;
	
	If[
		q==0,
		
		{z0,z1}=Sqrt[solveQuadratic[1,p,r]];
		k+{-z1,-z0,z0,z1},
		
		y=solveCubic[2,-p,-2*r,p*r-q^2/4][[1]];
		m0=Sqrt[2*y-p];
		m1=(2*q)/m0;
		n0=Sqrt[-2*y-p+m1];
		n1=Sqrt[-2*y-p-m1];
		Sort[k+{
			(-m0+n0)/2,
			(-m0-n0)/2,
			(m0+n1)/2,
			(m0-n1)/2
		}]
	]
];




(*In[99]:= solveQuartic[1,3,4,0,0]

Out[99]= {0, 0, -(3/4) + 1/2 (-(3/2) - I Sqrt[7]), -(3/4) + 
  1/2 (-(3/2) + I Sqrt[7])}*)
