(* ::Package:: *)

(* 25.2.13 *)


 (*<<NumericalMath`ListIntegrate` *)


Off[SetDelayed::"write"]


 



 
Directory[]


Off[General::spell1]
Remove["Global`*"];
Unprotect[In,Out];
Clear[In,Out];

 
(* 1  2   3  *)
(* nr,w,alpha,rh *)
conf=ReadList["res.txt",{Number,Number ,Number  ,Number   }]

 
nr=conf[[1]][[1]];
w= conf[[1]][[2]];
alfa= conf[[1]][[3]];
rh= conf[[1]][[4]];
  

Print["winding number n = ",nr];
Print["w   = ",w];
Print["alfa = ",alfa]; 
 Print["rh = ",rh]; 


gr=ReadList["gridx.dat",{Number}];


lgr=Length[gr];
nx=lgr;
Print["nx = ",nx];

listar=Table[gr[[k]][[1]],{k,1,lgr}] ;
listalogr=Table[Log[10,gr[[k]][[1]]],{k,1,lgr}];

 unghi0=ReadList["gridy.dat",{Number}];
ny=Length[unghi0];
Print["ny = ",ny];
unghi=Table[unghi0[[k]][[1]],{k,1,ny}]; 

(*unghi=Table[(k-1)*Pi/2/(ny-1),{k,1,ny}];*)

ntot=nx*ny;

a=ReadList["functf.dat",{Number,Number,Number,Number,Number,Number,Number,Number}];


lung1=Length[a];


(*Datele sunt salvate direct cu indexarea globala*)
F1=Table[a[[k]][[1]],{k,1,lung1}];
F2=Table[a[[k]][[2]],{k,1,lung1}];
F0=Table[a[[k]][[3]],{k,1,lung1}];  
W=Table[a[[k]][[4]],{k,1,lung1}]; 
H1=Table[a[[k]][[5]],{k,1,lung1}];
H2=Table[a[[k]][[6]],{k,1,lung1}];
H3=Table[a[[k]][[7]],{k,1,lung1}];
V=Table[a[[k]][[8]],{k,1,lung1}];


(*Se construiesc ny liste pt. marimi de interes la unghiuri fixate *)
(*foarte util in reprezentari grafice *)

Do[

F1u[k]=Table[F1[[i]],{i,(k-1)*nx+1,k*nx}];
F2u[k]=Table[F2[[i]],{i,(k-1)*nx+1,k*nx}];
F0u[k]=Table[F0[[i]],{i,(k-1)*nx+1,k*nx}];  
Wu[k]=Table[W[[i]],{i,(k-1)*nx+1,k*nx}]; 
H1u[k]=Table[H1[[i]],{i,(k-1)*nx+1,k*nx}]; 
H2u[k]=Table[H2[[i]],{i,(k-1)*nx+1,k*nx}]; 
H3u[k]=Table[H3[[i]],{i,(k-1)*nx+1,k*nx}]; 
Vu[k]=Table[V[[i]],{i,(k-1)*nx+1,k*nx}]; 



,{k,1,ny}];
 
 
as1=2;
as2=IntegerPart[ny/2];
as3=ny-1;

sa1=3;
sa2=IntegerPart[nx/2];
sa3=nx-1;


Print["rmax = ",gr[[nx]][[1]]];


minF0=Min[F0] 
maxF0=Max[ F0 ] 
minF1=Min[F1] 
maxF1=Max[ F1 ] 
minF2=Min[F2] 
maxF2=Max[ F2 ] 
minW=Min[W] 
maxW=Max[ W ] 
maxH1=Max[H1 ] 
minH1=Min[H1] 
maxH2=Max[H2 ] 
minH2=Min[H2] 
maxH3=Max[H3 ] 
minH3=Min[H3] 
maxV=Max[V ] 
minV=Min[V] 


f00=F0u[1][[1]]
f10=F1u[1][[1]]
f20=F2u[1][[1]] 

f0Pi2=F0u[ny][[1]]
f1Pi2=F1u[ny][[1]]
f2Pi2=F2u[ny][[1]] 
 



n1=nx-1;


ni=100;
cut= 1;


nf=nx- cut;


ct=1;
ini=5; 
Do[

data=Table[{listar[[i]] ,F0u[k][[i]]   },{i,nx-ini,nx-cut}];
u=Fit[data,{ 1/x ,1/x^2  ,1/x^3   } ,x];
cf01[k ]=Coefficient[u,1/x ];

data=Table[{listar[[i]] ,F1u[k][[i]]   },{i,nx-ini,nx-cut}];
u=Fit[data,{1/x ,1/x^2  ,1/x^3     } ,x]; 
cf11[k ]=Coefficient[u,1/x ];


data=Table[{listar[[i]] ,F2u[k][[i]]   },{i,nx-ini,nx-cut}];
u=Fit[data,{1/x ,1/x^2  ,1/x^3   } ,x];
cf21[k ]=Coefficient[u,1/x];

 data=Table[{listar[[i]] ,Wu[k][[i]]   },{i,nx-ini,nx-cut}];
u=Fit[data,{1/x ,1/x^2, 1/x^3} ,x];
cW[k ]=Coefficient[u,1/x  ];

,{k,1,ny }]

f01=Table[cf01[k],{k,1,ny }] ;
f11=Table[cf11[k],{k,1,ny }] ;
f21=Table[cf21[k],{k,1,ny }] ;
W3=Table[cW[k],{k,1,ny }] ;
 


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 crucial numerical test 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

test1=1+(2f01+f11 )/(f21);
err1=Max[Abs[test1]]


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRELUCRARE  DATA infinity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

dat=ReadList["fx-inf.txt",Number,RecordLists->True];
lung1=Length[dat] ;

r=Table[dat[[i]][[1]],{i,1,lung1}];
infF1=Table[dat[[i]][[2]],{i,1,lung1}]
infF2=Table[dat[[i]][[3]],{i,1,lung1}];
infF0=Table[dat[[i]][[4]],{i,1,lung1}]
infW=Table[dat[[i]][[5]],{i,1,lung1}];



constINF=Sum[infF0[[i]],{i,1,ny}]/ny
Max[infF0]/Min[infF0]

 constJINF=Sum[infW[[i]],{i,1,ny}]/ny

Max[infW]/Min[infW]


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRELUCRARE  DATA infinity - second dervatives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

dat=ReadList["fxx-inf.txt",{Number,Number ,Number ,Number,Number ,Number  ,Number ,Number ,Number }];
lung1=Length[dat] ;

r=Table[dat[[i]][[1]],{i,1,lung1}];
infF1xx=Table[dat[[i]][[2]],{i,1,lung1}];
infF2xx=Table[dat[[i]][[3]],{i,1,lung1}];
infF0xx=Table[dat[[i]][[4]],{i,1,lung1}];
infWxx=Table[dat[[i]][[5]],{i,1,lung1}]



Mx=Sum[infF0[[i]],{i,1,ny}]/ny


 Mxx=-Sum[infF0xx[[i]],{i,1,ny}]/ny/2


 Jxx=Sum[infWxx[[i]],{i,1,ny}]/ny


Jinterpol=Sum[W3[[i]],{i,1,ny}]/ny
Minterpol=Sum[f01[[i]],{i,1,ny}]/ny;


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
computation Mass from asymptotics
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(* th result:*) 
(*
Series[-g[4,4],{r,Infinity,1}]
1+(2 const-rh)/r+O[1/r]^2
*)

const=Sum[f01[[i]],{i,1,ny}]/ny;

Mc=  constINF;

MSch=rh/2 ;
Mass=MSch+Mc;

Print["Mass Schw     = ",MSch];
Print["Mass correction= ",Mc];
Print["total Mass     = ",Mass];

(*Print[ MSAdS/Mc ]*)


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRELUCRARE  DATA t-0 -- no conical singularities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)


dat=ReadList["f-t0.txt",Number,RecordLists->True];
lung1=Length[dat] ;

r=Table[dat[[i]][[1]],{i,1,lung1}];
t0F1=Table[dat[[i]][[2]],{i,1,lung1}];
t0F2=Table[dat[[i]][[3]],{i,1,lung1}];
t0F0=Table[dat[[i]][[4]],{i,1,lung1}];
t0W=Table[dat[[i]][[5]],{i,1,lung1}];
t0H1=Table[dat[[i]][[6]],{i,1,lung1}];
t0H2=Table[dat[[i]][[7]],{i,1,lung1}];
t0H3=Table[dat[[i]][[8]],{i,1,lung1}];
t0V=Table[dat[[i]][[9]],{i,1,lung1}];

ratio= t0F2-t0F1;




(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRELUCRARE r=0 DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)


dat=ReadList["f-0.txt",Number,RecordLists->True];
lung1=Length[dat] ;

unghi=Table[dat[[i]][[1]],{i,1,lung1}];
hF1=Table[dat[[i]][[2]],{i,1,lung1}]; 
hF2=Table[dat[[i]][[3]],{i,1,lung1}]; 
hF0=Table[dat[[i]][[4]],{i,1,lung1}]; 
hW=Table[dat[[i]][[5]],{i,1,lung1}]; 
hH1=Table[dat[[i]][[6]],{i,1,lung1}];
hH2=Table[dat[[i]][[7]],{i,1,lung1}];
hH3=Table[dat[[i]][[8]],{i,1,lung1}];
hV=Table[dat[[i]][[9]],{i,1,lung1}];  


(*%%%%%%% Hawking temperature %%%%%%%%*)
TH0=1/(4 Pi rh) ;

THc=  1/lung1 Sum[ E^( (hF0[[i]]-hF1[[i]])),{i,1,lung1}];(* correction due to the scalar field*)
TH=TH0 THc;
Print["Schw temp. TH0= ",TH0]
Print["correction THc= ",THc]
Print["TH= ",TH]


errTH=1-Abs[Min[hF0-hF1] /Max[hF0-hF1]];
Print["error TH= ",errTH//N]




(*%%%%%%% event horizon area %%%%%%%%*)
AH0=4 Pi rh^2;
 iAHc=Table[{unghi[[k]],1/2  Sin[unghi[[k]]] E^((hF1[[k]] +hF2[[k]] )) },{k,1,lung1 }];

(* 2 because I integrate between 0, Pi/2 *)
 AHc= 2Integrate[Interpolation[iAHc,InterpolationOrder->1][x],{x,0,Pi/2}];

AH=AH0 AHc;

Print["Schw area  AH0= ",AH0];
Print["correction AHc= ",AHc];
Print["Event horizon area =",AH];



(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
computation Le, Lp -- see MATH code for derivation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

(*Le=2 \[ExponentialE]^F2[0,\[Pi]/2] \[Pi] rH*)
Le=2 E^hF2[[ny]] \[Pi] rh;
Print["Le = ",Le ];

(*Lp=2 \!\(
\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(\[Pi]\)]\(\(
\*SuperscriptBox[\(\[ExponentialE]\), \(F1[0, t]\)]\ rH\)\[DifferentialD]t\)\)*)
Lp1= Table[{unghi[[k]],2 rh    E^hF1[[k]]   },{k,1,lung1 }];
(*Lp=  2ListIntegrate[Lp1,2]//N;(*factor 2: because I integrate [0,pi/2]*) *)
Lp=2 Integrate[Interpolation[Lp1,InterpolationOrder->3][x],{x,Min[Lp1[[All,1]]],Max[Lp1[[All,1]]]}];
Print["Lp = ",Lp ];
Print["  " ];


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 computation MASS from the energy momentum tensor   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

asa1=2;
asa2=IntegerPart[ny/2];
asa3=ny-1;


 (* ordinea este: {T34,T44,Ttot,J4}  *)
q=ReadList["T44.dat",{Number,Number,Number,Number }];
lungq=Length[q];

diference=lungq-ntot;
Print["It must be zero! ",diference];
 

T34=Table[q[[k]][[1]],{k,1,lungq}]; 
ro=Table[q[[k]][[2]],{k,1,lungq}]; 
 Ttot=Table[q[[k]][[3]],{k,1,lungq}]; 
J4=Table[q[[k]][[4]],{k,1,lungq}]; 

(*Se construiesc ny liste pt. marimi de interes la unghiuri fixate *)
(*foarte util in reprezentari grafice *)

Do[ 
T34u[k]=Table[T34[[i]],{i,(k-1)*nx+1,k*nx}]; 
rou[k]=Table[ro[[i]],{i,(k-1)*nx+1,k*nx}]; 
Ttotu[k]=Table[Ttot[[i]],{i,(k-1)*nx+1,k*nx}]; 
J4u[k]=Table[J4[[i]],{i,(k-1)*nx+1,k*nx}]; 
(*	Print[T44u[k]];*)
,{k,1,ny}]



ni=2;
cut= 8;


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 now I compute the Proca field contribution to the total mass
& Smarr law&etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(* sqrt = \[ExponentialE]^(F0[r,t]+2 F1[r,t]+F2[r,t]) r^2 Sin[t]*)

(*Se construiesc ny liste pt. integralele marimilor de interes la unghiuri fixate *)

Do[

 Mio2[k]=Table[{listar[[i]],  E^(F0u[k][[i]]+2 F1u[k][[i]]+F2u[k][[i]])  listar[[i]]  Sqrt[listar[[i]]^2+rh^2]Ttotu[k][[i]]},{i,ni,nx-1}];
 Mio3[k]=Table[{listar[[i]], E^(F0u[k][[i]]+2 F1u[k][[i]]+F2u[k][[i]])  listar[[i]]  Sqrt[listar[[i]]^2+rh^2]  T34u[k][[i]]},{i,ni,nx-1}];
 Mio4[k]=Table[{listar[[i]],  E^(F0u[k][[i]]+2 F1u[k][[i]]+F2u[k][[i]])  listar[[i]]  Sqrt[listar[[i]]^2+rh^2]  J4u[k][[i]]},{i,ni,nx-1}];


,{k,2,ny-1}];



(*Se construiesc ny liste pt. integralele marimilor de interes la unghiuri fixate *)
Do[
(*Ma2[k]=ListIntegrate[Mio2[k],2]//N;
Ma3[k]=ListIntegrate[Mio3[k],2]//N;
Ma4[k]=ListIntegrate[Mio4[k],2]//N;*)
Ma2[k]=Integrate[Interpolation[Mio2[k]1,InterpolationOrder->3][x],{x,Min[Mio2[k][[All,1]]],Max[Mio2[k][[All,1]]]}]//N;
Ma3[k]=Integrate[Interpolation[Mio3[k]1,InterpolationOrder->3][x],{x,Min[Mio3[k][[All,1]]],Max[Mio3[k][[All,1]]]}]//N;
Ma4[k]=Integrate[Interpolation[Mio4[k]1,InterpolationOrder->3][x],{x,Min[Mio4[k][[All,1]]],Max[Mio4[k][[All,1]]]}]//N;

 ,{k,2,ny-1}];

 
Ma2[1]=Ma2[2];
Ma2[ny]=Ma2[ny-1];
 
Ma3[1]=Ma3[2];
Ma3[ny]=Ma3[ny-1];

Ma4[1]=Ma4[2];
Ma4[ny]=Ma4[ny-1];


 Minn2=Table[{unghi[[k]], Sin[unghi[[k]]] Ma2[k]},{k,1,ny}];
  Minn3=Table[{unghi[[k]], Sin[unghi[[k]]] Ma3[k]},{k,1,ny}];
  Minn4=Table[{unghi[[k]], Sin[unghi[[k]]] Ma4[k]},{k,1,ny}];

 (*Mint=-ListIntegrate[Minn2,2]//N; 
Jint=ListIntegrate[Minn3,2]//N; 
Q=ListIntegrate[Minn4,2]//N; *)
Mint=Integrate[Interpolation[Minn2,InterpolationOrder->3][x],{x,Min[Minn2[[All,1]]],Max[Minn2[[All,1]]]}]//N;
Jint=Integrate[Interpolation[Minn3,InterpolationOrder->3][x],{x,Min[Minn3[[All,1]]],Max[Minn3[[All,1]]]}]//N;
Q=Integrate[Interpolation[Minn4,InterpolationOrder->3][x],{x,Min[Minn4[[All,1]]],Max[Minn4[[All,1]]]}]//N;


 Print["Mintegral = ",Mint];  
 Print["Jintegral = ",Jint];  
 Print["Q = ",Q];  



roH=Table[{unghi[[k]],-rou[k][[2]]},{k,2,ny-1}];


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
plot quantities which show the quality of numerics
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

 
 (* ordinea este: {Eq11,Eq12,Ricci,Kr, gauge}  *)
q=ReadList["eq1.dat",{Number,Number,Number,Number,Number  } ];
lungq=Length[q];

diference=lungq-ntot;
Print["It must be zero! ",diference];
 

Eq11=Table[q[[k]][[1]],{k,1,lungq}]; 
Eq12=Table[q[[k]][[2]],{k,1,lungq}]; 
 Ricci=Table[q[[k]][[3]],{k,1,lungq}]; 
Kr=Table[q[[k]][[4]],{k,1,lungq}];  
gauge=Table[q[[k]][[5]],{k,1,lungq}]; (*it should be zero*)

(*Se construiesc ny liste pt. marimi de interes la unghiuri fixate *)
(*foarte util in reprezentari grafice *)

Do[ 
Eq11u[k]=Table[Eq11[[i]],{i,(k-1)*nx+1,k*nx}]; 
Eq12u[k]=Table[Eq12[[i]],{i,(k-1)*nx+1,k*nx}]; 
 Ricciu[k]=Table[Ricci[[i]],{i,(k-1)*nx+1,k*nx}]; 
 Kru[k]=Table[Kr[[i]],{i,(k-1)*nx+1,k*nx}]; 
 gaugeu[k]=Table[gauge[[i]],{i,(k-1)*nx+1,k*nx}]; 
 
,{k,1,ny}]


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
computation MH, JH -- see MATH code for derivation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
dat=ReadList["fxx-0.txt",Number,RecordLists->True];
lung1=Length[dat] ;

unghi=Table[dat[[i]][[1]],{i,1,lung1}];
hF1xx=Table[dat[[i]][[2]],{i,1,lung1}]; 
hF2xx=Table[dat[[i]][[3]],{i,1,lung1}]; 
hF0xx=Table[dat[[i]][[4]],{i,1,lung1}]; 
hWxx=Table[dat[[i]][[5]],{i,1,lung1}]; 
hH1xx=Table[dat[[i]][[6]],{i,1,lung1}];
hH2xx=Table[dat[[i]][[7]],{i,1,lung1}];
hH3xx=Table[dat[[i]][[8]],{i,1,lung1}];
hVxx=Table[dat[[i]][[9]],{i,1,lung1}];  

 
hW2= hWxx;

(* tr44EH=-(1/2) \[ExponentialE]^(f00[t]+f20[t]) rh Sin[t]+(\[ExponentialE]^(-f00[t]+3 f20[t]) w0 Sin[t]^3 (-w0+rh^2 w2[t]))/rh;;*)
 itr44EH=Table[{unghi[[k]],-(1/2) E^(hF0[[k]] +hF2[[k]] ) rh Sin[unghi[[k]]]+  1/rh  E^(-hF0[[k]] +3hF2[[k]] )  hW[[k]] Sin[unghi[[k]]]^3(-hW[[k]] +(1/2)rh^2 hW2[[k]]) },{k,1,lung1 }];
 it=Table[{unghi[[k]],-(1/2) E^(hF0[[k]] +hF2[[k]] ) rh Sin[unghi[[k]]]},{k,1,lung1 }];



(*tr34EH=-\[ExponentialE]^(-f00[t]+3 f20[t]) rh Sin[t]^3 (-w0+rh^2 w2[t]);*)
 itr34EH=Table[{unghi[[k]], -E^(-hF0[[k]] +3hF2[[k]] )  rh Sin[unghi[[k]]]^3(-hW[[k]] +(1/2)rh^2 hW2[[k]])  },{k,1,lung1 }];


(* 2 because I integrate between 0, Pi/2 *)
(*
MH= 2 2 Pi Integrate[Interpolation[itr44EH,InterpolationOrder\[Rule]1][x],{x,0,Pi/2}];
 JH= 2 2 Pi   Integrate[Interpolation[itr34EH,InterpolationOrder\[Rule]1][x],{x,0,Pi/2}];

(* I consider without factor of 4 Pi because I take 4 Pi G=2 2 Pi 
*)
*)

MH=- Integrate[Interpolation[itr44EH,InterpolationOrder->1][x],{x,0,Pi/2}];JH=  1/2 Integrate[Interpolation[itr34EH,InterpolationOrder->1][x],{x,0,Pi/2}];
qt=   -Integrate[Interpolation[it ,InterpolationOrder->1][x],{x,0,Pi/2}];



(* MH=2 TH1/4 AH-2  w   (1/2constJINF -Jint) *)


J=-(1/2)constJINF;
MInt=Mint 1/2 2alfa^2;

errM=1-(MH+MInt)/Mass;
errJ=1-(JH+Jint)/J;
Print["M   = ",Mass];
Print["MInt= ",MInt];
Print["MH  = ",MH];
Print["Merror = ",errM];

Print[""];

Print["J   = ",J];
Print["Jint= ",Jint];
Print["JH  = ",JH];
Print["Jerror= ",errJ];


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 Smarr law 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

OmegaH=w;

Print["total mass     = ",Mass];
Print["mass integral  = ", Mint ];
Print["factor TH S    = ",2 TH 1/4 AH];
Print["factor OmegaHJ = ",2 OmegaH   (J -Jint)];
Print[" "];

(* smarr relation *)
errSmarr=1-(2(TH 1/4 AH +OmegaH(J-Jint))+MInt)/Mass;
Print[" errSmarr= ",errSmarr]; 

 





asa= Table[{w,alfa,nr,Mint,Jint,Q,Minterpol,Jinterpol,Mx,Jxx  ,minF0,maxF0,minF1,maxF1,minF2,maxF2,minW,maxW,minH1,maxH1,minH2,maxH2,minH3,maxH3,minV,maxV,f00,f10,f20,f0Pi2,f1Pi2,f2Pi2 ,Max[Abs[Eq11]],Max[Abs[Eq12]],Max[Abs[gauge]],Le,Lp,MH,JH,Mass,MInt,J,Jint,AH,TH ,rh}] 





stmp=OpenAppend["tmp.txt"];
Write[stmp,asa];
Close[stmp] ;
 
 
