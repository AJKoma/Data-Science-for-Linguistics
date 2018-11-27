C
C ASJP62xo: Levenshtein distance from variable lists, * " ~ $ define units, divides,
C cutoff for list length, loans optional, output for old Mega
C	
	CHARACTER*1 TITLE(8000,300),AA(400),FORM(72),AB(100)
	CHARACTER*1 BLA,TAB,SLA,XXX,COM,PIP,RPA,STA,QUO,TIL,DOL
	INTEGER XB(100),CT(2000),CD(2000),BT(2000,2),BD(2000,3)
	INTEGER AW(100),AX(100),AY(100),AZ(100),DD(8000)
	INTEGER*2 B(8000,100,100),XX(8000,100,2)
	DOUBLE PRECISION PMJ,PNJ,QMJ,QNJ
	DATA TAB/'	'/,BLA/' '/,SLA/'/'/,XXX/'X'/,COM/','/
	DATA PIP/'|'/,RPA/'}'/,STA/'*'/,QUO/'"'/,TIL/'~'/,DOL/'$'/
	READ (5,18) NWORD,NTW,NDATE,LWE
	IF (NWORD.EQ.0) NWORD=1
	RDATE=-NDATE
	READ (5,12) FORM
	DO 88 J=1,100
88	XB(J)=0
	DO 90 J=1,101
	READ (5,FORM) K
	IF (K.EQ.0) GO TO 92
90	XB(K)=J
92	NIW=J-1
	DO 120 J=1,100
	READ (5,14) AB(J)
120	IF (AB(J).EQ.BLA) GO TO 122
122	NSY=J-1
	READ (5,12) AA
	JJ=0
	NST=NSY*3
	NS=NST
	MT=0
	MD=0
	NT=100
200	READ (5,12) AA
c	write (6,12) aa
      IF (AA(1).EQ.BLA) GO TO 500
	IF (NID(AA).GT.0) GO TO 212
202	READ (5,16) NAB,POP
	IF ((POP.LT.RDATE).OR.(POP.GE.-1)) GO TO 206
204	READ (5,12) AA
	IF (NID(AA).GT.0) GO TO 204
      IF (AA(1).EQ.BLA) GO TO 500
	GO TO 202
206	IF (NT.GE.NTW) JJ=JJ+1
	NT=0
	NTI=0
	DO 207 K=1,300
207	TITLE(JJ,K)=BLA
	DO 208 J=1,300
	IF (AA(J).EQ.PIP) GO TO 209
	IF ((AA(J).EQ.TAB).OR.(AA(J).EQ.BLA)) GO TO 210
208	TITLE(JJ,J)=AA(J)
209	TITLE(JJ,J)=RPA
210	DO 211 IJ=1,100
	XX(JJ,IJ,1)=0
211	XX(JJ,IJ,2)=0
	GO TO 200	 
212	IJ=XB(NID(AA))
	IF (IJ.EQ.0) GO TO 200
	NWOR=1
	NN=0
	K=0
	DO 220 J=2,400
220	IF ((AA(J).EQ.TAB).AND.(AA(J+1).NE.TAB)) GO TO 222
222	JP=J+1
	DO 230 J=JP,400
230	IF ((AA(J).NE.BLA).AND.(AA(J).NE.SLA)) GO TO 232
232	JP=J
	IF (LWE.EQ.0) GO TO 238
	CALL LOANS(AA,JP)
c	write (6,20) aa
238	IF ((AA(JP).EQ.XXX).AND.(AA(JP+1).EQ.XXX).AND.(AA(JP+2).EQ.XXX)) 
	1GO TO 280
	DO 270 J=JP,400
	IF (AA(J).NE.BLA) GO TO 240
	IF ((AA(J+1).EQ.BLA).OR.(AA(J+1).EQ.SLA)) GO TO 280
	GO TO 270
240	IF (AA(J).NE.COM) GO TO 252
	NWOR=NWOR+1
	IF (NWOR.LE.NWORD) GO TO 250
	NWOR=NWORD
	GO TO 280
250	NN=50
	IF (K.GT.50) K=50
	XX(JJ,IJ,1)=K
	K=0
	GO TO 270
252	IF (AA(J).NE.QUO) GO TO 254
	LL=NSY+B(JJ,IJ,NN+K)
	GO TO 268
254	IF (AA(J).NE.STA) GO TO 256
	LL=2*NSY+B(JJ,IJ,NN+K)
	GO TO 268
256	IF (AA(J).NE.TIL) GO TO 258
	LL=ITIL(B,JJ,IJ,NN+K,BT,CT,MT,NS)
	K=K-1
	GO TO 268
258	IF (AA(J).NE.DOL) GO TO 264
	LL=IDOL(B,JJ,IJ,NN+K,BD,CD,MD,NS)
	K=K-2
	GO TO 268
264	LL=ID(AA(J),AB,NSY)
	IF (LL.EQ.0) GO TO 270
	IF ((NN+K).GE.100) GO TO 280
	K=K+1
268	B(JJ,IJ,NN+K)=LL
270	CONTINUE	 
280	XX(JJ,IJ,NWOR)=K
	IF (K.GT.0) NT=NT+1
	GO TO 200
500	JT=JJ
	IF (NT.LT.NTW) JT=JT-1
	WRITE (6,30)
	WRITE (6,31)
	WRITE (6,32) JT
	WRITE (6,33)
	WRITE (6,34) JT
	WRITE (6,35)
	WRITE (6,40)
	DO 510 J=1,JT
510	WRITE (6,20) J,(TITLE(J,K),K=1,300)
	WRITE (6,22)
	WRITE (6,24) (J,J=1,JT)
	WRITE (6,26)
	J=1
	WRITE (6,28) J
	DO 670 JJ=2,JT
	JM=JJ-1
	DO 668 JK=1,JM
	PMJ=0.
	PNJ=0.
	QMJ=0.
	QNJ=0.
	DO 662 IJ=1,NIW
	MXJ=XX(JJ,IJ,1)
	IF (MXJ.EQ.0) GO TO 662
	DO 610 J=1,MXJ
610	AW(J)=B(JJ,IJ,J)
	NXJ=XX(JJ,IJ,2)
	IF (NXJ.EQ.0) GO TO 620
	DO 612 J=1,NXJ
612	AX(J)=B(JJ,IJ,50+J)
620	DO 660 IK=1,NIW
	MXK=XX(JK,IK,1)
	IF (MXK.EQ.0) GO TO 660
	DO 630 J=1,MXK
630	AY(J)=B(JK,IK,J)
	NXK=XX(JK,IK,2)
	IF (NXK.EQ.0) GO TO 640
	DO 632 J=1,NXK
632	AZ(J)=B(JK,IK,50+J)
640	TT=1.
	TD=DLEV(AW,MXJ,AY,MXK)
	IF (NXJ.EQ.0) GO TO 642
	TT=TT+1.
	TD=TD+DLEV(AX,NXJ,AY,MXK)
642	IF (NXK.EQ.0) GO TO 650
	TT=TT+1.
	TD=TD+DLEV(AW,MXJ,AZ,NXK)
	IF (NXJ.EQ.0) GO TO 650
	TT=TT+1.
	TD=TD+DLEV(AX,NXJ,AZ,NXK)
650	TD=TD/TT
	IF (IJ.NE.IK) GO TO 654
	PNJ=PNJ+1.
	PMJ=PMJ+TD
	GO TO 660
654	QNJ=QNJ+1.
	QMJ=QMJ+TD 
660	CONTINUE
662	CONTINUE
	DDD=.5+10000.*pmj*qnj/(pnj*qmj)
668	DD(JK)=IFIX(DDD)
	WRITE (6,28) JJ,(DD(K),K=1,JM)	
670	CONTINUE
	STOP
12    FORMAT (400A1)
14	FORMAT (A1,I4)
16	FORMAT (I2,16X,F12.0)
18    FORMAT (12I6)
20	FORMAT (' [',I4,'] #',300A1)
22	FORMAT (' ')
24	FORMAT (' [ ',204I5,2X,40(3X,204I5,2X))
26	FORMAT (' ]')
28	FORMAT (' [',I4,']',169I6,4X,50(1X,170I6,4X))
30	FORMAT (' #mega')
31	FORMAT (' !Title: Concatenated Files;')
32	FORMAT (' !Format DataType=Distance DataFormat=LowerLeft NTaxa=',
	1I4,';')
33	FORMAT (' !Description')
34	FORMAT ('   No. of Taxa :',I5)
35	FORMAT ('   No. of Groups :  1')
40	FORMAT (' ;')
      END
	FUNCTION NID(AA)
	CHARACTER*1 AB(10),AA(400)
	DATA AB(1)/'0'/,AB(2)/'1'/,AB(3)/'2'/,AB(4)/'3'/,AB(5)/'4'/,
	1AB(6)/'5'/,AB(7)/'6'/,AB(8)/'7'/,AB(9)/'8'/,AB(10)/'9'/
	NID=0
	DO 150 J=1,3
	DO 140 ID=0,9
140	IF (AA(J).EQ.AB(ID+1)) GO TO 150
	RETURN
150	NID=10*NID+ID	
	RETURN
	END 
	FUNCTION ID(AX,AB,NSY)
	CHARACTER*1 AB(100),AX
	DO 140 ID=1,NSY
140	IF (AX.EQ.AB(ID)) RETURN
	ID=0
	RETURN
	END
	FUNCTION ITIL(B,JJ,IJ,K,BT,CT,MT,NS)
	INTEGER BT(2000,2),CT(2000)
	INTEGER*2 B(8000,100,100)
	N1=B(JJ,IJ,K-1)
	N2=B(JJ,IJ,K)
	IF (MT.EQ.0) GO TO 142
	DO 140 IT=1,MT
	IF (N1.NE.BT(IT,1)) GO TO 140
	IF (N2.NE.BT(IT,2)) GO TO 140
	ITIL=CT(IT)
	RETURN
140	CONTINUE
142	MT=MT+1
	BT(MT,1)=N1
	BT(MT,2)=N2
	NS=NS+1
	ITIL=NS
	CT(MT)=NS
	RETURN
	END 
	FUNCTION IDOL(B,JJ,IJ,K,BD,CD,MD,NS)
	INTEGER BD(2000,3),CD(2000)
	INTEGER*2 B(8000,100,100)
	N1=B(JJ,IJ,K-2)
	N2=B(JJ,IJ,K-1)
	N3=B(JJ,IJ,K)
	IF (MD.EQ.0) GO TO 142
	DO 140 ID=1,MD
	IF (N1.NE.BD(ID,1)) GO TO 140
	IF (N2.NE.BD(ID,2)) GO TO 140
	IF (N3.NE.BD(ID,3)) GO TO 140
	IDOL=CD(ID)
	RETURN
140	CONTINUE
142	MD=MD+1
	BD(MD,1)=N1
	BD(MD,2)=N2
	BD(MD,3)=N3
	NS=NS+1
	IDOL=NS
	CD(MD)=NS
	RETURN
	END 
	SUBROUTINE LOANS(AA,JP)
	CHARACTER*1 AA(400),COM,PCT,BLA,SLA,XXX
	DATA COM/','/,PCT/'%'/,BLA/' '/,SLA/'/'/,XXX/'X'/
	NP1=0
	NP2=0
	NCO=0
	DO 190 J=JP,400
	IF (AA(J).NE.BLA) GO TO 110
	IF ((AA(J+1).EQ.BLA).OR.(AA(J+1).EQ.SLA)) GO TO 200
110	IF (AA(J).NE.COM) GO TO 120
	IF (NCO.GT.0) GO TO 112
	NCO=J
	GO TO 190
112	AA(J)=BLA
	AA(j+1)=SLA
	GO TO 200
120	IF (AA(J).NE.PCT) GO TO 190
	IF (NP1.GT.0) GO TO 130
	NP1=J
	GO TO 190
130	NP2=J
190	CONTINUE
200	IF (NP1.EQ.0) RETURN
	IF ((NCO.GT.0).AND.(NP2.EQ.0)) GO TO 210
	AA(JP)=XXX	
	AA(JP+1)=XXX
	AA(JP+2)=XXX
	RETURN
210	IF (NP1.LT.NCO) GO TO 220
	AA(NCO)=BLA
	AA(NCO+1)=SLA
	RETURN
220	JPP=NCO+1
	JPM=JP-1
	DO 230 J=1,400
	AA(JPM+J)=AA(JPP+J)
	IF (AA(JPM+J).NE.BLA) GO TO 230
	IF ((AA(JPP+J+1).EQ.BLA).OR.(AA(JPP+J+1).EQ.SLA)) GO TO 240
230	CONTINUE
240	AA(JP+J)=SLA
	RETURN
	END		  			 
	FUNCTION DLEV(AW,MXJ,AY,MXK)
	INTEGER AW(100),AY(100)
	REAL D(100,100)
	DMAX=MAX(MXJ,MXK)
	NXJ=MXJ+1
	NXK=MXK+1
	DO 110 J=1,NXJ
110	D(J,1)=J-1
	DO 120 K=2,NXK
120	D(1,K)=K-1
	DO 130 J=2,NXJ
	DO 130 K=2,NXK
	C=1.
	IF (AW(J-1).EQ.AY(K-1)) C=0.
130	D(J,K)=MIN((D(J-1,K)+1.),(D(J,K-1)+1.),(D(J-1,K-1)+C))
	DLEV=D(NXJ,NXK)/DMAX
	RETURN
	END