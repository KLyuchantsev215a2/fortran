      subroutine dysai
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c .                                                                   .
c .   p r o g r a m                                                   .
c .        to input control information and nodal points data         .
c .                                                                   .
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c .   s.n.k/    m.i.b/  26.07.93 a.n.s/ 30.06.93   v.v.a/ 16.01.94    .
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      implicit real*8(a-h,o-z)
c
      common /maina/ numnp,nste,mtot,modex,numce,itwo
      common /hed/ hed(20)
      common /max/ mce,ndof,nlcur
      common /dim/ n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1             n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28
      common /dim2/ n4b1,n4b2,n4b3,n4b4,n4b5,n4b6,n4b7,
     1              n4b8,n4b9,n4b10,n4b11,n4a1,n4a2,n4a3
      common /el/ ind,icount,npar(20),numeg,negl,negnl,klin,nfirst,nlast
      common /dynam/ imass,idamp,istat,ieig,imassn,idampn,inocmd
      common /sol/ neq,nwk,nwm,nwc,numest,midest,maxest,mk
      common /solb/ nblmax,nblock,lblock,lblmax,lbln,icore,MXLN
      common /const/ dt,dta,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,iope,olddt
      common /dampng/ adamp,bdamp
      common /dysain/ opvar(2),tstart,ipri
      common /prcon/ npb,idc,ivc,iac,ipc,ipnode(2,8),ntpatr,ipatr
      common /tapes/ nt1,nt2,nt3,nt4,nt7,nt8,nt9,nt10,nt11,nt12,nt13
      common /tapesi/ iin,iout
      common /id/ idof(6)
      common /load/ nload,nptm,mcurg,npr2dc,npr2dn,ndisp
      common /load3/ npr3dc(3),npr3dn(3)
      common /loadsh/ npr2sc,npr2sn,npr3sc,npr3sn,npr3bc,npr3bn
      common /flag/ itempf,igrav,iconsv,numnps,itmgf1,itmgf2
      common /nonlin/ ite,itemax,iref,ieqref,numref,iequit,isref,itetyp new
      common /norms/ rnorm,renorm,dnorm,energy,dtol,ftol,etol,
     1               tolls,dn1,dn2,rhsn
      common /nonln1/ nutmre,lcnstr,n20a                                new
      common /arclnc/ nustar,iustar,nstar,ustar,alph,delt,iteopt,psi,   new
     1                idetrf                                            new
      common /arclnv/ unoral,unorac,deltln,extw,detn,mantdn,deto,mantdo,new
     1                alamb,alambo,lcmkey,karlew,iteold,alambi,alambx   new
c      common / sw1 / sp1,ds0,cs01,cs02,rrsp,linsch,igso,irco,idmp      new
c      common / sw2 / rlnew,alfa0,dsx,iteopt,idctrl,riksf,nuback,mthunl new
      common /total/ itrlas,irflas,irhlas,itrtot,irftot,irhtot
      common /cont/ ovnor0,ovnorm,otol,tolcg,ncontb,nparc(10),nf,nl,
     1              ninf,ioverl,icontf,iminit,minitm,itcgm,neqc
      common /contp/ iconp
      common /dymod/ modal,idampm,ifreq,itrans,irand
      common /loadf/ ws,we,ifft,ltype,npfm,isc,iunit,nbase,ibamp
      common /eigen/ nfreq,cofq,ifpr,iffr,numpro,nc,ifss,nitem,rtol
      common /buckln/ ibuckl
      common /skew/ n0,n00,nskew
      common /joint/ njoint,numsln
      common /energ/ienerf,energk,energu,energt,energr(20)
      common a(60000000)
c
      real a,hed
c
c
c     r e a d   c o n t r o l   i n f o r m a t i o n
c
c
      numref=0
      olddt=0.0
      read (iin,1000) hed,numnp,(idof(i),i=1,6),negl,negnl,imass,idamp,
     1                modex,nste,dt,tstart,ieig,ipri,nskew
      if (numnp.eq.0) stop
      read (iin,1010) numce,mce,numnps,icore,itempf,itmgf1,itmgf2,
     1                iconp,ncontb,ienerf,numsln,njoint,ndisp
      read (iin,1030) imassn,idampn,iope,opvar,modal,idampm,
     1                ifreq,itrans,irand,olddt
      if (ieig.ne.0)
     *read (iin,1031) nfreq,cofq,ifpr,iffr,numpro,nc,ifss,nitem,rtol,
     *                ibuckl
      if (ifreq.ne.0)
     *read (iin,1032) ws,we,isc,iunit
      if ((negnl.ne.0).and.(iope.ne.3))
     *read (iin,1035) isref,nutmre,iequit,itemax,itetyp,dtol,ftol,etol, new
     1                lcnstr                                            new
      if (lcnstr.ne.0)                                                  new
     *read (iin,1038) nustar,iustar,ustar,alph,delt,iteopt,psi,idetrf   new
c      if (itetyp.eq.6.or.itetyp.eq.7)
c     *read (iin,1036)
c     1 idctrl,iteopt,ds0,dsx,irco,idmp,nuback,linsch,tolls
      if (iconp.ne.0)
     *read (iin,1037) ioverl,icontf,iminit,minitm,itcgm,otol,tolcg
      read (iin,1040) npb,idc,ivc,iac
      read (iin,1050) ipnode
c
c     verify and initialize solution variables
c
      numeg=negl+negnl
c
      if(nitem.eq.0) nitem=16
      if(rtol.eq.0) rtol=1.e-06
      if (ieig.ne.0.and.nc.eq.0) nc=min0(2*nfreq,nfreq+8)
      ndof=6
      do 10 i=1,6
   10 ndof=ndof-idof(i)
      if (ipri.eq.0) ipri=1
      istat=1
      if (imass.eq.0) istat=0
      if ((idamp .ne.0).and.(imass.eq.0)) go to 20
      if ((idampn.ne.0).and.(imass.eq.0)) go to 20
      if ((idampm.ne.0).and.(imass.eq.0)) go to 20
      if ((imassn.ne.0).and.(imass.eq.0)) go to 20
      go to 30
   20 write (iout,3000)
      stop
c  30 if ((idamp.eq.2).and.(idampm.eq.0)) go to 31
c     go to 32
c  31 write (iout,3001)
c     stop
   30 dta=dt
      klin=1
      if (negnl.eq.0) klin=0
      if (ieig.le.3) go to 35
      write (iout,3010)
      stop
   35 if ((klin.eq.0).or.(iope.eq.3)) go to 37
      if (isref.eq.0) isref=1
      if (iequit.eq.0) iequit=1
      if (itemax.eq.0) itemax=15
      if (itetyp.eq.0) itetyp=1
      if (ioverl.eq.0) ioverl=1
      if (icontf.eq.0) icontf=1
      if (iminit.eq.0) iminit=1
      if (minitm.eq.0) minitm=15
      if (itcgm.eq.0)  itcgm=15
      if (ieig.eq.2.and.ibuckl.eq.0) ibuckl=1                           new
      if (dtol.eq.0.0) dtol=1.0d-03
      if (ftol.eq.0.0) ftol=1.0d-03
      if (etol.eq.0.0) etol=1.0d-03
      if (otol.eq.0.0) otol=1.0d-03
      if (tolcg.eq.0.0) tolcg=1.0d-10
c
      if (lcnstr.ne.0) then                                             new
      if (alph.eq.0.) alph=10.                                          new
      if (delt.eq.0.) delt=0.15                                         new
      if (iteopt.eq.0) iteopt=6                                         new
      if (psi.ne.0.) psi=psi*psi                                        new
      if (modex.le.1) lcmkey=1                                          new
      end if                                                            new
cc
cc  arc-length
cc
c      igso  =0
cc    termtm=dt*ntime
c      if (itetyp.eq.6.or.itetyp.eq.7) then
c      alfa0 =1.0
c      rlnew =0.0
c      riksf =dt
c      rrsp=1.
c      numspu=0
c      if (iteopt.eq.0) iteopt=11
c      if (idmp.eq.0)  idmp=0
c      if (irco.eq.0)  irco  =0
c      if (dsx.eq.0.0)  dsx=0.0
c      if (linsch.eq.0) linsch=1
c      if (nuback.eq.0) nuback=10
c      if (idctrl.eq.0) idctrl=0
c      if (numref.eq.0) numref=10
ccc    if (maxref.eq.0) maxref=15
ccc    if (sltol.eq.0.) sltol=.0010
ccc    if (slhrd.eq.0.) slhrd=.0100
ccc    if (sltol.lt.0.) sltol=0.000
ccc    if (slhrd.lt.0.) slhrd=0.000
c      if (tolls.eq.0.) tolls=.90
c      endif
      go to 40
   37 isref=nste+1
      iequit=nste+1
   40 continue
      if (npb.le.8) go to 42
      write (iout,3020) npb
      stop
   42 if (iope.ne.3) go to 46
      if (imass.eq.1) go to 44
      write (iout,3030)
      stop
   44 if (idamp.eq.0) go to 46
      write (iout,3040)
      stop
   46 ipc=idc+ivc+iac
      if (npb.ne.0) go to 50
      ipc=1
      npb=1
      idc=1
      ivc=1
      iac=1
      ipnode(1,1)=1
      ipnode(2,1)=numnp
   50 continue
      if (iope.le.0) iope=1
c     if ((numnps.ne.0).and.(ishelf.eq.0)) ishelf=2
c
      write (iout,2000) hed,numnp,(idof(i),i=1,6),negl,negnl
      write (iout,2010) imass,idamp,modex
      write (iout,2015) nste,dt,tstart,ieig,ipri,nskew
      write (iout,2016) ifreq,itrans,irand
      write (iout,2020) numce,mce,numnps,icore,itempf,numsln,njoint
      write (iout,2025) itmgf1,itmgf2,iconp,ncontb,ienerf,ndisp,
     *                  imassn,idampn,idampm
      write (iout,2030) istat,klin,modal
      if (ieig.ne.0)
     *write (iout,2031) nfreq,cofq,iffr,ifpr,numpro,nc,ifss,nitem,rtol,
     *                  ibuckl
      if (ifreq.ne.0)
     *write (iout,2032) ws,we,isc,iunit
      if ((klin.ne.0).and.(iope.ne.3))
     *write (iout,2035) isref,nutmre,iequit,itemax,itetyp,dtol,ftol,    new
     1                  etol,lcnstr                                     new
      if (lcnstr.ne.0)                                                  new
     *write (iout,2036) nustar,iustar,ustar,alph,delt,iteopt,psi,idetrf new
c      if (itetyp.eq.6.or.itetyp.eq.7) then
c      write(iout,2052) idctrl,idirw,dsx
c      write(iout,2054) irco,idmp,tolls,nuback,linsch,iteopt
c      endif
      if (iconp.ne.0)
     *write (iout,2037) ioverl,icontf,iminit,minitm,itcgm,otol,tolcg
c
      inocmd=1
      if ((idamp.eq.0).and.(imass.ne.2)) inocmd=0
      if (idampn.ne.0) inocmd=1
      if ((klin.ne.0).and.(iope.ne.3)) dtol=dtol*dtol
      if ((klin.ne.0).and.(iope.ne.3)) ftol=ftol*ftol
      if ((istat.eq.0).or.(nste.eq.0)) go to 60
c
c     set time integration coefficients in case of dynamic problem
c
      call opcoef(opvar)
c
      write (iout,2040) iope
      if (iope.eq.1)
     *write (iout,2050) opvar(1)
      if (iope.eq.2)
     *write (iout,2060) opvar(1),opvar(2)
c
c     print disp/vel/acc output information
c
   60 write (iout,2070) npb,idc,ivc,iac
      do 70 l=1,npb
   70 write (iout,2080) l,l,ipnode(1,l),l,ipnode(2,l)
c
c
c     r e a d   n o d a l   p o i n t   d a t a
c
c
      n0  = 1
      n00 = n0  + itwo*9*nskew +2
      n1  = n00 + numnp
      n2  = n1  + mce*numce +2 + numsln*mce*ndof
      n3  = n2  + mce*numce +2 + numsln*mce*ndof
      if ((itwo.eq.2).and.(mod(n3,2).eq.0)) n3=n3+1
      n4  = n3  + mce*numce*itwo +2 + numsln*mce*ndof*itwo
      n4b1 = n4  + ndof*numnp +2
      n4b2 = n4b1 + ndisp * 2
      n4b3 = n4b2 + ndisp
      n4b4 = n4b3 + ndisp
      n4b5 = n4b4 + ndisp
      n4b6 = n4b5 + ndisp
      n4b7 = n4b6 + ndisp
      if (itwo.eq.2.and.mod(n4b7,2).eq.0) n4b7=n4b7+1
      n4b8 = n4b7 + ndisp*itwo
      n4b9 = n4b8 + ndisp*itwo
      n5  = n4b9 + ndisp*itwo
      n6  = n5  + numnp*itwo
      n7  = n6  + numnp*itwo
      n9  = n7  + numnp*itwo
      n10 = n9  + numnps*itwo
      n11 = n10 + numnps*itwo
      n13 = n11 + numnps*itwo
      n14 = n13 + numnps
      n15 = n14 + numnps
      n16 = n15 + numnps
      n17 = n16 + numnp
      if (n17.gt.mtot) call error (n17-mtot,1)
c
c     constraint equation data
c
      if (numce.gt.0)
     *call inptce (a(n1),a(n2),a(n3),numce,mce,numnp)
c
c     nodal point data
c
c Soldatkin      call input (a(n00),a(n1),a(n2),a(n3),a(n4),a(n5),a(n6),a(n7),
c Soldatkin     1            a(n16),numce,mce,numnp,ndof,neq)
      call input (a(n00),a(n4),a(n5),a(n6),a(n7),
     1            a(n16),numnp,ndof,neq)
c
c     node displacement reference system sets
c
      if(nskew.ne.0) call inpref (a(n0),nskew)
c
c     constrain component of displacement vector for load constraint    new
c     algorithm                                                         new
c
      if (lcnstr.ne.0)                                                  new
     *call condis (a(n4),ndof)                                          new
c
c     shell nodal point data
c
      if (numnps.gt.0)
     *call inptuv (numnps,a(n9),a(n10),a(n11),a(n13),a(n14),a(n15),
     1            a(n5),a(n6),a(n7))
c
c     evaluate constants for gauss numerical integration
c
      call xgwgt
c
c     nodal displacement data
c
      if (ndisp.eq.0) go to 71
      n14 = n13 + ndisp*itwo
      n15 = n14 + ndisp*itwo
      n16 = n15 + ndisp*itwo
      n17 = n16 + ndisp
      n18 = n17 + ndisp
      if (n18.gt.mtot) call error (n18-mtot,1)
c
      call inpdis (a(n4),a(n4b1),a(n4b2),a(n4b3),a(n4b7),a(n4b8),
     1             a(n4b9),a(n13),a(n14),a(n15),a(n16),a(n17),ndof)

c
c     c a l c u l a t e   a n d   s t o r e   l o a d   v e c t o r s
c
   71 read  (iin,1060)  nlcur,nptm,nload,nbase,ltype,ifft
      write (iout,2090) nlcur,nptm,nload,nbase,ltype,ifft
      read  (iin,1050)  igrav,mcurg
      write (iout,2092) igrav,mcurg
      if ((igrav.eq.0).and.(mcurg.eq.0)) go to 72
      if ((igrav.ne.0).and.(mcurg.gt.0).and.(mcurg.le.nlcur)) go to 72
      write (iout,3042)
      stop
c
   72 read (iin,1050) npr2dc,npr2dn,(npr3dc(i),npr3dn(i),i=1,3),
     1                npr2sc,npr2sn,npr3sc,npr3sn,npr3bc,npr3bn
      write (iout,2093)
      write (iout,2094) npr2dc,npr2dn
      write (iout,2095)
      write (iout,2094) npr3dc(1),npr3dn(1)
      write (iout,2096)
      write (iout,2094) npr3dc(2),npr3dn(2)
      write (iout,2097)
      write (iout,2094) npr3dc(3),npr3dn(3)
      write (iout,2098)
      write (iout,2094)  npr2sc,npr2sn
      write (iout,2099)
      write (iout,2094)  npr3sc,npr3sn
      write (iout,2091)
      write (iout,2094)  npr3bc,npr3bn
      iconsv=0
      iconsv = npr2dn + npr3dn(1) + npr3dn(2) + npr3dn(3) +
     1         npr2sn + npr3sn + npr3bn
      if (iconsv.gt.0) iconsv=1
c
      n13a= n13 + nlcur*nste*itwo       !-- rg
      n13b= n13a+ nlcur*nste*2*itwo     !-- rf
      n14 = n13b+ nste*itwo             !-- omega
      if (ifft.eq.0.and.ltype.eq.0) n14=n13a
      if ((itwo.eq.2).and.(mod(n14,2).eq.0)) n14=n14+1
      if (ifft.eq.0.and.ltype.gt.0) n13a=n13
      n14a= n14 + neq*itwo              !-- r - rc
      n15 = n14a+ neq*itwo              !-- rs
      if (ltype.eq.0) n15=n14a
      n16 = n15 + nptm*itwo +2          !-- rv  - amp
      n16a= n16 + nptm*itwo +2          !-- tim - omeg
      n17 = n16a+ nptm*itwo +2          !-- phas
      if (ltype.eq.0) n17=n16a
      n18 = n17 + nload +2              !-- nod
      n19 = n18 + nload +2              !-- idirn
      n20 = n19 + nload +2              !-- ncur
      n21 = n20 + nload +2              !-- kstar
      if ((itwo.eq.2).and.(mod(n21,2).eq.0)) n21=n21+1
      n22 = n21 + nload*itwo +2         !-- fac
      n23 = n22 + nload*itwo +2         !-- arvtm
      if (n23.gt.mtot) call error (n23-mtot,2)
c
      if(mce.eq.0) mce=1
      if(ndof.eq.0) ndof=1
      nlcur1=nlcur
      if(nlcur.eq.0) nlcur1=1
      nfcur1=nlcur1*2
      call loads (a(n1),a(n2),a(n3),a(n4),a(n5),a(n6),a(n7),a(n13),
     1            a(n14),a(n13a),a(n14),a(n14a),a(n15),a(n16),a(n16a),
     2            a(n17),a(n18),a(n19),a(n20),a(n21),a(n22),a(n13b),
     3            nlcur,ndof,mce,n23,nlcur1,nfcur1)
c
c
c     i n p u t  r a y l e i g h  d a m p i n g  c o e f f i c i e n t s
c
c
      if (idamp.ne.1) go to 80
      read  (iin,1070)  adamp,bdamp
      write (iout,2100) adamp,bdamp
      if ((adamp.ne.0.).or.(bdamp.ne.0.)) go to 90
      write (iout,3060)
      stop
   80 adamp=0.0
      bdamp=0.0
c
c
c     e s t a b l i s h  c o n c e n t r a t e d   n o d a l
c           m a s s   a n d   d a m p i n g   v e c t o r s
c
c
   90 if (istat.eq.0) go to 100
      call nodmas (a(n1),a(n2),a(n3),a(n4),a(n14),ndof,mce)
c
c
c     i n p u t   m o d a l   d a m p i n g   r a t i o s
c
c
      if (idamp.ne.2) go to 100
      call modamp(a(n14),idampm)
c
c
c     f i n d   i n i t a l   c o n d i t i o n s
c
c
  100 n16 = n15 + neq*itwo
      n17 = n16 + neq*itwo
      n18 = n17 + neq*itwo
      if ((klin.eq.0).and.(iope.ne.3)) n18=n17
      if (istat.ne.0) go to 110
      n17=n15
      n18=n16
  110 continue
      if (n18.gt.mtot) call error (n18-mtot,2)
c
      call inital (a(n14),a(n15),a(n16),a(n17),a(n4))
c
      return
c
c
 1000 format (20a4/i5,6i1,i4,5i5,2f10.0,3i5)
 1010 format (16i5)
 1030 format (3i5,2f10.0,5i5,f10.0)
 1031 format (i5,e10.4,6i5,e10.4,i5)
 1032 format (2e10.4,3i5)
 1035 format (5i5,3e10.4,i5)                                            new
 1038 format (2i5,3f10.0,i5,f10.0,i5)                                   new
 1036 format(2i5,2e10.0,4i5,e10.0)
 1037 format (5i5,2f10.0)
 1040 format (4i5)
 1050 format (16i5)
 1060 format (6i5)
 1070 format (2f10.0)
c
 2000 format (/////,1x,20a4 ///
     155h c o n t r o l  i n f o r m a t i o n                     //5x,
     255hnumber of nodal points  . . . . . . . . . . (numnp)   =,i5//5x,
     355hmaster x-translation code . . . . . . . . . (idof(1)) =,i5//5x,
     455hmaster y-translation code . . . . . . . . . (idof(2)) =,i5//5x,
     555hmaster z-translation code . . . . . . . . . (idof(3)) =,i5//5x,
     655hmaster x-rotation    code . . . . . . . . . (idof(4)) =,i5//5x,
     755hmaster y-rotation    code . . . . . . . . . (idof(5)) =,i5//5x,
     855hmaster z-rotation    code . . . . . . . . . (idof(6)) =,i5//5x,
     955hnumber of linear element groups . . . . . . (negl)    =,i5//5x,
     a55hnumber of nonlinear element groups  . . . . (negnl)   =,i5    )
 2010 format (/5x,
     155hmass matrix code  . . . . . . . . . . . . . (imass)   =,i5 /5x,
     255h   eq.0, no mass effects                                   /5x,
     355h   eq.1, lumped mass                                       /5x,
     455h   eq.2, consistent mass                                  //5x,
     555hdamping matrix code . . . . . . . . . . . . (idamp)   =,i5 /5x,
     655h   eq.0, no rayleigh damping                               /5x,
     755h   eq.1, rayleigh damping                                  /5x,
     *55h   eq.2, modal damping ratios corresponding                /5x,
     *55h         to the circular frequencies                      //5x,
     855hsolution mode . . . . . . . . . . . . . . . (modex)   =,i5 /5x,
     955h   eq.0, data check                                        /5x,
     a55h   eq.1, execution                                         /5x,
     b55h   eq.2, restart                                              )
 2015 format (/5x,
     155hnumber of time steps  . . . . . . . . . . . (nste)    =,i5//5x,
     251htime step increment . . . . . . . . . . . . (dt)  =, e11.4//5x,
     351htime at solution start  . . . . . . . . . (tstart)=, e11.4//5x,
     455heigenproblem solution code. . . . . . . . . (ieig)    =,i5 /5x,
     555h   eq.0, no eigenproblem solution                          /5x,
     655h   eq.1, frequencies and mode shapes                       /5x,
     755h         are determined                                    /5x,
     655h   eq.2, nonlinear buckling loads and                      /5x,
     755h         mode shapes are determined                       //5x,
     855hprinting interval . . . . . . . . . . . . . (ipri)    =,i5//5x,
     855hnumber of skew displacement reference                      /5x,
     855h                             systems  . .  (nskew)    =,i5    )
 2016 format (/5x,
     *55hfrequency analysis code . . . . . . . . . . (ifreq)   =,i5 /5x,
     *55h   eq.0, no frequency analysis                             /5x,
     *55h   eq.1, frequency response                               //5x,
     *55htransient analysis code . . . . . . . . . . (itrans)  =,i5 /5x,
     *55h   eq.0, no transient analysis                             /5x,
     *55h   eq.1, trunsient response                               //5x,
     *55hrandom analysis code  . . . . . . . . . . . (irand)   =,i5 /5x,
     *55h   eq.0, no random analysis                                   )
 2020 format (/5x,
     155hnumber of constraint equations  . . . . . . (numce)   =,i5//5x,
     255hnumber of members in any                                   /5x,
     355h   constraint equation  . . . . . . . . . . (mce)     =,i5//5x,
     455hnumber of different unit vectors  . . . . . (numnps)  =,i5//5x,
     *55hstiffness matrix block partitioning code  . (icore )  =,i5 /5x,
     *55h   eq.0, stiffness matrix is placed only in-core           /5x,
     *55h   eq.1, stiffness matrix divided to blocks                /5x,
     *55h         automatically in case of linear analysis          /5x,
     *55h   eq.2, stiffness matrix divided to blocks                /5x,
     *55h         automatically in case of frequencies analysis     /5x,
     *55h   ge.3, stiffness matrix block length = mtot/icore       //5x,
     *55h   lt.0, if sparse format is used, and icore(sparse) =     /5x,
     *55h         -1-icore(profile)                                //5x,
     b55htemperature code  . . . . . . . . . . . . . (itempf)  =,i5 /5x,
     c55h   eq.0, no thermal effects                                /5x,
     d55h   eq.n, temperature-time history is defined               /5x,
     d55h         by load curve  n                                  /5x,
     155hnumber of slave nodes . . . . . . . . . . . (numsln)  =,i5//5x,
     155hnumber of joints  . . . . . . . . . . . . . (njoint)  =,i5    )
 2025 format (/5x,
     *55htemperature gradient code (t-direction) . . (itmgf1)  =,i5 /5x,
     c55h   eq.0, no thermal effects                                /5x,
     d55h   eq.n, temperature-time history is defined               /5x,
     d55h         by load curve  n                                 //5x,
     *55htemperature gradient code (s-direction) . . (itmgf2)  =,i5 /5x,
     c55h   eq.0, no thermal effects                                /5x,
     d55h   eq.n, temperature-time history is defined               /5x,
     d55h         by load curve  n                                 //5x,
     e55hcontact problem code  . . . . . . . . . . . (iconp)   =,i5 /5x,
     f55h   eq.0, no contact problem                                /5x,
     g55h   eq.1, lagrange multiplier method                        /5x,
     h55h   eq.2, penalty function method                          //5x,
     a55hnumber of contact groups  . . . . . . . . . (ncontb)  =,i5//5x,
     b55hcalculating energy       . . . . . . . . . .(ienerf)  =,i5//5x,
     *55hnumber of nodal displacements . . . . . . . (ndisp)   =,i5//5x,
     555hnumber of nodal masses  . . . . . . . . . . (imassn)  =,i5//5x,
     655hnumber of nodal dampers . . . . . . . . . . (idampn)  =,i5//5x,
     *55hnumber modal damping ratios . . . . .   . . (idampm)  =,i5 /5x,
     *55h       corresponding to circular frequencies                  )
 2030 format (//
     155h a n a l y s i s  t y p e                                 //5x,
     255htime dependency code  . . . . . . . . . . . (istat)   =,i5 /5x,
     355h   eq.0, static analysis                                   /5x,
     455h   eq.1, dynamic analysis                                 //5x,
     555hnonlinearity code . . . . . . . . . . . . . (klin)    =,i5 /5x,
     655h   eq.0, linear analysis                                   /5x,
     755h   eq.1, nonlinear analysis                               //5x,
     *55hmodal code  . . . . . . . . . . . . . . . . (modal)   =,i5 /5x,
     *55h   eq.0, direct analysis                                   /5x,
     *55h   eq.1, modal analysis                                       )
 2031 format (///,40h e i g e n v a l u e s   s o l u t i o n,3x,
     125h c o n t r o l   d a t a //5x,
     255h number of eigenmodes to be determined. . . (nfreq)   =,i5 /5x,
     351h cutoff eigenvalues . . . . . . . . . . . . (cofq)=, e11.4 /5x,
     355h rigid body shift . . . . . . . . . . . . . (iffr)    =,i5 /5x,
     455h print option . . . . . . . . . . . . . . . (ifpr)    =,i5 /5x,
     555h   eq.0, no printing                                       /5x,
     655h   eq.1, print                                             /5x,
     755h method solving eigenvalues problems. . . . (numpro)  =,i5 /5x,
     855h   eq.0, determinant method                                /5x,
     955h   eq.1, subspace iteration method                         /5x,
     *55h   eq.2, rqcgm                                             /5x,
     155h number of iteration vectors must be used . (nc)      =,i5 /5x,
     255h   default set to min(2*nfreq,nfreq+8)                     /5x,
     355h   but nc cannot be larger than the number                 /5x,
     455h   of mass degrees of freedom                              /5x,
     555h   if nc.gt.2*nfreq will used lanczos vectors              /5x,
     655h   for subspace iteration method                           /5x,
     755h flag for sturm sequence check. . . . . . . (ifss)    =,i5 /5x,
     855h   eq.0  no check                                          /5x,
     955h   eq.1  check                                             /5x,
     *55h number of subspace iterations permitted. . (nitem)   =,i5 /5x,
     152h convergence tolerance on eigenvalues . . . (rtol) =,e11.4 /5x,
     *55h method of the solution of buckling problem (ibuckl)  =,i5 /5x,
     *55h   eq.0, no nonlinear buckling problem                     /5x,
     *55h   eq.1, eigenmodal problem is solved                      /5x,
     *55h        beetween the time steps *t-dt* and *t*             /5x,
     *55h   eq.2, eigenmodal problem is solved                      /5x,
     *55h        beetween the time steps *t-2dt* and *t*            /5x)
 2032 format (/5x,
     *51h frequency domain interval  . . . . . . . . (ws)  =, e11.4 /5x,
     *51h                                            (we)  =, e11.4 /5x,
     *55h scale frequencies domain code  . . . . . . (isc)     =,i5 /5x,
     *55h   eq.0, logorithm interpolation                           /5x,
     *55h   eq.1, linear interpolation                              /5x,
     *55h units code frequencies domain              (iunit)   =,i5 /5x,
     *55h   eq.0, cycl/sec                                          /5x,
     *55h   eq.1, second                                            /5x,
     *55h   eq.2, rad/sec                                              )
 2035 format (/5x,
     155hnumber of time steps between reforming                     /5x,
     255h  effective stiffness matrix  . . . . . . . (isref)   =,i5//5x,
     355hnumber of step size reduction permitted                    /5x,new
     455h  in each time step . . . . . . . . . . . . (nutmre)  =,i5//5x,new
     555hnumber of time steps between                               /5x,
     655h  equilibrium iterations  . . . . . . . . . (iequit)  =,i5//5x,
     755hmaximum number of equilibrium                              /5x,
     855h  iterations permitted  . . . . . . . . . . (itemax)  =,i5//5x,
     955htype of iteration procedure . . . . . . . . (itetyp)  =,i5//5x,
     a55h   eq.1, modified newton-raphson                           /5x,
     b55h         iteration (m.n.r.i.)                              /5x,
     c55h   eq.2, m.n.r.i. with aitken acceleration                 /5x,
     d55h   eq.3, m.n.r.i. with modified aitken                     /5x,
     e55h         acceleration                                      /5x,
     f55h   eq.4, bfgs quasi-newton method                          /5x,
     *55h   eq.5, newton-raphson iteration                         //5x,
     *55h   eq.6, modified arc-length with m.n.r.i.                //5x,
     *55h   eq.7, modified arc-length method with bfgs             //5x,
     g51hdisplacement convergence tolerance  . . . . (dtol)=,e9.2  //5x,
     h51hforce convergence tolerance . . . . . . . . (ftol)=,e9.2  //5x,
     i51henergy convergence tolerance  . . . . . . . (etol)=,e9.2  //5x,
     *55hload constraint option. . . . . . . . . . . (lcnstr)  =,i5 /5x,new
     *55h   eq.0, no load constraints                               /5x,new
     *55h   eq.1, load constraints used                                )new
 2036 format (/5x,                                                      new
     *55hnodal point number for prescribed                          /5x,new
     *55h  displacement component. . . . . . . . . . (nustar)  =,i5//5x,new
     *55hdegree of freedom for prescribed                           /5x,new
     *55h  displacement component. . . . . . . . . . (iustar)  =,i5//5x,new
     *52hprescribed displacement component value . . (ustar)=,e11.4//5x,new
     *55hparameter *alpha* to bound the norm of                     /5x,new
     *52h  displacement increment. . . . . . . . . . (alph) =,e11.4//5x,new
     *55hparameter *delta* to switch from                           /5x,new
     *55h  the spherical constant arc-length scheme to              /5x,new
     *52h  the constant increment of external work . (delt) =,e11.4//5x,new
     *55hoptimal number of iterations per time step. (iteopt)  =,i5//5x,new
     *55hparameter *psi* of the spherical constant                  /5x,new
     *52h  arc-length equation . . . . . . . . . . . (psi)  =,e11.4//5x,new
     *55hcode to precize critical load . . . . . . . (idetrf)  =,i5 /5x,new
     *55h   eq.0, no time step refine                               /5x,new
     *55h   eq.1, time step refine                                     )new
c 2052 format(
c     3/5x,'nodal point number for displacement control            '
c     4/5x,'  of arc length method  . . . . . . . . . . (idxyz)   =',i5/
c     5/5x,'direction of displacement for controlling              '
c     6/5x,'  node (x=1,y=2,z=3) . . . . . . . . . . . .(idof)    =',i5/
c     7/5x,'desired arc length (if zero determined by pioner       '
c     8/5x,'  based on initial step size)  . . . . . . . (ds) =',e9.2)
c 2054 format(
c     1/5x,'arc length constraint method . . . . . . . .(iarc)    =',i5
c     2/5x,'   eq.0: crisfields                                       '
c     2/5x,'   eq.1: modified riks                                    '
c     3/5x,'   eq.2: padovan                                           '/
c     1/5x,'arc length damping option . . . . . . . . . (idmp)    =',i5
c     2/5x,'   eq.0: no damping                                        '
c     3/5x,'   eq.1: damping applied                                   '/
c     4/5x,'line search tolerence  . . . . . . . . . (tollin) =',e9.2
c     5/5x,'number of step size reductions permitted per step .   =',i7/
c     1/5x,'line search option for resid . . . . . . .(linsch)    =',i5
c     2/5x,'   eq.1: included                                          '
c     3/5x,'   eq.2: not included                                     '/
c     5/5x,'optimal number of iterations per step . . (iteopt)    =',i7)
 2037 format (/5x,
     155hmethod of evaluation of overlap . . . . . . (ioverl)  =,i5//5x,
     255hmethod of evaluation of contact force . . . (icontf)  =,i5//5x,
     355hiteration method code . . . . . . . . . . . (iminit)  =,i5 /5x,
     455h   eq.1, one-level iteration method                        /5x,
     555h   eq.2, two-level iteration method                       //5x,
     655hmaximum number of minor                                    /5x,
     755h  iterations permitted  . . . . . . . . . . (minitm)  =,i5//5x,
     855hmaximum number of iterations                               /5x,
     955h  permitted for c.g. method . . . . . . . . (itcgm)   =,i5//5x,
     a51hoverlap convergence tolerance . . . . . . . (otol)=,e9.2  //5x,
     b51hc.g. method convergence tolerance . . . . .(tolcg)=,e9.2  )
 2040 format (/5x,
     155htime integration code . . . . . . . . . . . (iope)    =,i5 /5x,
     255h   eq.1, wilsons theta method                              /5x,
     355h   eq.2, newmarks method                                   /5x,
     455h   eq.3, central difference method                            )
 2050 format (5x,
     155hintegration parameter . . . . . . . . . . . (theta)   =,f5.2)
 2060 format (5x,
     155hintegration parameters  . . . . . . . . . . (delta)   =,f5.2/5x
     2,55h                                            (alpha)   =,f5.2 )
 2070 format (//
     155h d i s p / v e l / a c c   p r i n t o u t   c o d e      //5x,
     255hnumber of blocks of nodal printout  . . . . . . (npb) =,i5//5x,
     355hdisplacement printout code  . . . . . . . . . . (idc) =,i5 /5x,
     455h   eq.0, no printing of displacements                      /5x,
     555h   eq.1, print displacements                              //5x,
     655hvelocity printout code  . . . . . . . . . . . . (ivc) =,i5 /5x,
     755h   eq.0, no printing of velocities                         /5x,
     855h   eq.1, print velocities                                 //5x,
     955hacceleration printout code  . . . . . . . . . . (iac) =,i5 /5x,
     a55h   eq.0, no printing of accelerations                      /5x,
     b55h   eq.1, print accelerations                                  )
 2080 format (/5x,
     16hblock ,i2                                                  //8x,
     248hfirst node of this block   . . . . .  (ipnode(1,i1,3h) =i5//8x,
     348hlast node of this block  . . . . . .  (ipnode(2,i1,3h) =i5    )
 2090 format (////12h l o a d s  // 5x,
     155h number of load curves . . . . . . . . . . .  (nlcur) =,i5//5x,
     255h max number of points in load curves . . . . . (nptm) =,i5//5x,
     355h number of concentrated loads  . . . . . . .  (nload) =,i5//5x,
     455h number of base motion loads . . . . . . . .  (nbase) =,i5//5x,
     555h load type code  . . . . . . . . . . . . . .  (ltype) =,i5 /5x,
     655h   le.-1, spectrum domain                                  /5x,
     655h   eq.0,  time history domain                              /5x,
     755h   ge.+1, frequency domain                                //5x,
     855h fast fourier transform code . . . . . . . . . (ifft) =,i5 /5x,
     955h   eq.0, fourier transform                                 /5x,
     a55h   eq.1, inverse fourier trunsform                            )
 2092 format (                                                      /5x,
     155h gravity load code . . . . . . . . . . . . . .(igrav) =,i5 /5x,
     255h   eq.0, no gravity loads                                  /5x,
     355h   eq.1, gravity loads                                     /5x,
     455h         are determined                                   //5x,
     555h number of gravity load curves . . . . . . .  (mcurg) =,i5    )
 2093 format (                                                      /5x,
     155h 2/d continuum element face                  (npr2dc/n)    /5x,
     255h         pressure specifications                              )
 2094 format (                                                      /5x,
     155h    (a) number of conservative                             /5x,
     255h                   pressure loads . . . . . . . . . . =,i5 /5x,
     355h    (b) number of nonconservative                          /5x,
     455h                   pressure loads . . . . . . . . . . =,i5    )
 2095 format (                                                      /5x,
     155h 3/d continuum  4-nodal element face      (npr3dc/n(1))    /5x,
     255h         pressure specifications                              )
 2096 format (                                                      /5x,
     155h 3/d continuum  9-nodal element face      (npr3dc/n(2))    /5x,
     255h         pressure specifications                              )
 2097 format (                                                      /5x,
     155h 3/d continuum 16-nodal element face      (npr3dc/n(3))    /5x,
     255h         pressure specifications                              )
 2098 format (                                                      /5x,
     155h shell lateral surface                                     /5x,
     255h         load specifications                                  )
 2099 format (                                                      /5x,
     155h shell face surface                                        /5x,
     255h         pressure specifications                              )
 2091 format (                                                      /5x,
     155h beam  face surface                                        /5x,
     255h         pressure specifications                              )
 2100 format (////51h r a y l e i g h   d a m p i n g   c o e f f i c i
     1        10he n t s   ///
     2        40h     alpha . . . . . . . . . (adamp)   =,e13.4/
     3        40h     beta  . . . . . . . . . (bdamp)   =,e13.4//)
c
 3000 format (//46h **stop - if rayleigh damping or concentrated ,
     1         43hmasses/dampers, tnen imass cannot equal 0      )
 3010 format (//42h **stop - ieig.gt.1 not permitted in this
     1        15hversion of dysa  )
 3020 format (//41h **stop - number of printout blocks .gt.8 ,
     1        2x,4hnpb=,i5)
 3030 format (//52h **stop - if iope.eq.3,then imass must be equal to 1)
 3040 format (//52h **stop - if iope.eq.3,then idamp must be equal to 0)
 3042 format (//45h **stop - if gravity loads exist then *mcurg*
     1          28h must be gt.0  and  le.nlcur  )
 3050 format (//47h **stop - static problem has not external loads )
 3060 format (//48h **stop - no rayleigh damping coefficients input)
 3001 format (//51h **stop - if idamp.eq.2, then idampm must be .ge. 1)
c
      end
