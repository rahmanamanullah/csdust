        program scattercs_color
        parameter (pi=3.141593)
        parameter (radius=1000.,cosmax=1.0,cosmin=-1.0)
        parameter (intup=14,nmax=1000,nwavel=13)
        parameter(NWPAW=500000,nphot=5000,tmax=5000.,nmaxhit=50000)
        character*40 a
        character*70 fname,header
        character*7 tags(intup)
        logical clathrate,geometric
        data tags/ 'x      ','y      ','z      ','wavel  ','albedo ',
     +             'scadust','absdust','tau    ','time   ','thetatr',
     +             'phitr  ','scat_n ','filter ','ebv_s  '     /
        dimension vtup(intup)
        integer argc,iargc,i
        real albedo(nwavel),wavel(nwavel),cost(nwavel)
        real    C_ext(nwavel),abs_K(nwavel),cos2t(nwavel)
        character*10 comment(nwavel)
        real al_B,al_V,a_B,a_V
        COMMON / QUEST / IQUEST(100)
        common/PAWC/BUF(NWPAW)
        real sum_cs(nwavel),sum_abs(nwavel) 
        real abs_x_is(nwavel),abs_x_cs(nwavel)
        real scatdust(nwavel),absdust(nwavel),avt(nwavel)
        integer nhit(nwavel)
        logical dopaw
        logical MW
        data dopaw/.false./
*********************************************************
* Study of scattering in circumstellar medium
*********************************************************
c Open extract of file from 
c ftp://ftp.astro.princeton.edu/draine/dust/mix/
      MW = .false.
      fname = 'UBVRIJHK_LMC.dat' 
      open(unit = 1,file=fname, status='old')
      open(unit = 2,file='color_test_LMC_CS.out', 
     + status='unknown')
      open(unit = 3,file='scattering_test_LMC_CS.out', 
     + status='unknown')
      open(unit = 4,file='color_test_LMC_IS.out', 
     + status='unknown')

c Open extract of file from 
c ftp://ftp.astro.princeton.edu/draine/dust/mix/
c      MW = .false.
c      fname = 'UBVRIJHK_SMC.dat' 
c      open(unit = 1,file=fname, form='formatted',status='old',
c     +   access='direct')
c      open(unit = 2,file='color_test_SMC_CS.out', 
c     + form='formatted',status='unknown', access='direct')
c      open(unit = 3,file='scattering_test_SMC_CS.out', 
c     + form='formatted',status='unknown',access='direct')
c      open(unit = 4,file='color_test_SMC_IS.out', 
c     + form='formatted',status='unknown',access='direct')



C      MW = .true.
C      fname = 'UBVRIJHK_MW_3.1.dat' 
C      open(unit = 1,file=fname, form='formatted',status='old',
C     +   access='direct')
C      open(unit = 2,file='color_test_MW_CS.out', 
C     + form='formatted',status='unknown', access='direct')
C      open(unit = 3,file='scattering_test_MW_CS.out', 
C     + form='formatted',status='unknown',access='direct')
C      open(unit = 4,file='color_test_MW_IS.out', 
C     + form='formatted',status='unknown',access='direct')

       
      if (MW) then 
      do i=1,80
       read(1,'(a)') header
C       print *,header
      enddo  
      do i=1,nwavel

       read(1,'(a70)') header
c       print *,header
       read(header,200) wavel(i),albedo(i),cost(i),C_ext(i),
     +             abs_K(i),cos2t(i),comment(i)
c       write(6,200) wavel(i),albedo(i),cost(i),C_ext(i),
c     +             abs_K(i),cos2t(i),comment(i)
      enddo
      close(1)
      else

      do i=1,43
       read(1,'(a)') header
       print *,header
      enddo  
      do i=1,nwavel

       read(1,'(a70)') header
c       print *,header
       read(header,201) wavel(i),albedo(i),cost(i),C_ext(i),
     +             abs_K(i)
       write(6,201) wavel(i),albedo(i),cost(i),C_ext(i),
     +             abs_K(i)
      enddo
      close(1)
      endif 

c  2.19000E+00 0.4391  0.1310 5.566E-23 1.670E+03 0.40176 K filter
 200  format(E12.5,F7.4,F8.4,2E10.3,1X,F7.5,a10)
 201  format(E10.5,F7.4,F8.4,2E10.3)

       IF (dopaw) then
C BOOK NTUPLE
       call hlimit(nwpaw)
       IQUEST(10) = 64000 !Big File ?
       Call Hropen(60,'cs','/data/ariel/cs.hbook','NQ',1024,Istat)
       print *,'Book ntuples'
       call hbookn(1,'cs scattered photons',intup,'cs',1000,tags)
       call hbookn(2,'unscattered  photons',intup,'cs',1000,tags)
       endif
C
C INITILAIZE RANDOM NUMBER GENERATION
*
               
       call rnclck ( iseed )
       call rninit ( iseed )
*                
        geometric = .false.
       write(2,'(a)'),'Summary of colors '   
       write(2,'(a)'),'------------------'
       write(2,'(14A6)') 'EBV','A_u','A_U','A_B','A_g','A_V',
     +  'A_r','A_R','A_i','A_I','A_z',
     +          'A_J','A_H','A_K' 
       write(2,'(a84)') '- - - - - - - - - - - - - - - - - - - - - - - '
       write(4,'(a)'),'Summary of colors '   
       write(4,'(a)'),'------------------'
       write(4,'(14A6)') 'EBV','A_u','A_U','A_B','A_g','A_V',
     +  'A_r','A_R','A_i','A_I','A_z',
     +          'A_J','A_H','A_K' 
       write(4,'(a84)') '- - - - - - - - - - - - - - - - - - - - - - - '
       write(3,'(7A8)') 'lambda','tau_s','tau_a','albedo',
     +              'costhe','time','EBV'
       write(3,'(a56)') '- - - - - - - - - - - - - - - - - - - - - - - '
C- adjust scattering length so so that K-band has ndust scattering lengths
C;       scatconst = (radius/xscat)*abs_K(1)/(1./albedo(1)-1.)
C- Adjust scattering length to given value of E(B-V)
C- B index 11; V index 9
       al_B = albedo(11)
       al_V = albedo(9)
       a_B   = abs_K(11)
       a_V   = abs_K(9)
       factor = a_B/(1.- al_B) - a_V/(1. - al_V)
       do 333 iebv=4,4
       ebv = float(iebv)/18. 
       scatconst = ebv/1.085736/radius/factor
       do 2000 jj=1,nwavel  ! loop over UBVRIJHKugriz
        sum_cs(jj)  = 0.
        sum_abs(jj) = 0.
        avt(jj) = 0.
        tau_dust=cost(jj)        
        scatdust(jj) = 1./abs_K(jj)*(1./albedo(jj)-1.)/scatconst
        smin = scatdust(jj)/10000.
        absdust(jj)  = 1./abs_K(jj)/scatconst
c        print *,wavel(jj),scatdust,absdust
        nhit(jj) = 0
        do 1000 iphot=1,nphot
c- Start from (0,0,0)           
          x        = 0.
          y        = 0.
          z        = 0.
          path     = 0.
c-  generate a length to next scatter with dust
c          sdust    = rnexp(scatdust(jj))
c          if (sdust.lt.smin) sdust=smin
c          astop    = rnexp(absdust(jj)) !absorption
c          print *,ebv,wavel(jj),sdust,astop,radius
          astop    = rnexp(absdust(jj)) !absorption
          if (astop .le. radius) goto 990
          do 100 istep=1,nmax
          sdust    = rnexp(scatdust(jj))
          if (sdust.lt.smin) sdust=smin
          if (istep.eq.1) then 
             costheta = (cosmax-cosmin)*rnunif(dummy)+cosmin
             theta    = acos(costheta)
             sintheta = sin(theta)
             cos0     = costheta
             if (sdust.ge. radius .and. astop.ge.radius) then
c     if (sdust.ge. radius ) then
                phi      = 2.*pi*rnunif(dummy)
                cosphi   = cos(phi)
                sinphi   = sin(phi)
                vtup(1) = radius*sintheta*cosphi
                vtup(2) = radius*sintheta*sinphi
                vtup(3) = radius*costheta
                vtup(4) = wavel(jj)
                vtup(5) = albedo(jj)
                vtup(6) = scatdust(jj)
                vtup(7) = absdust(jj)
                vtup(8) = tau_dust
                vtup(9) = 1.
                vtup(10)= theta
                vtup(11)= phi
                vtup(12)= 0.
                vtup(13)=float(jj)
                vtup(14)=ebv
c     print *,'Reached radius'
                sum_abs(jj) = sum_abs(jj) + 1.
                if (dopaw) call hfn(2,vtup)                          
             endif
          else
 2           continue
C     - generate a scattering angle with dust
             theta = acos(CTHENYEY_GREENSTEIN(tau_dust))
C     - Now we have a scattering angle in the coordinate system of the 
C     - incoming photon
             costheta = cos(theta)
             sintheta = sin(theta)
          endif
C-generate an azimuthal (isotropic) angle 
          phi      = 2.*pi*rnunif(dummy)
          cosphi   = cos(phi)
          sinphi   = sin(phi)
          step=sdust

c            print *,'path=',path
* photon absorbed ?          
             if (path.ge.astop) then
c              print *,'Photon absorbed before reaching radius' 
c              print *,path,step,astop,sdust,albedo(jj)
              goto 990
             endif
             path = path + step
             if (step .lt. smin) step=smin
c-- This are the coordinate changes with respect to the TRACK
             dxtr    = step*sintheta*cosphi
             dytr    = step*sintheta*sinphi
             dztr    = step*costheta
            if (istep.gt.1) then
c-- axis of track in detector coordinate system
             IF (dx.eq.0. .and. dy.eq.0.) then 
              phitr = 0.
             ELSE
              phitr  = atan2(dy,dx)
             ENDIF
             cthetr = dz/sqrt(dx**2+dy**2+dz**2)
             thetr = acos(cthetr)
c-- and these are in the coordinate system of the DETECTOR
             call TRANSPOSElbe(thetr,phitr,dxtr,dytr,dztr,dx,dy,dz)
             rnorm=sqrt((dx**2+dy**2+dz**2)/(dxtr**2+dytr**2+dztr**2))
             if (abs(rnorm-1.).gt.0.0001) then 
              print *,'Matrix normalization OFF: ',rnorm
             endif 
            else
             dx = dxtr
             dy = dytr
             dz = dztr  
            endif  
c* check if track crosses a detector sphere within step           
           rnow  = sqrt(x**2+y**2+z**2)
           rnext = sqrt((x+dx)**2+(y+dy)**2+(z+dz)**2)
           zdet    = radius
            idet = 0 
            if (zdet.ge.rnow .and. zdet.le.rnext) then 
C cross the detector OUTWARDS within the next step 
             t =  (zdet-rnow)/step
             idet = 1
            else
             x = x + dx
             y = y + dy
             z = z + dz
            endif
            range = step*t+path
            time  = range/radius 
            avt(jj) = avt(jj) + time
            if (idet.eq.1) then
* crossing sphere from the inside out or the othe way around !
             xcross = x + dx*t
             ycross = y + dy*t
             zcross = z + dz*t
             nhit(jj) = nhit(jj) + 1
             if (nhit(jj) .GT. nphot) then 
              print *,'Warning double counting!:',nhit(jj)
             endif
             geodis = sqrt(xcross**2+ycross**2+zcross**2)
             vtup(1) = xcross
             vtup(2) = ycross
             vtup(3) = zcross
             vtup(4) = wavel(jj)
             vtup(5) = albedo(jj)
             vtup(6) = scatdust(jj)
             vtup(7) = absdust(jj)
             vtup(8) = tau_dust
             vtup(9) = time
             vtup(10)= thetr
             vtup(11)= phitr
             vtup(12)= float(istep)   
             vtup(13)= float(jj)
             vtup(14)= ebv
             sum_cs(jj) = sum_cs(jj) + 1.
c             print *,'Photon counted in colors'
             if (dopaw) call hfn(1,vtup)
             goto 990 ! next photon please!
            endif 
  100        end do !loop over steps
  990      continue
c           print *,'Next Photon'
c           read(5,*) a
 1000     end do ! loop over photons
 2000    end do !loop over wavlength  
 222     continue 

       ebv_cs =   -2.5*alog10(sum_cs(11)/float(nphot)) + 
     +             2.5*alog10(sum_cs(9)/float(nphot)) 
       ebv_is =   -2.5*alog10(sum_abs(11)/float(nphot)) + 
     +             2.5*alog10(sum_abs(9)/float(nphot))  
       do i=nwavel,1,-1
       abs_x_is(i) = -2.5*alog10(sum_abs(i)/float(nphot))  
       abs_x_cs(i) = -2.5*alog10(sum_cs(i)/float(nphot))
       avt(i)      = avt(i)/sum_cs(i)
       write(3,'(7F10.4)') wavel(i),scatdust(i)/radius,
     +             absdust(i)/radius,
     +             albedo(i),cost(i),avt(i),ebv_cs 
       enddo 
 33    format(5F8.3,1X,A10)

        write(2,'(14F6.3)') ebv_cs,(abs_x_cs(k),k=nwavel,1,-1)
        write(4,'(14F6.3)') ebv_is,(abs_x_is(k),k=nwavel,1,-1)
 333   continue
       close(2)
       close(3)
       close(4)
       if (dopaw) then
        CALL HROUT(0,ICYCLE,' ')
        CALL HREND('cs')
       endif
      end
c       SUBROUTINE TRANSPOSE(THETA,PHI,X1,Y1,Z1,X2,Y2,Z2)
c       cphi=cos(phi)
c       sphi=sin(phi)
c       cthe=cos(theta)
c       sthe=sin(theta)  
c       x2 =  x1*cphi*cthe +y1*sphi*cthe -z1*sthe
c       y2 = -x1*sphi      +y1*cphi 
c       z2 =  x1*sthe*cphi +y1*sthe*sphi +z1*cthe
c       END

        SUBROUTINE TRANSPOSE(THETA,PHI,X1,Y1,Z1,X2,Y2,Z2)
* It is actually the inverse or transponate of the matrix above !!
        cphi=cos(phi)
        sphi=sin(phi)
        cthe=cos(theta)
        sthe=sin(theta)  
        x2 =   x1*cphi*cthe -y1*sphi   +z1*sthe*cphi
        y2 =   x1*cthe*sphi +y1*cphi   +z1*sthe*sphi
        z2 =  -x1*sthe                 +z1*cthe
        END

       subroutine transposelbe(theta,phi,x1,y1,z1,x2,y2,z2)
       cphi=cos(phi)
       sphi=sin(phi)
       sthe=sin(theta)
       cthe=cos(theta)
       x2=(cphi**2*cthe+sphi**2)*x1+(cphi*sphi*cthe-cphi*sphi)*y1
       x2=x2+sthe*cphi*z1
       y2=(sphi*cphi*cthe-cphi*sphi)*x1
       y2=y2+(sphi**2*cthe+cphi**2)*y1+sphi*sthe*z1
       z2=-sthe*cphi*x1-sthe*sphi*y1+cthe*z1
       return
       end
            

                

      FUNCTION CTHENYEY_GREENSTEIN(TAU)
************************************************
* formula taken from manuscript for applied optics
*
**************************************************
        r  = rnunif(dummy)
        rt = r*tau
        ct=(1.-tau+rt)*r*(1.+tau)**2/(1.-tau+2.*rt)**2
        ct=2.*ct-1. 
        IF (abs(ct).gt.1) THEN 
         print *,'Warning !! ct=',ct 
         if (ct .gt. 1.) then
          ct = 1.
         else
          if (ct .lt. -1.) ct = -1.
         endif
       ENDIF
       CTHENYEY_GREENSTEIN = ct
      END

      FUNCTION RNEXP(BETA)
      rn    = rnunif(dummy)
      rnexp = -beta*alog(rn)
      END

      FUNCTION ATTICE(WAVEL)
      REAL wavel
* Function due to Lars Bergstroem, output in m^-1 input in nm
      attice = 1./(8100.*exp(-6700./wavel))
      end  
