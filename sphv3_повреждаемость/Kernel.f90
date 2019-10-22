MODULE kernel
 
   IMPLICIT NONE
    CONTAINS
    
    function Compute_W(xi,xj,hi,hj)
        real*8::xi(2)
        real*8::xj(2)
        real*8::hi
        real*8::hj
        real*8::Compute_W
        real*8::r(2)
        real*8::q
        real*8::C
        real*8::KERi
        real*8::KERj
        
        KERi=0.0d0
        KERj=0.0d0
        
        r=xi-xj
        q=sqrt(r(1)*r(1)+r(2)*r(2))/hi
        C=1.0/(3.14159265358979323846*hi*hi)

        if((q>=0)*(q<=1)) then
               KERi=C*(10.0d0 / 7.0d0)*(1.0d0-3.0d0/2.0d0*q*q*(1.0d0-q/2.0d0))
        end if
    
        if ((q > 1) * (q <=2)) then
            KERi = C*(10.0d0 / 7.0d0)*(1.0d0/4.0d0)*(2.0d0 - q)*(2.0d0 - q)*(2.0d0 - q)
        end if
        
        q=sqrt(r(1)*r(1)+r(2)*r(2))/hj
        C=1.0d0/(3.14159265358979323846*hj*hj)

        if((q>=0)*(q<=1)) then
               KERj=C*(10.0d0 / 7.0d0)*(1.0d0-3.0d0/2.0d0*q*q*(1.0d0-q/2.0d0))
        end if
    
        if ((q > 1) * (q <=2)) then
            KERj = C*(10.0d0 / 7.0d0)*(1.0d0/4.0d0)*(2.0d0 - q)*(2.0d0 - q)*(2.0d0 - q)
        end if
        
        
    
    Compute_W=(KERi+KERj)/2.0d0
 end function Compute_W
        
END MODULE kernel