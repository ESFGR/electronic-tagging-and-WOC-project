#Function DENSATP(S,T,P)

#Equation of State is from Millero & Poisson (1981) DSR V28: 625-629.

#INPUT:   Salinity (S) in g/kg or pss.
#         Temperature (T) in degrees C.
#         Pressure (P) in decibar. --> 10.13 decibar at sea level

#OUTPUT:       Density in g/cc:


 densatp<-function(S, t2, P){
  
  #Change Temperature units from ºK to ºC
   
   t = t2-273.15
   
  #DEFINE CONSTANTS FOR EQUATION OF STATE
  
  R0 = 999.842594
  R1 = 0.06793952
  R2 = -0.00909529
  R3 = 0.0001001685
  R4 = -0.000001120083
  R5 = 0.000000006536332
  
  A0 = 0.824493
  A1 = -0.0040899
  A2 = 0.000076438
  A3 = -0.00000082467
  A4 = 0.0000000053875
  
  B0 = -0.00572466
  B1 = 0.00010227
  B2 = -0.0000016546
  
  C = 0.00048314
  
  #CALCULATE RHO

  SR = sqrt(S)

  RHO0 = R0 + t * (R1 + t * (R2 + t * (R3 + t * (R4 + t * R5))))
  A = A0 + t * (A1 + t * (A2 + t * (A3 + t * A4)))
  B = B0 + t * (B1 + t * B2)

  RHO = RHO0 + S * (A + B * SR + C * S)

  #DEFINE CONStANtS FOR SECANt BULK MODULUS
  
  D0 = 19652.21
  D1 = 148.4206
  D2 = -2.327105
  D3 = 0.01360477
  D4 = -0.00005155288
  
  E0 = 54.6746
  E1 = -0.603459
  E2 = 0.0109987
  E3 = -0.00006167
  
  F0 = 0.07944
  F1 = 0.016483
  F2 = -0.00053009
  
  G0 = 3.239908
  G1 = 0.00143713
  G2 = 0.000116092
  G3 = -0.000000577905
  
  H0 = 0.0022838
  H1 = -0.000010981
  H2 = -0.0000016078
  H3 = 0.000191075
  
  I0 = 0.0000850935
  I1 = -0.00000612293
  I2 = 0.000000052787
  
  J0 = -0.00000099348
  J1 = 0.000000020816
  J2 = 0.00000000091697
  
  #CORRECT P IN DECI-BARS TO BARS

  Pc = P / 10

  #CALCULATE K
  
  H = H0 + t * (H1 + t * H2) + H3 * SR
  J = J0 + t * (J1 + t * J2)
  
  K1 = D0 + t * (D1 + t * (D2 + t * (D3 + t * D4)))
  K2 = E0 + t * (E1 + t * (E2 + t * E3))
  K3 = F0 + t * (F1 + t * F2)
  K4 = G0 + t * (G1 + t * (G2 + t * G3)) + S * H
  K5 = I0 + t * (I1 + t * I2) + S * J
  
  K = K1 + S * (K2 + SR * K3) + Pc * (K4 + Pc * K5)
  
  #CORRECt FOR PRESSURE

  RHP = RHO * (1 / (1 - Pc / K))

 #CONVERT KG/M3 TO g/cc
  
  DENSATP = RHP / 1000
  
  return(DENSATP)
  
  
} #end function

