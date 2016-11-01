C     KUMMERSM computes the natural logarithm of the real part of
C     Kummer's M with parameters A and B evaluated at Z (all complex)
C     and returns it in the RESULT argument.  *RE and *IM are the real
C     and imaginary parts of *, and N is the number of elements in Z and
C     RESULT.  A and B are of length 1.

      SUBROUTINE KUMMERSM(ARE, AIM, BRE, BIM, ZRE, ZIM, RESULT, N)

      INTEGER N
      DOUBLE PRECISION ARE, AIM, BRE, BIM
      DOUBLE PRECISION ZRE(N), ZIM(N), RESULT(N)
      DOUBLE COMPLEX A, B, Z, CONHYP, VALUE

      A = DCMPLX(ARE, AIM)
      B = DCMPLX(BRE, BIM)
      DO I = 1, N
         Z = DCMPLX(ZRE(I), ZIM(I))
         VALUE = CONHYP(A, B, Z, 1, 0)
         RESULT(I) = DREAL(VALUE)
      ENDDO
      RETURN
      END
