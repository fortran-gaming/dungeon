
! TXCRYP - Subroutine to encrypt/decrypt text strings.
!
! This subroutine performs a reversible encryption on a text string.
! The purpose is not to protect the data base but to make it more
! difficult for the casual game user to read the data base file.
! It is located here, rather than in the SUBRoutine module, because
! it is used by both the game and the separate data base compiler.
!
elemental SUBROUTINE TXCRYP(R,LINE)
IMPLICIT None
integer, intent(in) :: r
CHARACTER(*), intent(inout) :: LINE
integer x,i

DO I=1,LEN(LINE)
  X=IAND(R, 31)+I
  LINE(I:I)=CHAR(IEOR(ICHAR(LINE(I:I)), X))
enddo

END subroutine
