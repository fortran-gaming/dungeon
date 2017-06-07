program dec

character(1000) :: fn='../dtext'
integer u,i,ind
integer, parameter :: imax=huge(0)
character(80) line

CALL RSPSUB(792,12)
call rspeak(726)


if (.false.) then
    open(u,file=fn,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=80)

    read(u,REC=1) line

    do concurrent (i=1:imax)
        call txcryp(i,line)
        ind = index(line,'ea')

        if (ind==0) cycle
            print *,'hi'
       print *,'i: ',i,'ind: ',ind
       print *,line
    enddo

    close(u)
endif

contains

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
end program
