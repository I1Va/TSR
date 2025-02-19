.model tiny
.186
locals @@

.code
org 100h

start:
                xor     ax, ax
                xor     bx, bx
                xor     cx, cx
                xor     dx, dx
@@while:
                inc     ax;
                jmp     @@while

end start

