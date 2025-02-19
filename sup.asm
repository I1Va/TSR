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

                mov     ax, 0AAAAh
                mov     bx, 0BBBBh
                mov     cx, 0CCCCh
                mov     dx, 0d

cycle:
                inc     dx
                jmp cycle

                mov ax, 4c00h
                int 21h


end start
