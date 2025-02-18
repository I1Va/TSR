.model tiny
.186
locals @@

.code
org 100h

start:
                xor     ax, ax
                mov     es, ax
                mov     bx, 09h * 4

                mov     ax, es:[bx]
                mov     old09fs, ax

                mov     ax, es:[bx + 2]
                mov     old09seg, ax

                cli

                mov     word ptr es:[bx], offset MyINT09
                push    cs
                pop     ax
                mov     es:[bx + 2], ax

                sti

                int     09h

                mov     ax, 3100h
                mov     dx, offset TSR_END
                shr     dx, 4
                inc     dx
                int     21h

MyINT09         proc
                push    ax bx cx dx ds si es di

                mov     al, cs:INT09_TOGGLE
                call debug_draw_char

                in      al, 60h
                cmp     al, POP_KEY
                je      do_pop
                jmp     ChainOldISR


do_pop:

                in      al, 61h
                mov     ah, al
                or      al, 80h
                out     61h, al
                xchg    ah, al
                out     61h, al

                xor     ax, ax
                mov     al, INT09_TOGGLE
                neg     al
                mov     cs:INT09_TOGGLE, al
                cmp     al, 1d
                je      ChainOldISR

                push    cs
                pop     ds
                mov     bx, VIDEOSEG
                mov     es, bx

                mov     ah, 4eh
                mov     si, offset RECT_STYLE
                mov     bx, 9
                mov     cx, 9

                mov     di, CONSOLE_WIDTH               ;|
                sub     di, cx                          ;| di = (CONSOLE_WIDTH - cx - 1) * 2
                add     di, CONSOLE_WIDTH
                shl     di, 1                           ;| left upper rect corner addr

                push di
                call draw_rect
                pop di

                add     di, 2
                add     di, CONSOLE_WIDTH * 2

                mov     si, offset AX_REG_EQU
                push di
                call draw_string
                pop di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset BX_REG_EQU
                push di
                call draw_string
                pop di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset CX_REG_EQU
                push di
                call draw_string
                pop di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset DX_REG_EQU
                push di
                call draw_string
                pop di



                jmp     ChainOldISR

                iret
                endp

ChainOldISR:
                pop     di es si ds dx cx bx ax         ; restore prev registers
                db      0eah                            ; 0eah = jmp
old09fs         dw      0                               ; old ISR address
old09seg        dw      0                               ; old ISR segment



;##########################################
;               draw_string
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws a string by addr ES:DI untill
;       END_TEXT_CHAR
; Entry:
;       AH      ; color attr
;       DS:SI   ; string memory addr
;       ES:DI   ; line beginning addr
; Desroy:
;       AL, BX, CX, SI, DI
;------------------------------------------
draw_string     proc
                cld                                     ; DF = 0 (++)
@@while:;-----------------------------------------------; while (CX != 0) {
                lodsb                                   ;       al = ds:[si++]

                cmp     al, TEXT_END_CHAR               ;|      if al == TEXT_END_CHAR: jmp end
                je      @@end                           ;|

                stosw                                   ;|      es:[di+=2] = ax                             ;       es:[di++] = ax
                jmp @@while
;-------------------------------------------------------; while end }

@@end:
                ret
                endp
;------------------------------------------
;##########################################



;//----------DEBUG_FUNCTIONS. SLOW. SIMPLE. SAFE-------------//

;##########################################
;               debug_draw_char
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws a char in videoseg by addr 0
; Entry:
;       AL      ; char ascii code
; Desroy:
;       None
;------------------------------------------
debug_draw_char    proc

                push ax bx es di

                mov     ah, 11001111b
                mov     bx, VIDEOSEG
                mov     es, bx
                mov     di, 0d
                mov     es:[di], ax
                pop di es bx ax

                ret
                endp
;------------------------------------------


;//----------DEBUG_FUNCTIONS. SLOW. SIMPLE. SAFE-------------//



;##########################################
;               draw_pat_line
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws a line by pattern
; Entry:
;       AH      ; color attr
;       DS:SI   ; line pattern addr
;       CX      ; line length
;       ES:DI   ; line beginng addr
; Desroy:
;       AL, BX, CX, SI, DI
;------------------------------------------
draw_pat_line   proc
                cld                                     ; DF = 0 (++)

                lodsb                                   ; al = ds:[si++]
                stosw                                   ; es:[di++] = ax
                sub cx, 2d                              ; cx -= 2 for first, last char

                lodsb                                   ; al = ds:[si++]

                rep stosw                               ; while (CX != 0) {es:[di+=2] = ax}

                lodsb                                   ; al = ds:[si++]
                stosw                                   ; es:[di+=2] = al

                ret
                endp
;------------------------------------------
;##########################################

;##########################################
;               draw_rect
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws rectangle by pattern
; Entry:
;       AH      - color attr
;       DS:SI   - addr of pattern
;       BX      - rectangle height
;       CX      - rectangle width
;       ES:DI   - rectangle upper left corner
; Destr:
;       AX, SI, DI
;------------------------------------------
draw_rect       proc
                push bx                                 ;|
                push cx                                 ;|reg saving
                push di                                 ;|

                call draw_pat_line                      ; call draw_pat_line

                pop di                                  ;|
                pop cx                                  ;|reg restoring
                pop bx                                  ;|

                add di, 160                             ;|next line
                sub bx, 2                               ;|

@@while:;-----------------------------------------------; while (BX > 0) {
                push bx                                 ;|
                push cx                                 ;|      reg saving
                push di                                 ;|

                push si                                 ;       save pattern middle triad addr

                call draw_pat_line                      ;       call draw_pat_line

                pop si                                  ;       restore patterm middle triad addr

                pop di                                  ;|
                pop cx                                  ;|      reg restoring
                pop bx                                  ;|

                add di, 160                             ;|      next line
                dec bx                                  ;|
                cmp bx, 0                               ;       if dx > 0 -> jump @@while
jg @@while;---------------------------------------------; while end }

                add  si, 3d
                push bx                                 ;|
                push cx                                 ;|reg saving
                push di                                 ;|

                call draw_pat_line                      ; call draw_pat_line

                pop di                                  ;|
                pop cx                                  ;|reg restoring
                pop bx                                  ;|

                ret
                endp
;------------------------------------------
;##########################################


.data
VIDEOSEG                equ 0b800h
POP_KEY                 equ 22h             ; 'G'
TEXT_END_CHAR           equ '%'
CONSOLE_WIDTH           equ 80d
CONSOLE_HEIGH           equ 25d
RECT_STYLE              db "+=+|.|+=+$"

AX_REG_EQU              db "AX=%$"
BX_REG_EQU              db "BX=%$"
CX_REG_EQU              db "CX=%$"
DX_REG_EQU              db "DX=%$"

INT09_TOGGLE            db -1d

TSR_END:

end start

