.model tiny
.186
locals @@

.data
VIDEOSEG        equ 0b800h
COUNTER         dw  2006h
POP_KEY         equ 22h                                     ; G scancode
msg_string      db 'EbaniyTSRBLYAT', '%', '$'
TEXT_END_CHAR   equ '%'

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


                mov     ah, 4eh
                push    cs
                pop     ds
                mov     si, offset msg_string

                mov     bx, VIDEOSEG
                mov     es, bx
                mov     di, 5 * 80 * 2 + 15 * 2

                mov     cx, ds
                call    draw_string
                jmp     ChainOldISR


TSR_ERROR:
                mov     ah, 11001001b
                push    cs
                pop     ds

                mov     bx, VIDEOSEG
                mov     es, bx
                mov     di, 0
                mov     al, 'E'

                mov     es:[di], ax

                jmp ChainOldISR

                iret
                endp

ChainOldISR:
                pop     di es si ds dx cx bx ax         ; restore prev registers
                db      0eah                            ; 0eah = jmp
old09fs         dw      0                               ; old ISR address
old09seg        dw      0                               ; old ISR segment



;##########################################
;               TSR_draw_string
;------------------------------------------
; WARNING: DS CHANGES INTO FUNCTION BODY
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

                cld

                mov     ax, ds
                cmp     ax, cx
                jne     TSR_ERROR

                mov     al, ds:[si]
                mov     es:[di], ax

                inc     si
                add     di, 2d

                mov     al, ds:[si]
                mov     es:[di], ax

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
;       Draws a char by addr ES:DI
; Entry:
;       AH      ; color attr
;       AL      ; char ascii code
;       ES:DI   ; char output addr
; Desroy:
;       None
;------------------------------------------
debug_draw_char    proc

                mov     es:[di], ax
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
;       AX, SI
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



;##########################################
;               print_msg
;------------------------------------------
;------------------------------------------
; Print string,
;   placed in msg_string asm variable
; Entry:
; Exit: None
; Destr: None
;------------------------------------------
print_msg       proc
                push    ax dx ds
                push    cs
                pop     ds
                mov     ah, 09h                             ;
                mov     dx, offset msg_string               ; dx = &msg_string
                int     21h                                 ; print(dx)
                pop     ds dx ax
                ret
                endp
;------------------------------------------
;##########################################

TSR_END:

end start
