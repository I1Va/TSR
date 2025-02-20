.model tiny
.186
locals @@

.code
org 100h

start:

                xor     ax, ax                          ;|
                mov     es, ax                          ;|
                mov     bx, 09h * 4                     ;|
                mov     ax, es:[bx]                     ;| save old INT09 addr
                mov     old09fs, ax                     ;|
                mov     ax, es:[bx + 2]                 ;|
                mov     old09seg, ax                    ;|

                xor     ax, ax                          ;|
                mov     es, ax                          ;|
                mov     bx, 08h * 4                     ;|
                mov     ax, es:[bx]                     ;| save old INT09 addr
                mov     old08fs, ax                     ;|
                mov     ax, es:[bx + 2]                 ;|
                mov     old08seg, ax                    ;|


                cli                                     ; stop interrupt processing

                mov     bx, 09h * 4                     ;|
                mov     word ptr es:[bx], offset MyINT09;|
                push    cs                              ;| INT09 address replacing
                pop     ax                              ;|
                mov     es:[bx + 2], ax                 ;|

                mov     bx, 08h * 4                     ;|
                mov     word ptr es:[bx], offset MyINT08;|
                push    cs                              ;| INT08 address replacing
                pop     ax                              ;|
                mov     es:[bx + 2], ax                 ;|

                sti                                     ; renewal interrupt processing


                mov     ax, 3100h                       ;| Exit with Staing Resident
                mov     dx, offset TSR_END              ;| dx - count of pages, occupied by TSR program
                shr     dx, 4                           ;|
                inc     dx                              ;|
                int     21h                             ;|


ToggleTable     proc
                push    ax

@@toggle:
                mov     ax, cs:[REG_TABLE_TOGGLE_DELAY]
                mov     cs:[REG_TABLE_TOGGLE_TIMER], ax

                mov     al, cs:[REG_TABLE_TOGGLE]
                cmp     al, 00h
                je      @@set_on
                jne     @@set_off

@@set_on:
                mov     al, 01h
                jmp     @@end
@@set_off:
                mov     al, 00h

                mov     word ptr cs:[DEBUG_VAL_3], 0AAAAh
                call    load_tablet_bgr
                jmp     @@end
@@end:
                mov     cs:[REG_TABLE_TOGGLE], al
@@return:
                pop     ax
                ret
                endp





MyINT09         proc
                push    ax bx cx dx ds si es di

                in      al, 60h
                cmp     al, POP_KEY
                je      do_pop
                jmp     JmpOldINT09
do_pop:

                call ToggleTable


                in      al, 61h
                mov     ah, al
                or      al, 80h
                out     61h, al
                xchg    ah, al
                out     61h, al

                jmp     JmpOldINT09

                iret
                endp

JmpOldINT09:
                pop     di es si ds dx cx bx ax         ; restore prev registers
                db      0eah                            ; 0eah = jmp
old09fs         dw      0                               ; old ISR address
old09seg        dw      0                               ; old ISR segment




MyINT08         proc

                push    ax bx cx dx ds si es di

                call    debug_update_info
                call    debug_show_info

                mov     bx, VIDEOSEG
                mov     es, bx
                mov     di, RECT_ADDR
                xor     bx, bx
                mov     bl, es:[di]
                cmp     bl, cs:[RECT_STYLE]
                je      @@save_screen_end
@@save_screen:
                call    save_tablet_bgr
@@save_screen_end:

                mov     al, cs:[REG_TABLE_TOGGLE]
                cmp     al, 01h
                je      @@start
                jmp     @@end

@@start:
                pop     di es si ds dx cx bx ax
                push    ax bx cx dx ds si es di
                push    ax bx cx dx

                push    cs                              ;|
                pop     ds                              ;| ds = cs (code segment)
                mov     si, offset RECT_STYLE           ; si = rect style mem addr
                mov     ah, 4eh                         ; ah - color

                call    show_register_tablet

@@end:
                jmp     JmpOldINT08

                iret
                endp

JmpOldINT08:
                pop     di es si ds dx cx bx ax
                db      0eah                            ; 0eah = jmp
old08fs         dw      0                               ; old ISR address
old08seg        dw      0                               ; old ISR segment



;##########################################
;        register_tablet_manager
;------------------------------------------
;------------------------------------------
; Descr:
;
; Entry:
;
;
; Desroy:
;
;------------------------------------------

;------------------------------------------
;##########################################










;#############################################################################################
;           show_register_tablet
;-------------------------------------------------------
; :FIXME: Разобраться с адресацией [BP+2k] по стэку
;-------------------------------------------------------
; Descr:
;       Draws a tablet with registers (AX, BX, CX, DX) info
;       at right upper corner in VIDEOSEG
; Entry:
;       AH    - rectangle color attr
;       DS:SI - rectangle style mem addr
;       pops from stack values from registers
;       push them in stack in order: AX, BX, CX, DX
; Desroy: AX, BX, CX,
;-------------------------------------------------------
show_register_tablet     proc
                push    bp                              ;| save prev bp
                mov     bp, sp                          ;| bp = sp


                mov     bx, VIDEOSEG                    ;|
                mov     es, bx                          ;| es = VIDEOSEG

                mov     bx, RECT_HEIGHT                 ;| rect height
                mov     cx, RECT_WIDTH                  ;| rect width
                mov     di, RECT_ADDR                   ;| rect left upper corner mem addr

                jmp     @@start

@@start:
                push    di                              ;|
                call    draw_rect                       ;| draw frame
                pop     di                              ;|

                add     di, 2                           ;|
                add     di, CONSOLE_WIDTH * 2           ;| di - addr of first row in frame

                mov     si, offset AX_REG_EQU
                push    di
                call    draw_string
                mov     bx, [bp + 10]                    ; 1st stack arg. AX
                call    draw_16bits
                pop     di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset BX_REG_EQU
                push    di
                call    draw_string
                mov     bx, [bp + 8]                    ; 2nd stack arg. BX
                call    draw_16bits
                pop     di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset CX_REG_EQU
                push di
                call draw_string
                mov     bx, [bp + 6]                    ; 3rd stack arg. BX
                call    draw_16bits
                pop di

                add     di, CONSOLE_WIDTH * 2
                mov     si, offset DX_REG_EQU
                push di
                call draw_string
                mov     bx, [bp + 4]                    ; 4th stack arg. BX
                call    draw_16bits
                pop di



                pop     bp                              ; restore prev bp val
                ret     8d                              ; stack args clear, ret
                endp
;------------------------------------------
;##########################################


;##########################################
;               draw_16bits
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws a hex presentation of 16 bits number on ES:DI addr
; Entry:
;       AH      ; color attr
;       BX      ; input number
;       ES:DI   ; line beginning addr
; Desroy: BX, CX, AX, DI, SI
;------------------------------------------------------;
draw_16bits     proc

                mov     si, bx                          ; save bx
                xor     cx, cx                          ; cx = 0
                cld                                     ; DF = 0 (++)
@@WHILE:;-----------------------------------------------;
                mov     bx, si                          ; restore bx

                shl     bx, cl                          ; mov char number of cl to
                shr     bx, 12d                         ; lower place

                cmp     bx, 10                          ; bx - digit?
                jl      @@DIGITS
@@LETTERS:
                add     bx, 7h                          ;|
@@DIGITS:                                               ;| bx -> ASCII
                add     bx, DIGITS_SHIFT                ;|

                mov     al, bl                          ;| al - ASCII code
                stosw                                   ;| es:[di+=2] = ax

                add     cl, 4d                          ;| next char

                cmp     cl, 16d                         ;| if 4 char were written -> end
                jne     @@WHILE
;WHILE_END----------------------------------------------;
                ret
                endp
;------------------------------------------
;##########################################

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
;             debug_update_info
;------------------------------------------
;------------------------------------------
; Descr:
;           Updates debug info from DEBUG_INFO data block
; Entry:
;           None
;
; Desroy:
;           None
;------------------------------------------
debug_update_info proc
                push    ax bx cx dx di si es ds

                xor     ax, ax
                mov     al, cs:[REG_TABLE_TOGGLE]
                mov     cs:[DEBUG_VAL_1], ax

                push    VIDEOSEG
                pop     es
                mov     di, RECT_ADDR
                mov     bl, es:[di]
                mov     cs:[DEBUG_CHAR_1], bl

                pop     ds es si di dx cx bx ax

                ret
                endp
;------------------------------------------
;##########################################

;##########################################
;             debug_show_info
;------------------------------------------
;------------------------------------------
; Descr:
;           Prints debug info from DEBUG_INFO data block
; Entry:
;           None
;
; Desroy:
;           None
;------------------------------------------
debug_show_info proc
                push    ax bx cx dx di si es ds

                mov     bx, VIDEOSEG                    ;|
                mov     es, bx                          ;| es = VIDEOSEG

                mov     ah, 01001111b
                push    cs
                pop     ds

                mov     di, 2 * CONSOLE_WIDTH * 0
                mov     si, offset DEBUG_LABEL_1
                call    draw_string
                mov     bx, cs:[DEBUG_VAL_1]
                call    draw_16bits

                mov     di, 2 * CONSOLE_WIDTH * 1
                mov     si, offset DEBUG_LABEL_2
                call    draw_string
                mov     bx, cs:[DEBUG_VAL_2]
                call    draw_16bits

                mov     di, 2 * CONSOLE_WIDTH * 2
                mov     si, offset DEBUG_LABEL_3
                call    draw_string
                mov     bx, cs:[DEBUG_VAL_3]
                call    draw_16bits

                mov     di, 2 * CONSOLE_WIDTH * 3
                mov     si, offset DEBUG_LABEL_4
                call    draw_string
                mov     al, cs:[DEBUG_CHAR_1]
                call debug_draw_char

                pop     ds es si di dx cx bx ax

                ret
                endp
;------------------------------------------
;##########################################

;##########################################
;               debug_draw_char
;------------------------------------------
;------------------------------------------
; Descr:
;       Draws a char in videoseg by addr DI
; Entry:
;       AX      - char attr
;       DI      - output addr
; Desroy:
;       None
;------------------------------------------
debug_draw_char    proc

                push ax bx es di

                mov     bx, VIDEOSEG
                mov     es, bx
                mov     es:[di], ax
                pop di es bx ax

                ret
                endp
;------------------------------------------


;//----------DEBUG_FUNCTIONS_END. SLOW. SIMPLE. SAFE-------------//
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


;##########################################
;               clear_screen
;------------------------------------------
;------------------------------------------
; Descr:
;       Clears screen
; Entry:
;       None
; Destr:
;       None
;------------------------------------------
clear_sreen proc
    push ax bx cx dx
    mov ax, 0620h
    mov bx, 0h
    mov cx, 0h
    mov dx, 1998h
    int 10h
    pop dx cx bx ax
    ret
endp
;------------------------------------------
;##########################################


;##########################################
;               save_tablet_bgr
;------------------------------------------
;------------------------------------------
; Descr:
;       Save video memory area, obscured by
;        register tablet to TWIN_VIDEO_MEM
;       Uses tablet info from 'Register tablet info' section
; Entry:
;       None
; Desroy:
;       None
;------------------------------------------
save_tablet_bgr     proc

                push ax cx si di es ds

                mov     ax, VIDEOSEG                    ;|
                mov     ds, ax                          ;| ds = VIDEOSEG
                push    cs                              ;|
                pop     es                              ;| es = cs (TWIN_VIDEO_MEM segment)

                mov     si, RECT_ADDR
                mov     di, (offset TWIN_VIDEO_MEM) + RECT_ADDR
                mov     ax, RECT_HEIGHT

@@WHILE:
                mov     cx, RECT_WIDTH
                rep     movsw                           ; ds:[si+=2] => es:[di+=2]
                sub     si, RECT_WIDTH * 2 - CONSOLE_WIDTH * 2
                sub     di, RECT_WIDTH * 2 - CONSOLE_WIDTH * 2

                dec     ax

                cmp     ax, 0h
                jg      @@WHILE


                pop     ds es di si cx ax
                ret
                endp
;------------------------------------------
;##########################################

;##########################################
;               load_tablet_bgr
;------------------------------------------
;------------------------------------------
; Descr:
;       Restore video memory area, obscured by
;        register tablet from TWIN_VIDEO_MEM
;       Uses tablet info from 'Register tablet info' section
; Entry:
;       None
; Desroy:
;       None
;------------------------------------------
load_tablet_bgr     proc

                push ax cx si di es ds

                mov     ax, VIDEOSEG                    ;|
                mov     es, ax                          ;| ds = VIDEOSEG
                push    cs                              ;|
                pop     ds                              ;| es = cs (TWIN_VIDEO_MEM segment)

                mov     di, RECT_ADDR
                mov     si, (offset TWIN_VIDEO_MEM) + RECT_ADDR
                mov     ax, RECT_HEIGHT

@@WHILE:
                mov     cx, RECT_WIDTH
                rep     movsw                           ; ds:[si+=2] => es:[di+=2]

                dec     ax
                sub     si, RECT_WIDTH * 2 - CONSOLE_WIDTH * 2
                sub     di, RECT_WIDTH * 2 - CONSOLE_WIDTH * 2
                cmp     ax, 0h
                jg      @@WHILE


                pop     ds es di si cx ax
                ret
                endp

;------------------------------------------
;##########################################









;##########################################
;               save_screen
;------------------------------------------
;------------------------------------------
; Descr:
;       Saves VIDEOSEG CONSOLE_WIDTH * CONSOLE_HEIGHT
;        to TWIN_VIDEO_MEM
; Entry:
;       None
; Desroy:
;       None
;------------------------------------------
save_screen     proc

                push ax cx si di es ds

                mov     ax, VIDEOSEG                    ;|
                mov     ds, ax                          ;| ds = VIDEOSEG
                push    cs                              ;|
                pop     es                              ;| es = cs (TWIN_VIDEO_MEM segment)

                mov     cx, CONSOLE_WIDTH * CONSOLE_HEIGHT      ; cx - count of copying words
                xor     si, si                                  ; si = 0 (start addr of video memory)
                lea     di, TWIN_VIDEO_MEM                      ; di = TWIN_VIDEO_MEM addr

                rep     movsw                           ; ds:[si+=2] => es:[di+=2]

                pop     ds es di si cx ax
                ret
                endp
;------------------------------------------
;##########################################

;##########################################
;               load_screen
;------------------------------------------
;------------------------------------------
; Descr:
;       Loads VIDEOSEG CONSOLE_WIDTH * CONSOLE_HEIGHT
;        from TWIN_VIDEO_MEM
; Entry:
;       None
; Desroy:
;       None
;------------------------------------------
load_screen     proc


                push ax cx si di es ds

                mov     ax, VIDEOSEG                    ;|
                mov     es, ax                          ;| es = VIDEOSEG
                push    cs                              ;|
                pop     ds                              ;| ds = cs (TWIN_VIDEO_MEM segment)

                mov     cx, CONSOLE_WIDTH * CONSOLE_HEIGHT      ; cx - count of copying words
                xor     di, di                                  ; di = 0 (start addr of video memory)
                lea     si, TWIN_VIDEO_MEM                      ; si = TWIN_VIDEO_MEM addr

                rep     movsw                           ; ds:[si+=2] => es:[di+=2]
                pop     ds es di si cx ax
                ret
                endp
;------------------------------------------
;##########################################


.data

REG_TABLE_TOGGLE        db  00h
REG_TABLE_TOGGLE_TIMER  dw  0000h
REG_TABLE_TOGGLE_DELAY  dw  0006h

VIDEOSEG                equ 0b800h
POP_KEY                 equ 22h             ; 'G' scan code
TEXT_END_CHAR           equ '%'

CONSOLE_WIDTH           equ 80d
CONSOLE_HEIGHT          equ 25d
CONSOLE_WIDTH_BYTE      db  80d
CONSOLE_HEIGHT_BYTE      db  25d


;##########################################
;           Register tablet info
;##########################################
RECT_STYLE              db  "+=+|.|+=+$"
RECT_HEIGHT             equ  9d
RECT_WIDTH              equ  9d
RECT_ADDR               equ  (CONSOLE_WIDTH - RECT_WIDTH) * 2
TWIN_VIDEO_MEM          dw  CONSOLE_WIDTH*CONSOLE_HEIGHT dup(?)
;##########################################

;##########################################
;           DEBUG_INFO
;##########################################
DEBUG_VAL_1             dw  0000d
DEBUG_VAL_2             dw  0000d
DEBUG_VAL_3             dw  0000d
DEBUG_CHAR_1            db  00d
DEBUG_LABEL_1           db  'Toggle:%$'
DEBUG_LABEL_2           db  'SaveSc:%$'
DEBUG_LABEL_3           db  'LoadSc:%$'
DEBUG_LABEL_4           db  'LefCor:%$'
;##########################################



DIGITS_SHIFT            equ 30h
AX_REG_EQU              db "AX=%$"
BX_REG_EQU              db "BX=%$"
CX_REG_EQU              db "CX=%$"
DX_REG_EQU              db "DX=%$"



TSR_END:

end start
