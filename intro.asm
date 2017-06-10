; (c) 1999 joric^proxium (most code, letters, blur and color morphing)
; sinetable fx from Radian 512b by BSL/ZcS (ByteFall'99)

.model tiny
.code
.startup
.386
start:

dstr macro x,y,start,attack,sustain,decay,pos,string
    dw x,y,start,attack,sustain,decay,pos
    db string,0
endm

writeln macro _string
local _afterstring
      call _afterstring
db    _string,13,10,'$'
_afterstring:
       pop dx
       mov ah,9
       int 21h
endm

malloc macro buf,size
      mov     ah,48h
      mov     bx,size
      int     21h
      mov     buf,ax
endm
-----------------------------------------------------------------------------
                ; adjust memory block size (setblock)
                mov     ah, 4Ah
                mov     bx, 1000h  ; BX = new size in paragraphs
                push    cs
                pop     es         ; ES = segment address of block to change
                int     21h

                malloc  buffer1,1000h
                malloc  buffer2,1000h
                malloc  palette,768*2
                malloc  textbuf1,168h
                malloc  textbuf2,168h
                malloc  gs,1000h
                malloc  fs,1000h
                malloc  tab,200h

                push    buffer1
                pop     buffer

                call    clearbuffer
-----------------------------------------------------------------------------
                push    buffer2
                pop     buffer
                call    clearbuffer
                push    buffer1
                pop     buffer

                call    @gensinus

                mov    ax,70
                mov    TimerRate,ax
                call   SetInt

                mov ax,1130h
                mov bh,06h
                int 10h
                mov fontsegm,es
                mov fontoffs,bp

                mov   ax,13h
                int   10h ;goto 13h mode
@WaitESC:
                call    fx
                call    greets
                call    pal_morph
                call    wait_retrace

                call    copybuffer
                mov     ax,showtime
                cmp     ax,counter
                jnc     @notimeover
                mov     counter,0
                @notimeover:

                ;read scancode
                in      al,60h
                cmp     al,1       ; is ESC pressed?
                jne     @WaitESC
                ; flush keyboard buffer
                xor     ah,ah
                int     16h

                call    ResetInt

                ;textmode
                mov     ax,3
                int     10h
                writeln ('Coded by Joric^Proxium (c) Y2K');
                ;terminate program and exit
                mov     ax,4C00h
                int     21h
-----------------------------------------------------------------------------
clearbuffer:    mov     es,buffer
                xor     di,di
                xor     ax,ax
                mov     cx,64000/2
                rep     stosw
                ret
-----------------------------------------------------------------------------
lerp_x1 dw ?
lerp_x2 dw ?
lerp_x  dw ?
lerp_v1 dw ?
lerp_v2 dw ?
lerp_v  dw ?
lerp:           ;lerp: linear interpolation between v1 and v2
                ; v=v1+(v2-v1)*x/(x2-x1)
                ; x1 > x2 allowed; v1 > v2 allowed
                push   ax
                push   bx
                push   dx
@lerp_3:
                mov    ax,lerp_v2
                sub    ax,lerp_v1
                mov    bx,lerp_x2
                sub    bx,lerp_x1
                jnz    @lerp4
                mov    ax,lerp_v1
                jmp    @exit_lerp
@lerp4:         xor    dx,dx
                push   bx
                mov    bx,lerp_x
                sub    bx,lerp_x1
                imul   bx
                pop    bx
                idiv   bx
                add    ax,lerp_v1
@exit_lerp:     mov    lerp_v,ax
                pop    dx
                pop    bx
                pop    ax
                ret
-----------------------------------------------------------------------------
set_palette:   push     di
               mov      cx,pal_num
               inc      cx
               xor      bx,bx
@sp1:          mov      dl,[offset palettes+bx]
               inc      bx
               dec      cx
               jz       @sp_set
               xor      dh,dh
               mov      ax,dx
               mov      dh,8
               mul      dh
               add      bx,ax
               jmp      @sp1
               mov      bx,1
@sp_set:
;               mov      dl,5
;               mov      bx,1
               mov      ah,dl
@sp_rep:       mov      si,offset i1
               mov      cx,8
@sp2:          mov      al,[offset palettes+bx]
               mov      [si],al
               inc      bx
               inc      si
               loop     @sp2
               push     ax
               push     bx
               call     setgradient
               pop      bx
               pop      ax
               dec      ah
               jnz      @sp_rep
               pop      di
               ret
-----------------------------------------------------------------------------
i1 db 0
i2 db 255
C1 db 0,0,0
C2 db 63,0,63
i  db ?
k  db ?

setgradient:
                mov     es,palette

                xor     ah,ah
                mov     al,i1
                mov     dl,3
                mul     dl
                mov     di,ax    ;di+=i1*3
                add     di,pal_offset

                mov     ch,i2
                sub     ch,i1
                inc     ch
                mov     al,i1
                mov     i,al

@pl1:           xor     bx,bx
                mov     cl,3
@pl2:           xor     ah,ah
                mov     al,i1
                mov     lerp_x1,ax
                mov     al,i2
                mov     lerp_x2,ax
                mov     al,i
                mov     lerp_x,ax
                mov     al,[C1+bx]
                mov     lerp_v1,ax
                mov     al,[C2+bx]
                mov     lerp_v2,ax
                call    lerp
                mov     ax,lerp_v

                mov     es:[di],al
                inc     di
                inc     bx
                dec     cl
                jnz     @pl2
                inc     i
                dec     ch
                jnz     @pl1
                ret
-----------------------------------------------------------------------------
setpalette:     push    ds
                cli
                mov     si,pal_offset
                mov     ds,palette
                mov     dx,3C6h
                mov     al,0FFh
                out     dx,al
                mov     dx,3C8h
                xor     al,al
                out     dx,al
                mov     dx,3C9h
                mov     cx,768
                ;cld
                ;rep     outsb
@inner:         mov     al,ds:[si]
                out     dx,al
                inc     si
                loop    @inner

                pop     ds
                sti
                ret
-----------------------------------------------------------------------------
border=0
copybuffer:     push    ds
                mov     es,screen
                xor di,di
                mov     di,320*border
                mov     si,di
                mov     ds,buffer
                mov     cx,64000/2
                mov     cx,(64000-320*border*2)/2
                rep     movsw
                pop     ds
                ret
-----------------------------------------------------------------------------
blur:           push  ds
                mov   dx,blurwidth
                shr   dx,1
                mov   es,textbuf1
                mov   ds,textbuf2
                mov   di,dx
                mov   si,dx
                mov   cx,320*16
                sub   cx,dx
                xor   bx,bx
@loop2:         mov   ax,bx
                ;div  ax by dx... yeah, I can write cool comments :)
                cmp   dx,2
                jnc   @b_good
                mov   al,es:[di]
                jmp   @b_good1
@b_good:        push  dx
                push  bx
                mov   bx,dx
                add   bx,bx
                xor   dx,dx
                div   bx
                pop   bx
                pop   dx
                sub   al,32
                jnc   @b_good1
                xor   al,al
@b_good1:       mov   ds:[si],al
                ;recalculate scanline
                push  di
                xor   ah,ah
                sub   di,dx
                mov   al,es:[di]
                sub   bx,ax
                add   di,dx
                add   di,dx
                mov   al,es:[di]
                add   bx,ax
                pop   di

                inc   di
                inc   si

                loop  @loop2
                pop   ds
                ret
-----------------------------------------------------------------------------
wait_retrace:
                mov   dx,3DAh
@wr1:           in    al,dx
                and   al,08h
                jnz   @wr1
@wr2:           in    al,dx
                and   al,08h
                jz    @wr2
                ret
-----------------------------------------------------------------------------
fontsegm dw ?
fontoffs dw ?

printxchar proc far
                push  di
                push  x
                push  y
                mov   es,fontsegm
                mov   di,fontoffs
                mov   al,char
                xor   ah,ah
                shl   ax,4
                add   di,ax
                mov   bx,16
@prcx0:         push  x
                mov   cx,8
                mov   al,es:[di]
@prcx1:         shl   al,1
                jnc   @prcx2
                call  p_textbuf
@prcx2:         inc   x
                loop  @prcx1
                pop   x
                inc   di
                inc   y
                dec   bx
                jnz   @prcx0
                pop   y
                pop   x
                pop   di
                ret
endp
-----------------------------------------------------------------------------
printchar:      call  findchar
                push  x
                push  y
                mov   si,offset font
                mov   al,char
                shl   al,1
                xor   ah,ah
                add   si,ax
                mov   bx,16
@prc0:          push  x
                mov   cx,16
                mov   ah,[si]
                mov   al,[si+1]
@prc1:          shl   ax,1
                jnc   @prc2
                call  p_textbuf
@prc2:          inc   x
                loop  @prc1
                pop   x
                mov   ax,tablelength
                shl   ax,1
                add   si,ax
                inc   y
                dec   bx
                jnz   @prc0
                pop   y
                pop   x
                ret
-----------------------------------------------------------------------------
; message offset in ds:di
printstr:       push    y
                push    x
                mov     y,0
                ;calculating string width & coords
                push    di
                xor     cx,cx
@pr0:           mov     al,[di]
                cmp     al,0
                jz      @prexit
                inc     di
                add     cx,char_width
                jmp     @pr0
@prexit:        pop     di

                mov     ax,pm_align
                cmp     ax,pm_centered
                jnz     @p11
                shr     cx,1
                sub     x,cx
                jmp     @pr1
@p11:           cmp     ax,pm_align_right
                jnz     @pr1
                sub     x,cx
@pr1:           mov     al,[di]
                cmp     al,0
                jz      @prexit1
                mov     char,al
                call    printchar
                inc     di
                mov     dx,char_width
                add     x,dx
                jmp     @pr1
@prexit1:       pop     x
                pop     y
                ret
-----------------------------------------------------------------------------
findchar:
                push    ax
                push    di
                mov     di,offset chartable
                mov     ah,-1
@fch1:          mov     al,[di]
                inc     di
                inc     ah
                cmp     al,0
                jz      @fcnotf
                cmp     al,char
                jnz     @fch1
                mov     char,ah
                jmp     @fcexit
@fcnotf:        mov     char,0
@fcexit:        pop     di
                pop     ax
                ret
-----------------------------------------------------------------------------
p_textbuf:      ;this procedure sets point to textbuf1
                push    es
                push    ax
                push    di
                push    bx
                mov     es,textbuf1
                mov     ax,y
                inc     ax
                mov     bx,320
                mul     bx
                add     ax,x
                mov     di,ax
                mov     al,color
                stosb
                pop     bx
                pop     di
                pop     ax
                pop     es
                ret
-----------------------------------------------------------------------------
;this procedure copying textbuffer2 line to screen buffer at position y
copytextbuf:    push    ds
                mov     es,buffer
                mov     ax,y
                mov     bx,320
                mul     bx
                mov     di,ax
                mov     ds,textbuf2
                xor     si,si
                add     si,320
                mov     cx,320*15
@ctxtb:         mov     al,ds:[si]
                mov     bl,es:[di]
                add     al,bl
                jnc      @ctxok
                mov      al,255
@ctxok:         mov     es:[di],al
                inc     si
                inc     di
                loop    @ctxtb
                pop     ds
                ret
-----------------------------------------------------------------------------
cleartxtbuf:    mov     es,textbuf1
                xor     di,di
                xor     ax,ax
                mov     cx,320*18/2
                rep     stosw
                ret
-----------------------------------------------------------------------------
writemessage:    call    cleartxtbuf
                 call    get_msg_offset
                 mov     bx,di
                 add     bx,ax
                 mov     ax,[bx]
                 mov     x,ax
                 mov     ax,[bx+2]
                 mov     y,ax
                 add     di,14
                 call    printstr
                 call    blur
                 call    copytextbuf
                 ret
-----------------------------------------------------------------------------
get_msg_offset:  mov     di,offset msg_parm
                 push    cx
                 mov     cx,message
                 inc     cx
@gms1:           dec     cx
                 jz      @gms3
                 add     di,14
@gms2:           mov     al,[di]
                 inc     di
                 cmp     al,0
                 jnz     @gms2
                 jmp     @gms1
@gms3:           pop     cx
                 ret
-----------------------------------------------------------------------------
greets:          xor     ax,ax
                 mov     message,ax
@g1:             call    get_msg_offset
                 cmp     di,offset end_msg_parm
                 jz      @gexit
                 mov     bx,di
                 mov     ax,[bx+4]
                 mov     starttime,ax
                 mov     ax,[bx+6]
                 mov     attacktime,ax
                 mov     ax,[bx+8]
                 mov     sustaintime,ax
                 mov     ax,[bx+10]
                 mov     decaytime,ax
                 mov     ax,[bx+12]
                 mov     pm_align,ax
                ;--------------
                 mov     ax,starttime
                 cmp     ax,counter
                 jnc     @fwdjmp

                 add     ax,attacktime
                 cmp     ax,counter
                 jnc     @attack

                 add     ax,sustaintime
                 cmp     ax,counter
                 jnc     @sustain

                 add     ax,decaytime
                 cmp     ax,counter
                 jnc     @decay
                 jmp     @outmes

                 jmp     @gnext

@fwdjmp:        jmp     @outmes
@backjmp:       jmp     @g1
@gnext:
                ------------------------------------;ATTACK
@attack:         ;lerp
                 mov    [lerp_x2],ax
                 sub    ax,attacktime
                 mov    [lerp_x1],ax
                 mov    ax,counter
                 mov    [lerp_x],ax
                 mov    [lerp_v1],155
                 mov    [lerp_v2],0
                 call   lerp
                 mov    ax,[lerp_v]
                 jmp    @gwrite
                ------------------------------------;SUSTAIN
@sustain:        mov     ax,0
                 jmp     @gwrite
                ------------------------------------;DECAY
@decay:          ;lerp
                 mov    [lerp_x2],ax
                 sub    ax,decaytime
                 mov    [lerp_x1],ax
                 mov    ax,counter
                 mov    [lerp_x],ax
                 mov    [lerp_v1],0
                 mov    [lerp_v2],155
                 call   lerp
                 mov    ax,[lerp_v]
@gwrite:         mov    blurwidth,ax
                 call   writemessage
@outmes:         inc     [message]
                 jmp   @backjmp
@gexit:          ret
-----------------------------------------------------------------------------
fx:             mov     di,offset fx_parm
@fx1:           mov     ax,[di]
                cmp     ax,counter
                jnc     @nofx
                mov     ax,[di+2]
                mov     effect_num,ax
                mov     ax,[di+4]
                mov     fx_speed,ax
@nofx:          add     di,6
                cmp     di,offset end_fx_parm
                jnz     @fx1
                call    @effect
                ret
-----------------------------------------------------------------------------
pal_morph:      mov     di,offset pal_parm
@p_loop1:       push    di
                mov     ax,[di+0]
                cmp     ax,counter
                jnc     @outpal

                add    ax,[di+2]
                cmp    ax,counter
                jc     @outpal

                mov    ax,[di+4]
                mov    pal_num,ax
                call   set_palette

                mov    ax,[di+6]
                mov    pal_num,ax
                mov    pal_offset,768
                call   set_palette

                mov    ax,[di+0]
                mov    [lerp_x1],ax
                add    ax,[di+2]
                mov    [lerp_x2],ax
                mov    ax,counter
                mov    [lerp_x],ax
                mov    es,palette
                mov    cx,768
                xor    di,di
@pmorph:        xor    ah,ah
                mov    al,es:[di]
                mov    [lerp_v1],ax
                mov    al,es:[di+768]
                mov    [lerp_v2],ax
                call   lerp
                mov    ax,[lerp_v]
                mov    es:[di],al
                inc    di
                loop   @pmorph

                mov    pal_offset,0
                call   setpalette
@outpal:        pop    di
                add    di,8
                cmp    di,offset end_pal_parm
                jnz    @p_loop1
                ret
-----------------------------------------------------------------------------
@gensinus:
; ------------- Генерим синусоиду ----------------------------------------
; for(x=0; x<256; x++) { tab[x]=(byte)(sin(a)*128); a+=(2*pi/256); }
                xor     si,si
                fninit
                mov     es,tab
                xor     di,di
                mov     cx,256
                fldz
gts:
                fld     st
                fsin
                fimul   word ptr [i128]
                fistp   word ptr [si]

                mov     al,ds:[si]
                mov     es:[di+256],al
                movsb

                dec     si
                fadd    dword ptr [f2PIdiv256]
                loop    gts

; ------------- Генерим таблицы окружностей и радиусов ---------------------
;               for(y=-100; y<100; y++) for(x=-160; x<160; x++)
;                    {   *r++ = (char)sqrt((x*x) + (y*y));
;                 *p++ = (char)(atan(((float)x / (float)y))*(256/pi)); }
                push    gs
                pop     es
                xor     di,di
                mov     bx,-100
gty:
                mov     [si],bx
                fild    word ptr [si]
                fld     st
                fmul    st,st
                mov     cx,-160
gtx:
                mov     [si],cx
                fild    word ptr [si]
                fld     st
                fmul    st,st
                fadd    st,st(2)
                fsqrt
                fistp   word ptr [si]
                mov     al,[si]
                mov     fs:[di],al
                fld     st(2)
                fpatan
                fmul    dword ptr [f256divPI]
                fistp   word ptr [si]
                movsb
                dec     si
                inc     cx
                cmp     cx,160
                jl      gtx
                fninit ; А мне пофиг на тормоза ;) Зато короче...
;                fstp    st
;                fstp    st
                inc     bx
                cmp     bx,100
                jl      gty
; ------------- С таблицами покончено :) --------------------------------

@effect:
                push    ds
                push    buffer
                pop     es
                mov     bx,effect_num
                movzx   dx,[proctab+bx]

                push    counter

                add     dx,offset proc1
                mov     ds,[tab]
                mov     cx,64000
                xor     di,di

                call    dx

                pop     bx  ; getting rotating speed aka offset
                xor     bh,bh

                push    ds
                pop     es
                ;copy table with bx offset (timer driven routine)
                mov     cx,256
                xor     di,di
@gl1:           mov     al,es:[bx+256]
                mov     [di],al
                inc     bl
                inc     di
                loop   @gl1

                pop     ds
                ret
-----------------------------------------------------------------------------
proc1:
                ; *v++ = (tab[*r]+tab[(*p++ - tab[*r++])&255]);
;                and     bx,255
                mov     bl,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*r]
                mov     bl,gs:[di]      ; *p
                sub     bl,dl           ; *p - tab[*r]
                mov     al,ds:[bx]      ; tab[(*p - tab[*r])&255]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc1
                ret
-----------------------------------------------------------------------------
proc2:
                ; *v++ = (tab[*p]+tab[(*r++ - tab[*p++])&255]);
                mov     bl,gs:[di]      ; *p
                mov     dl,ds:[bx]      ; tab[*p]
                mov     bl,fs:[di]      ; *r
                sub     bl,dl           ; *r - tab[*p]
                mov     al,ds:[bx]      ; tab[(*r - tab[*p])&255]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc2
                ret
-----------------------------------------------------------------------------
proc3:
                ; *v++ = (tab[*p++]+tab[tab[*r++])&255]);
                mov     bl,gs:[di]      ; *p
                mov     dl,ds:[bx]      ; tab[*p]
                mov     bl,fs:[di]      ; *r
                mov     bl,ds:[bx]      ; tab[*r]
                mov     al,ds:[bx]      ; tab[tab[*r]]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc3
                ret
-----------------------------------------------------------------------------
proc4:
                ; *v++ = (tab[*r++]+tab[tab[*p++])&255]);
                mov     bl,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*r]
                mov     bl,gs:[di]      ; *p
                mov     bl,ds:[bx]      ; tab[*p]
                mov     al,ds:[bx]      ; tab[tab[*p]]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc4
                ret
-----------------------------------------------------------------------------
proc5:
                ; *v++ = (tab[*r++]+tab[(*p++ - tab[0])&255]);
                mov     bl,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*r]
                mov     bl,gs:[di]      ; *p
                mov     al,ds:[0]       ; tab[0]
                sub     bl,al           ; *p - tab[0]
                mov     al,ds:[bx]      ; tab[(*p - tab[0])&255]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc5
                ret
-----------------------------------------------------------------------------
proc6:
                ; *v++ = (tab[*p++]+tab[(*r++ - tab[0])&255]);
                mov     bl,gs:[di]      ; *p
                mov     dl,ds:[bx]      ; tab[*r]
                mov     bl,fs:[di]      ; *r
                mov     al,ds:[0]       ; tab[0]
                sub     bl,al           ; *r - tab[0]
                mov     al,ds:[bx]      ; tab[(*r - tab[0])&255]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc6
                ret
-----------------------------------------------------------------------------
proc7:
                ; *v++ = (*p + tab[*p++] + *r + tab[*r++]);
                mov     bl,gs:[di]      ; *p
                mov     dl,ds:[bx]      ; tab[*p]
                add     dl,bl           ; *p + tab[*p++]
                mov     bl,fs:[di]      ; *r
                mov     al,ds:[bx]      ; tab[*r]
                add     al,bl           ; *r + tab[*r++]
                add     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc7
                ret
-----------------------------------------------------------------------------
proc8:
                ; *v++ = (tab[(*p++ + tab[*r++])&255]);
                mov     bl,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*r]
                mov     bl,gs:[di]      ; *p
                add     bl,dl           ; *p + tab[*r]
                mov     al,ds:[bx]      ; tab[(*p - tab[*r])&255]
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc8
                ret
-----------------------------------------------------------------------------
proc9:
                ; *v++ = ((tab[*p] + *r)-tab[(*p + tab[*r])&255]);
                mov     bl,gs:[di]      ; *p
                mov     al,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*p]
                add     dl,al           ; tab[*p] + *r
                xchg    al,bl
                mov     bl,ds:[bx]      ; tab[*r]
                add     bl,al
                mov     al,ds:[bx]      ; tab[*p + tab[*r]]
                sub     al,dl
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc9
                ret
-----------------------------------------------------------------------------
proc10:
                ; *v++ = tab[(*p + *r)&255] + tab[(*r+tab[*p])&255];
                mov     bl,gs:[di]      ; *p
                mov     al,fs:[di]      ; *r
                mov     dl,ds:[bx]      ; tab[*p]
                add     dl,al           ; tab[*p] + *r
                add     bl,al
                mov     al,ds:[bx]      ; tab[*p + *r]
                mov     bl,dl
                add     al,ds:[bx]
                add     al,ah
                jns      $+4
                neg     al
                stosb
                loop    proc10
                ret
-----------------------------------------------------------------------------
f256divPI       dd      81.4873308630504119     ; 256/pi
f2PIdiv256      dd      0.02454369260617026     ; 2*pi/256
i128            db      128
frame           dw      0
proctab         label   byte
                db      offset proc1 - offset proc1
                db      offset proc2 - offset proc1
                db      offset proc3 - offset proc1
                db      offset proc4 - offset proc1
                db      offset proc5 - offset proc1
                db      offset proc6 - offset proc1
                db      offset proc7 - offset proc1
                db      offset proc8 - offset proc1
                db      offset proc10 - offset proc1
                db      offset proc9 - offset proc1
tab             dw      ?
temp            dd      ?

counter         dw      0
-----------------------------------------------------------------------------
; Enables and starts the player interrupt.
SetInt:         cli
                xor     ax,ax
                mov     es,ax
                mov     ax,es:[8*4]
                mov     word ptr OldInt,ax
                mov     ax,es:2[8*4]
                mov     word ptr OldInt+2,ax
                mov     word ptr es:[8*4], offset NewInt
                mov     es:2[8*4],cs
                mov     ax,TimerRate
; Set the interrupt timer duty cycle.
; IN AX      - number of times per second for INT08.
                mov     bx,ax
                mov     ax,1193180 mod 65536
                mov     dx,1193180 / 65536
                div     bx
                mov     bx,ax
                mov     al,36h
                out     43h,al
                mov     al,bl
                out     40h,al
                mov     al,bh
                out     40h,al
                mov     TimerSteps,bx   ; for keeping 18.2 timer correct
                mov     TimerCnt,0      ; counter
                sti
                ret
-----------------------------------------------------------------------------
; Disables the interrupt.
ResetInt:       cli
                xor     ax,ax
                mov     es,ax
                mov     ax,word ptr OldInt
                mov     es:[8*4],ax
                mov     ax,word ptr OldInt+2
                mov     es:[8*4+2],ax
; Reset the interrupt timer back to 18.2/sec duty cycle.
                mov     al,36h
                out     43h,al
                xor     al,al
                out     40h,al
                out     40h,al
                sti
                ret
-----------------------------------------------------------------------------
; The interrupt.
NewInt:         push    ax
                inc     cs:counter
        ; see if we have passed 18.2/s mark
        @@lx:   mov     ax,TimerSteps           ; this no. of steps per int.
                add     TimerCnt,ax
                jnc     @@ly                    ; don't call timer interrupt
                pop     ax
                jmp     cs:OldInt               ; call old interrupt handlers
        ; standard exit
        @@ly:   mov     al,20h
                out     20h,al
                pop     ax
                iret
-----------------------------------------------------------------------------
OldInt          dd      ?
TimerCnt        dw      ?
TimerSteps      dw      ?
TimerRate       dw      70 ; timer interrupt rate in ticks
-----------------------------------------------------------------------------
screen          dw      0a000h
x               dw      10
y               dw      100
tx              dw      0
ty              dw      0
color           db      255
char            db      0
textbuf1        dw      ?
textbuf2        dw      ?
buffer1         dw      ?
buffer2         dw      ?
buffer          dw      ?
palette         dw      ?
blurwidth       dw      2
maxblurwidth    dw      256
blurinc         dw      1
chartable       db      '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-:",.!/()? ',0
tablelength     dw      47
message         dw      0
starttime       dw      50
attacktime      dw      50
sustaintime     dw      100
decaytime       dw      50
morphtime       dw      50
morph_into      dw      1
pal_num         dw      0
pal_offset      dw      0
pm_align        dw      1
effect_num      dw      3
fx_speed        dw      0
char_width      dw      12
-----------------------------------------------------------------------------
pm_align_right=0
pm_centered=1
pm_align_left=2
-----------------------------------------------------------------------------
palettes:
; n,  [index1,index2, R1,G1,B2, R2,G2,B2] [, ..(n times) ]
 black=0
db 2,   0,128,  0,0,0, 0,0,0
db      128,255,  0,0,0, 63,63,63
 red2yellow=1
db 3,   64,128,  63,0,0, 63,63,0
db      0,64,  0,0,0, 63,0,0
db      128,255,  63,63,0, 63,63,63
 green=2
db 2,   0,128,  0,0,0, 0,63,0
db      128,255,  0,63,0, 63,63,63
 blue=3
db 2,   0,128,  0,0,0, 0,0,63
db      128,255,  0,0,63, 63,63,63
 red=4
db 2,   0,128,  0,0,0, 63,0,0
db      128,255,  63,0,0, 63,63,63
 gray=5
db 1,   0,255,  0,0,0, 63,63,63
 purple=6
db 2,   0,128,    0,0,0, 32,32,42
db      128,255,  32,32,42, 63,63,63
 white=7
db 1,   0,255,    63,63,63, 63,63,63
-----------------------------------------------------------------------------
;This table needs 0 bytes, cool! ;)
page01 =  0
page02 =  page01+250
page03 =  page02+250
page04 =  page03+250
page05 =  page04+250
page06 =  page05+250
page07 =  page06+250
-----------------------------------------------------------------------------
showtime        dw      page07
-----------------------------------------------------------------------------
fx_parm:
-----------------------------------------------------------------------------
;starttime,fx,fx_speed
dw     page01, 0, 1
dw     page02+30, 2, 0
dw     page03, 2, 0
dw     page04, 3, 0
-----------------------------------------------------------------------------
end_fx_parm:
-----------------------------------------------------------------------------
pal_parm:
-----------------------------------------------------------------------------
; starttime,morphtime,morph_from,morph_into
dw      page01,               40,            blue, blue
dw      page02-20,            50,            blue, white
dw      page02+30,            50,            white, blue
dw      page04+20,            40,            blue, red
dw      page05,               100,           red,green
dw      page05+100,           100,           green, purple
dw      page05+200,           100,           purple,red2yellow
dw      page05+300,           100,           red2yellow,gray
dw      page05+400,           100,           gray,white
dw      page05+500,           100,           white,blue
-----------------------------------------------------------------------------
end_pal_parm:
-----------------------------------------------------------------------------
msg_parm:
-----------------------------------------------------------------------------
; x,y, start, attack,sustain,decay, pos, string
dstr 160,050, page01+000, 050,100,050, pm_centered,'WELCOME TO'
dstr 160,100, page01+020, 050,100,050, pm_centered,'BLABLABLA'
dstr 160,150, page01+040, 050,100,050, pm_centered,'BLABLABLABLABLA'

dstr 160,080, page02+020, 050,100,050, pm_centered,'THE BEST BLABLA BBS'
dstr 160,120, page02+040, 050,100,050, pm_centered,'EVER CREATED'

dstr 160,030, page03+020, 050,100,050, pm_centered,'SYSOP BLABLABLABLA'
dstr 160,070, page03+040, 050,100,050, pm_centered,'COSYSOP BLABLABLABLA'
dstr 160,110, page03+060, 050,100,050, pm_centered,'2-ND COSYSOP BLABLABLABLA'
dstr 160,160, page03+080, 050,100,050, pm_centered,'MAYBE YOU?'

dstr 160,080, page04+020, 050,100,050, pm_centered,'BLA-BLA-BLA'
dstr 160,120, page04+040, 050,100,050, pm_centered,'BLA!'
-----------------------------------------------------------------------------
end_msg_parm:
----------------------------------------------------------------------------------------------------------------------------------------------------------
font: include font.inc
end start
;EOF