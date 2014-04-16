  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;
;; VARS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .rsset $0000  ;;vars at RAM x0000
  
button1   .rs 1 ;reserve one byte of space
button2   .rs 1 
player1x  .rs 1
player1y  .rs 1

;;
;; CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

P1STARTX	= $70
P1STARTY	= $80

;;memory bank 1; executable code
  .bank 0
  .org $C000 
RESET:		;;when reset or powered on
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down


;;;;;;;;;;;;;;;;;
;;  INIT VARS

  LDA #P1STARTX
  STA player1x
  LDA #P1STARTY
  STA player1y


LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
              
              
              
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
NukeY:
  LDY #$00
  INX
  CPX #$08
  BEQ BGExit
  BNE LoadBackgroundLoop
  LDX #$00
LoadBackgroundLoop:
  LDA background, y     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INY
  CPY #$20				;
  BEQ NukeY
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
BGExit:
              
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


              
              
              
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000 ;; PPU CONTROL

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  

Forever:
  JMP Forever     ;jump back to aForever, infinite loop
  
 

NMI:

  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
;
; CONTROLLERS
;
  JSR ReadControllers
  JSR Movement
  
  RTI		;return from interrupt  
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;  subroutines
;;  

;;  CONTROLLER BITS: Bx-A-B-SEL-ST-U-D-L-R
ReadControllers:  ;;latch buttons on controller 1 and 2
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
    .ReadController1Loop:
  LDA $4016
  LSR A
  ROL button1
  DEX
  BNE .ReadController1Loop
  LDX #$08
    .ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL button2     ; bit0 <- Carry
  DEX
  BNE .ReadController2Loop
  RTS

Movement:  ;;input->sprite movement
;;p1
  ;;--right component
  LDA button1
  AND #%00000001
  BEQ .ReadRDone
  LDA player1x
  CLC
  ADC #$01
  STA player1x
    .ReadRDone:
  ;;--left component
  LDA button1
  AND #%00000010
  BEQ .ReadLDone
  LDA player1x
  SEC 
  SBC #$01
  STA player1x
    .ReadLDone:
  ;;--down
  LDA button1
  AND #%00000100
  BEQ .ReadDDone
  LDA player1y
  CLC
  ADC #$01
  STA player1y
    .ReadDDone:
  ;;--up
  LDA button1
  AND #%00001000
  BEQ .ReadUDone
  LDA player1y
  SEC 
  SBC #$01
  STA player1y
    .ReadUDone:
  
    .MoveSprites: ;;8 tile player sprite
  CLC
  LDA player1y
  ;;top
  STA $0200
  STA $0204
  ;;midtop
  ADC #$08
  STA $0208
  STA $020C
  ;;midbot
  ADC #$08
  STA $0210
  STA $0214
  ;;lowbot
  ADC #$08
  STA $0218
  STA $021C
  CLC
  LDA player1x
  ;;left
  STA $0203
  STA $020B
  STA $0213
  STA $021B
  ADC #$08
  ;;right
  STA $0207
  STA $020F
  STA $0217
  STA $021F


  RTS  ;;END Movement sr
 
;;
;;;;;;;;;;;;;;;-END BANK0

;;;;;;;;;;;;;;;-BANK1
;;
  .bank 1
  .org $E000
palette: ;$3F00-$3F0F =bg, $3F10-$3F1F=sprite
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$37,$01,$18,  $22,$37,$30,$0F,  $22,$37,$0C,$0F,  $21,$25,$38,$04   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $70, $00, $00, $80   ;sprite 0
  .db $70, $01, $00, $88   ;sprite 1
  .db $78, $02, $01, $80   ;sprite 2
  .db $78, $03, $01, $88   ;sprite 3
  .db $80, $04, $02, $80   ;sprite 2
  .db $80, $05, $02, $88   ;sprite 3
  .db $88, $06, $02, $80   ;sprite 2
  .db $88, $07, $02, $88   ;sprite 3


background: ; 
  .db $08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08
  .db $09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09
  
  .db $0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A
  .db $09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09
  
  .db $0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C
  .db $08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08
  
  .db $09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09
  .db $0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A,$09,$0C,$08,$09,$0A

attribute:
  .db %00100000, %01010000, %01000001, %01010000, %00010000, %01010000, %00010000, %01010000

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used
  
  
;;;;;;;;;;;;;;

  
  
  .bank 2
  .org $0000
  .incbin "mariohack.chr"   ;includes 8KB graphics file from SMB1