# c64space_scrolling_game
Pixel scrolling space game demo

;***********************************************************************
;Spaced-Out 
;Original version: 1991
;Revised 2015
;By Steve Morrow
;All rights reserved
;***********************************************************************
;Sprite Detection: 10/4/15
;=======================================================================

PRINT     = 65490

;sprite declarations
sprx      = 53248                  ;SPRITE'S X POSITION
spry      = 53249                  ;SPRITE'S Y POSITION
SPRY2     = 53251
SPREN     = 53269                  ;SPRITE ENABLED
SPRPT     = 2040                   ;SPRITE POINTER
SPRC0     = 53287                  ;SPRITE COLOR
SPRME     = 53276                  ;SPRITE MULTI ENABLE
SPRML     = 53285                  ;SPRITE MULTICOLOR
COLOR     = 53280                  ;SCREEN BORDER COLOR
joy       = 56320                  ;JOYSTICK ROUTINE
border    = $d020

;most significant byte
msbflag   = 0
msbflag2  = 0

;shoot animation
;declarations
pointer   = 41
pointerlmt = 7

;raster initialization
;declarations 
irqrout   = $ea31
irqvec    = $314
number    = 30
PNT       = 820
enappeartm = 821
PNT2      = 822

;alien initialization
;declarations
flipflag  = 823
transport = 824
pausealien = 750

;room/level data
roomsave  = 688

;raster stuff (aliens)
aliendelay = 0
rasterdelay = 0
alienhitpoints = 686
alien_ai  = 0

;scrolling data
;scrollmap = 766         ;
playerdistance = 689    ;how far player walks in map             
distancewait = 763      ;inner loop for playerdistance (slow it down)
scrollmapvert = 762
newvertmapdata = 2040

scrollthis = 692
mapscroll_lmt = 693

;screen setup (title screen)
viewscreen = 694

;Sprite/background detection
y40_LO    = 680
y40_HI    = 681
y8_LO  = 682
y8_HI     = 683
y32_LO    = 684
y32_HI    = 685
ZPTempLO = 253
ZPTempHi  = 254
isladder =      687

VIC_MEM         = 53248
SCREEN_BORDER   = VIC_MEM + 32
SCREEN_BG_COLOR = VIC_MEM + 33
COLOR_BLACK     = $00
COLOR_WHITE     = $01
COLOR_RED       = $02
COLOR_CYAN      = $03
COLOR_MAGENTA   = $04
COLOR_GREEN     = $05
COLOR_BLUE      = $06
COLOR_YELLOW    = $07
COLOR_ORANGE    = $08
COLOR_BROWN     = $09
COLOR_PINK      = $0a
COLOR_DARK_GREY = $0b
COLOR_GREY      = $0c
COLOR_L_GREEN = $0d

; 10 SYS (7000)

*=$801

        BYTE    $0E, $08, $0A, $00, $9E, $20, $28,  $37, $30, $30, $30, $29, $00, $00, $00

COLOR_L_GREY = $0f


*         = $1b58


start   
          jsr            CLRMEM
                                   ;Main variables initialization
                                   ;MAIN
          lda            #$40       ;44,76,108,124 (114)
          sta            $d018
          lda            #0        ;unused - we will use this to track
          sta            enappeartm;an enemy's moving position
          sta            charmovedelay
          sta            $dd04
          sta            roomsave  ;start at room=0
          sta            playerdistance
          sta            distancewait
          sta            691
          lda            #2        ;POKE 53280,2 - RED
          sta            COLOR     ;53280 (BORDER COLOR)
          lda            #0
          sta            COLOR+1   ;screen color
          lda            $d016     ;Get multicolor mode
          ora            #$08        ;set bit 3
          ora            #$10       ;set bit 4
          sta            $d016     ;store result
          lda            #$06        ;change background color 1 (3,4)
          sta            $d022     ;for sprite
          lda            #$01
          sta            $d02a     ;sprite 3 color
          lda            #7
          sta            SPREN     ;enable sprites
          ldx            roomsave
          lda            alny1positions,x
          sta            $d003     ;sprite 1 enemy y position (alien 1)
          lda            alnx2positions,x
          sta            $d004     ;sprite 2 enemy x position
          lda            alny2positions,x
          sta            $d005     ;sprite 2 enemy y position (alien 2)
          lda            aliensize,x
          and            $d017
          sta            $d017
          lda            #$e       ;background multicolor col
          sta            $d023
          lda            #$b
          sta            $d024
          lda            #$07
          sta            SPRME     ;enable sprite multicolors for 7 sprites
          lda            #$02
          sta            SPRML+3

          lda            #$03
          sta            alienhitpoints
          lda            #$21
          sta            PNT

          lda            #0
          sta            scrollmap
          lda            #$ff      
          sta            isladder  

          ;Start title screen animation
          ;jsr titlescreen
          ;42 - phaser facing right
          ;43 - phaser facing left
          ;44 - gun facing right
          ;45 - gun facing left
          ;46 - alien ship
          ;47 - alien (facing right)
          ;49 - alien (facing right ani2)
          ;50 - alien (facing left)
          ;51 - alien (facing left ani2)
          ;52 - alien 2 (facing right)
          ;53 - alien 2 (facing right ani2)
          ;Create sprites
          
          jsr            clearcharsprite
          ldx            #62            ;draw sprite data from 0-62
DWN1      lda            charsprite,X
          sta            2112,X
          lda            charsprite+64,X
          sta            2176,X
          lda            charsprite+128,X
          sta            2240,X
          lda            charsprite+192,X
          sta            2304,X
          lda            charsprite+256,X
          sta            2368,X
          lda            charsprite+320,X
          sta            2432,X
          lda            charsprite+384,X
          sta            2496,X
          lda            charsprite+448,X
          sta            2560,X
          lda            charsprite+512,X
          sta            2624,X
          lda            charsprite+576,X
          sta            2688,X
          lda            charsprite+640,X
          sta            2752,X
          lda            charsprite+704,X
          sta            2816,X
          lda            charsprite+768,X
          sta            2880,X
          lda            charsprite+832,X
          sta            2944,X
          lda            charsprite+896,X               ;960
          sta            3008,X
          lda            charsprite+960,X              ;1024
          sta            3072,X    
          lda            charsprite+1024,X              ;1088
          sta            3136,X    
          lda            charsprite+1088,X              ;1152
          sta            3200,X    
          lda            charsprite+1152,X              ;1216
          sta            3264,X    
          dex
          bne            DWN1
          lda            #62
          sta            PNT2      

          jsr            clearsprites
 
          ldx            #62
drwalien
          lda            aliensprite,x          ;53     3392   
          sta            3328,x
          lda            aliensprite+64,x          ;53     3392   
          sta            3392,x
          lda            aliensprite+128,x       ;54     3456    
          sta            3456,X
          lda            aliensprite+192,x      ;55     3520
          sta            3520,X
          lda            aliensprite+256,X      ;56     3584
          sta            3584,X
          lda            aliensprite+320,X      ;57     3648
          sta            3648,X
          lda            aliensprite+384,X      ;58     3712
          sta            3712,X
          lda            aliensprite+448,X      ;59     3776
          sta            3776,X
          lda            aliensprite+512,X      ;60     3840
          sta            3840,X
          lda            aliensprite+576,X      ;61     3904
          sta            3904,X
          lda            aliensprite+640,X      ;62     3968
          sta            3968,X
          lda            aliensprite+704,x      ;78     4992
          sta            4032,X    
          lda            aliensprite+768,x          ;53     3392   
          sta            832,x
          lda            aliensprite+832,X      ;       4096
          sta            896,X         
          lda            aliensprite+896,X      ;       4160
          sta            960,X
          dex
          bne            drwalien  

          lda            #6        ;change background color 1 (3,4)
          sta            53282     ;for sprite
          lda            #13
          sta            53290     ;sprite 3 color
          lda            #14       ;background multicolor col
          sta            53283
          lda            #11
          sta            53284     
          lda            #205
          sta            sprx      ;player 1 sprite x position
          sta            x1
          lda            #108
          sta            spry      ;player 1 sprite y position
          sta            y1   
                     
          jsr            clearscreen
          jsr            resetcolors
          jsr            newcharset1   
          ;jsr            game_msg_display 
          jsr            clearchsetbk
          ;jsr            fullscreenmap
          lda            #1        
          sta            viewscreen
          jsr            resetmapdata
          lda            #39       
          sta            scrollthis
startbars
          sei
          lda            #$7f
          ldx            #$01
          sta            $dc0d    ; Turn off CIA 1 interrupts
          sta            $dd0d     ; Turn off CIA 2 interrupts
          stx            $d01a     ;enable raster interrupts 

          asl            $d019     

          lda            #$1b      
          ldx            #$08      
          ldy            #$14      
          
          sta            $d011     
          stx            $d016     
          sty            $d018  

          jsr            resetmapdata
        
          lda            #<joyst     ;<alienani   
          ldx            #>joyst   ;>alienani

          ldy            $0801     
          sta            $0314     
          stx            $0315     
          sty            $d012     

          lda            $dc0d ; ACK CIA 1 interrupts      
          lda            $dd0d ; ACK CIA 2 interrupts
          asl            $d019 ; ACK VIC interrupts
          cli 
          rti

ever1
          jmp            ever1      ;(Can be replaced with rts)  


;coltbl = color table
                                   
;scanline = used for raster interrupts

coltbl         byte 45,75,100,130,225
scanline       byte 100,101,102,103,104
                                   
;alienmoveflag = flying saucer                                   
;alien2moveflag = dragon alien/snake                                   
;0 - gets the alien moving
alien1moveflag          byte 0,0,0,3,0,5 

;Compares against the value found in playerdistance
;playerdistance>5
;playerdistance>124
;playerdistance>135
alien2moveflag          byte 105,118,235,50,50,50,50

                                   
;Move interrupt routine

alienani
          tax
          tay
          cld
          sta            savea
          stx            savex
          sty            savey
          inc            $d019     
          lda            #$34           ;52
rastaln1  cmp            53266   ;$d012 - wait until raster line number 53 is being scanned
          bne            rastaln1
          jsr            rasteraliens

          lda            #230
          sta            $d012
          lda            #<joyst
          sta            $fffe
          lda            #>joyst
          sta            $ffff
          lda            savea
          ldx            savex
          ldy            savey  
          
          jmp            $ea81 
         
rasteraliens   
          sta            savea
          stx            savex
          sty            savey
          asl            $d019

; The table showalientbl will wait for the player
; to walk so far and then show new aliens on the screen
          ;jsr            game_msg_display
          lda            $d016          ;expand border to     
          and            #$f7           ;to 38-column mode
          sta            $d016
          ldx            roomsave
          lda            showmsg      
          cmp            #1             ;show alien commander
          bcs            showgame_mes1   
          jmp            noalienonscreen   
showgame_mes1  
          jmp            alienmoving

alienmoving
          jsr            reversealien
noalienonscreen 


;********************* RASTERS HERE *********************************

finsplit  
          inc            53254     ;move spaceship to the right
          inc            53254     ;ditto
dalien    

nxtalien
          inc            pausealien         
          lda            pausealien        ;slow down the animation
          cmp            #15               ;so we can see it
          bcc            skipcol           ;skip over if loop <15
          lda            #0        
          sta            pausealien        ;reset loop
                                    
          jsr            mvalien   ;move alien dragon
skipcol    

mainlp     
          rts

          ;if roomsave = alienalivestatus
          ;then that alien is still alive
          ;in that room.
          ;Any other non-matching room number
          ;means they are dead.

alienalivestatus        byte           0,1,2,3,4,1 
          ;34 - player facing still to right
          ;35 - player walk(1) to left                              
          ;36 - player standing still to left
          ;37 - player jumping to right
          ;38 - player falling to right (gun poised)
          ;39 - player double sided gun position
          ;40 - player laying down (dead) facing right
          ;41 - player gun (1) facing right
          ;42 - player shooting phaser (1) facing right
          ;43 - player shooting phaser (2) facing right
          ;44 - player gun poised facing left
          ;45 - player shooting phaser (1) facing left
          ;46 - player shooting phaser (2) facing left
          ;47 - player climbing facing right

          ;52-53 = alien dragon left
          ;54-55 = alien dragon right
          ;56-57 = snake alien left
          ;58-59 = snake alien right
          ;60 61 - alien commander facing left (1)
          ;62-63 - alien commander facing left (2)

resetcolors
          lda            #35       ;sprite shape data
          sta            SPRPT     ;for player 0 (our player)
          lda            #65
          sta            SPRPT+1
          lda            #62
          sta            SPRPT+2
          lda            #7        ;player face color
          sta            SPRML     ;sprite multicolor register 0
          lda            #13
          sta            53286     ;sprite multicolor register 1
          lda            #11
          sta            53287
          lda            #2
          sta            53288     ;sprite 1 color
          lda            #3
          sta            53289     ;sprite 2 color
          rts

          ;48-49 = alien dragon left
          ;50-51 = alien dragon right
          ;52-53 = snake alien left
          ;54-55 = snake alien right
          ;show animation stages for alien 1 & 2
          ;based on what room they are in
          ;each byte is a different alien shape
          ;change from top to botto`m (50,51)
          ;48 - alien dragon facing left
          ;49 - alien dragon facing right

alienfacergt1   byte 62,62,58,54,62,54
alienfacergt2   byte 63,63,59,55,63,55 

alienfacelft1   byte 60,60,56,52,60,52
alienfacelft2   byte 61,61,57,53,61,53
                                   ;vertical position size (53271)
aliensize      byte 0,0,0,0,0,0,0,0,0,0
          ;Make alien(s). Flip a direction after they reach any corner
          ;change the shape data and create animation frames for each
          ;alien sprite.
mvalien
          ldx            roomsave
          lda            aliensize,x
          sta            53271     
          lda            flipalien ;the flag is set if the alien(s)
          cmp            #0        
          beq            changealienshaperight
          cmp            #1        ;have reached the right edge
          beq            reversealien;then move him to the left
          jsr            changealienshapeleft

; Code below indicates when the commander alien changes
          rts
;Check if the alien commander has walked to the far right.
;Once he has reached the edge (when 53252 > 254) then
;get him moving to the left.
;Once he reaches the right edge, flip him to the other 
;direction (flipalien = 0)

reversealien
          lda            53252     
          cmp            #254      
          bcs            move_alienlft
          lda            flipalien 
          cmp            #1        
          beq            move_alienlft
          inc            53252     ;move commander,snake,or dragon    
          rts
move_alienlft
          jsr            changealienshapeleft
          lda            #1        ;this tells us that the alien
          sta            flipalien ;is now moving to the left
          lda            53252
          cmp            #60       ;if he reached the left border then flip
          bcc            mvaln     ;flipalien back to zero so he moves right again
          dec            53252     ;move alien to the left
          rts
                                   ;right alien movement - this changes the animation frames
changealienshaperight   
          ldx            roomsave
          lda            PNT2      
          inc            PNT2      ;check alien shape pointer
          cmp            alienfacergt2,x;if the shape is greater or equal 54
          bcs            resetalnpntright;then reset the shape pointer
          lda            PNT2
          sta            2042      
          rts
resetalnpntright
          ldx            roomsave
          lda            alienfacergt1,x
          sta            PNT2      ;data to first data pointer
          sta            2042   ;save in shape data
exitwlk   rts
                                   ;left alien movement - this changes the animation frames
changealienshapeleft
          ldx            roomsave
          lda            PNT2
          inc            PNT2      ;check alien shape pointer
          cmp            alienfacelft2,x;if the shape is greater or equal 54
          bcs            resetalnpntleft;then reset the shape pointer
          lda            PNT2      
          sta            2042
          rts
resetalnpntleft
          ldx            roomsave
          lda            alienfacelft1,x
          sta            PNT2      ;data to first data pointer
          sta            2042   ;save in shape data
          rts
mvaln     lda            #0
          sta            flipalien
          rts

spritecollision
          lda            53278
          and            #1
          beq            exitcol
exitcol
          lda            #0
          sta            53278
endrt     rts

          ;once player exits the screen, he reappears on lower level
          ;and background screen reloads (gmap1-gmap4 - currently)
          ;48-49 = alien dragon left
          ;50-51 = alien dragon right
          ;52-53 = snake alien left
          ;54-55 = snake alien right
          ;chkrmforaliens = check which room has an alien
          ;getshapeforalien1 = shows alien shape data (2041) - saucer
          ;getshapeforalien2 = shows alien shape data (2042) - alien dragon
          ;alnx1positions,alny1positions - saucer alien (x,y) position (53250,53251)
          ;alnx2positions,alny2positions - dragon alien (x,y) position (53252,53253)

chkrmforaliens byte 0,1,2,3,4,5,0
getshapeforalien1 byte 63,60,47,56,48,48,53
getshapeforalien2 byte 56,62,56,50,50,50,50

alnx1positions byte 50,50,150,150,210,240 
alny1positions byte 50,50,150,70,0,0
alnx2positions byte 0,90,100,150,210,240
alny2positions byte 70,70,165,150,170,100 


checkalien_ai
          lda            54299     ;get random generator
          and            #55       ;masked 200
          bne            endchkaln    
          rts
endchkaln
          lda            #0
          sta            54299 
          lda            #1        
          sta            pausealien
          inc            pausealien
          rts

evalroom        
          ldx            roomsave       ;in room 0, 1, 2, 3 , etc?
          lda            roomsave
          cmp            chkrmforaliens,x;check which alien is moving
          beq            position_aliens   ;0 - no moving alien
          rts

uplevel
          lda            #0
          sta            53264
          inc            roomsave       ;increment room or room=room+1
          cli
          rti
position_aliens
          ldx            roomsave
          lda            alnx2positions,x
          sta            53252
          lda            alny2positions,x
          sta            53253     
          rts

stophere1
          jmp            stophere1
          rti
stophere2
          jmp            stophere2

downlevel
          lda            $d010     ;get msb
          ora            #1
          sta            $d010
          dec            roomsave  ;increment room or room=room+1
          lda            roomsave
          cli
          rti 

rightedge                ;moved to the right
          lda            #1
          sta            flipflag
          sta            msbflag2
          lda            #0
          sta            msbflag
          lda            #10
          sta            53248
                                   ;jsr chktransport
          jsr            uplevel   ;go to level after player exits screen 1
fliprgtmv
          cli
          rti
                                   ;Routine returned from controller
leftedge                           ;moved to the left
          lda            #0
          sta            flipflag
          sta            msbflag2
          lda            #1
          sta            53264
          sta            msbflag
          lda            #255
          sta            53248
                                   ;jsr chktransport
          jsr            downlevel
          cli
          rti

r2_setraster3
          sta            savea
          stx            savex
          sty            savey
          lda            #56
          sta            $d012
          lda            #<scrollcol
          sta            $fffe
          lda            #>scrollcol
          sta            $ffff     
          lda            savea
          ldx            savex
          ldy            savey               
          rts

scrollcol
          cld
          sta            savea
          stx            savex
          sty            savey
          inc            $d019     
          inc            53249
          rts

r2_setraster0 
          sta            savea
          stx            savex
          sty            savey
          lda            #$34
          sta            $d012
          lda            #<alienani
          sta            $fffe
          lda            #>alienani
          sta            $ffff
          lda            savea
          ldx            savex
          ldy            savey     
          rts



golft     jmp            lft
mvpdn     jmp            mvdn
mvpup     jmp            mvup 

plrjmp    jsr           playershoot     ;playerjump
          rts

exit_int 

clearint    
          pla     
          tay
          pla
          tax
          pla
          rti                      ; return from interrupt 
          rts

;**********************************************************************
;                          JOYSTICK ROUTINE
;**********************************************************************

joyst

rastbars
          asl            53273
          ldx            #$00
          lda            #230
startrast
          cmp            53266      ;$d012
          bne            startrast 
          lda            53272     
          pha
          nop
          nop
          nop
          nop
          nop
          nop
          nop
          nop
loop
          ldy            timer,x
          lda            #21
          sta            53272     
          lda            #32       
          sta            53270
delay
;          dey
;          bne            delay
;          inx
;          cpx            #24
;          bne            loop      
          lda            #28       
          sta            53272  
          pla
          sta            53270

joystick
          asl            $d019    ; ACK interrupt (to re-enable it) 
          lda            joy       ;get joystick
          and            #4        ;check it for bit 4
          beq            golft     ;move player to the left
          lda            joy
          and            #8        ;check it for bit 8
          beq            rgt       ;move player to the right
          lda            joy
          and            #2        ;check it for bit 2
          beq            mvpdn     ;move player down
          lda            joy
          and            #1        ;check it for bit 1
          beq            mvpup     ;move player up
          ;lda            joy
          ;and            #16
          ;beq            chk_belowchar           
          lda            isladder  
          cmp            #3                     ;player touched platform
          beq            skip_fall_idle;and won't fall
          cmp            #31       
          bne            skip_fall_idle
          jsr            falling   
skip_fall_idle

          jmp            exit_int

;Right joystick routine
rgt

          lda            #39            ;192+7*256 = 1984            
          sta            251            ;39+4*256 = 1063
          lda            #4            
          sta            252       
                                   
          jsr            pixelscrollright
          sta            53280     
          lda            isladder
          cmp            #19                  
          beq            skip_falling 
          cmp            #31       
          beq            skip_falling
          cmp            #3        
          beq            skip_fall_rt
          cmp            #0                     ;if touching nothing
          bne            skip_fall_rt;player will fall
          cmp            #1        
          beq            mvplayer_up
          jsr            falling   
skip_fall_rt 
          sta            53280     
          lda            #1        
          sta            unlockdown
          sta            unlockup  
skip_falling
          jmp            cont_up
mvplayer_up
          dec            $d001     
          dec            $d001     
          dec            $d001     
cont_up
          ldx            #0
          stx            pointer
          ldx            #3
          stx            pointerlmt
          lda            #0
          sta            198
          LDA            PNT
          STA            SPRPT     
                                   ;Create right animation frames for player
anirgt
          inc            charmovedelay
          lda            charmovedelay
          cmp            #5       ;if timer is not yet 40 secs
          bcc            xitrgt    ;then don't move the alien
          lda            #0
          sta            charmovedelay
          LDA            PNT
          STA            SPRPT
          INC            PNT       ;increment through sprite shapes
          LDA            PNT       ;check to see if we cycled
          CMP            #35       ;35 - through all the frames
          BCS            RSETPT    ;if so, reset them
                                   ;cli
                                   ;rti

xitrgt    jmp            exit_int
          rts
RSETPT
          LDA            #33       ;36 - reset sprite 0 shape
          STA            PNT       ;data to first data pointer
          STA            SPRPT     ;save in shape data
                                   ;cli
          jmp            exit_int  
          jmp            $ea31
          rts
msbactive
          inc            x1
          lda            #1
          sta            msbflag
          sta            53264
          lda            53264
          cmp            #0
          beq            skipovr
          lda            x1
          cmp            #135
          bcs            rgtedge   
skipovr   jmp            exit_int
          rts
rgtedge   jsr            rightedge

          jmp            exit_int  
          rts

;Next
;Position map data to align correctly with 
;top/bottom of gmap so when the screen scrolls
;up or down it is aligned with the next 
;level data (above or below)

mvdn
          jsr            pixelscrolldown
          jmp            exit_int
          lda            isladder  
          cmp            #4       
          beq            unl_dn
          cmp            #19 
          beq            unl_dn    
          cmp            #0        
          beq            unl_dn
          cmp            #13       
          beq            down_ladder
          cmp            #14       
          beq            down_ladder 
          cmp            #11       
          bne            exitdn
          cmp            #19        
          beq            unl_dn    
          sta            53280 
          jmp            unl_dn    
down_ladder
          jsr            sprite_aniup
unl_dn  
          lda            #1        
          sta            unlockdown
          sta            unlockup  
          jmp            exit_int
skplocdn
          jmp            exit_int
          rts
   
          inc            y1
          lda            y1
          clc
          adc            #160        
          sta            vertposbkgnd
          inc            $d001   
          jmp            exit_int
          rts
          jmp            exit_int  
          rts
exitdn    
extdn
          rts
          jmp            exit_int
          rts
mvup
          lda            691
          cmp            #1
          beq            skip_upinit
          lda            #1        
          sta            691       
skip_upinit
          jsr            pixelscrollup
          lda            isladder  
          cmp            #13            ;can always climb       
          beq            unl_up         ;up any ladder
          cmp            #14       
          beq            unl_up 
          cmp            #19            ;floor top        
          beq            exit_up  
          cmp            #4             ;floor center    
          beq            unl_up    
          cmp            #1        
          beq            unl_up 
unl_up
          jmp            exit_int
          rts
          lda            #1        
          sta            unlockdown
          sta            unlockup  
          dec            y1        
          lda            y1
          sec
          sbc            #8        
          sta            vertposbkgnd
exit_up
          jmp            exit_int
exitup
           
          jmp            exit_int  
          rts

sprite_aniup
          inc            PNT
          lda            PNT
          sta            SPRPT
          cmp            #48       
          bcc            ani_exit_up  
          lda            #47  
          sta            PNT       
          sta            SPRPT
ani_exit_up
          rts

sprite_anidown
          dec            PNT
          lda            PNT
          cmp            #47       
          bcc            ani_exit_dwn  
          lda            #48  
          sta            PNT       
ani_exit_dwn
          rts
        

;Scroll up is cut off for now
exitscup  jmp            exit_int
extup     rts

RSETPTDN
          lda            #49       
          sta            PNT   
          sta            2040      
          jmp            exit_int
          rts

lftedge   jsr            leftedge
          rts

;************** Left move routine ********************
lft
          jsr            pixelscrollleft
          lda            isladder  
          cmp            #0       
          bne            skip_fall_lf
          jsr            falling   
skip_fall_lf
          ldx            #3
          stx            pointer
          ldx            #5
          stx            pointerlmt
          LDA            PNT
          STA            SPRPT
          lda            #0        ;reset clock to zero
          sta            enappeartm;after 15 ticks have passed
chklflip
                                   ;Create left animation frames for player
anirlf
          inc            charmovedelay
          lda            charmovedelay
          cmp            #10       ;if timer is not yet 40 secs
          bcc            xitlft    ;then don't move the alien
          lda            #0
          sta            charmovedelay
          LDA            PNT
          STA            SPRPT     ;change shape data
          INC            PNT
          LDA            PNT
          CMP            #37
          BCS            RSETPTL
xitlft    jmp            exit_int
          rts
RSETPTL
          LDA            #35
          STA            PNT
          STA            SPRPT
          jmp            exit_int                        
          rts
flipmsb
          lda            #0
          sta            53264
          lda            #255
          sta            53248
          sta            msbflag2  
          jmp            exit_int
          rts

sprite_bkcolis
          rts
          lda            #0    
          sta            53280 

;Detect ASCII screen memory - this may be off by + or - 32
          lda            #0        
          sta            253       
          lda            #4       
          sta            254       

;*************************************************
;                   x1: screen_tempx
;*************************************************
          ;sed
          lda            x1        
          sec
          sbc            #24  
          lsr
          lsr
          lsr
          sta            xchar     ;XC=(x1-24)/8  

;*************************************************
;                   y1
;*************************************************
          lda            y1      
          sec
          sbc            #21       
          lsr
          lsr
          lsr
          sec
          sbc            #5
          sta            ychar     ;YC=(y1-21)/8 (011)


          lda            #0
          sta            y32_HI    

;==================================================================
;                       11*8=88
;================================================================== 
          lda            ychar                ;011 - $2a8
          asl                                 ;11x2 = 2  (11*2=22)
          rol            y32_HI       

          asl                                 ;2x2 = 4  (11*4=44)
          rol            y32_HI    

          asl                                 ;4x2 = 8  (11*8=88)
          rol            y32_HI    

          asl                                 ;8x2 = 16 (11*16=176)
          rol            y32_HI    

          asl                                 ;16x2 = 32 (11*32=352)
          rol            y32_HI     
          sta            y32_LO               ;YC*32

;==================================================================
;                       ychar * 8
;==================================================================

          lda            #0        
          sta            y8_HI  
          lda            ychar     
          asl
          rol            y8_HI                ;x2
          asl
          rol            y8_HI                ;x4
          asl
          rol            y8_HI                ;x8
          sta            y8_LO                ;YC*8

;==================================================================
;                       ychar * 40
;==================================================================
          clc
          lda            y32_LO    
          adc            y8_LO     
          sta            y40_LO    
          lda            y32_HI    
          adc            y8_HI     
          sta            y40_HI    

;==================================================================
;                       ychar * 40 + Screen
;==================================================================
          clc
          lda            y40_LO    
          adc            253       
          sta            253       
          lda            y40_HI    
          adc            254       
          sta            254

;=================================================================         

;************   Start Y index search   ***************************
                      
resety
          ldy            xchar

chk_ladr_range

;*****************************************************************
;                 Peek into 1024+xchar+ychar
;*****************************************************************
          lda            (ZPTempLO),y           ;253
          sta            isladder  
          sta            1890      
skp
          lda            #0        
          sta            unlockdown
          sta            unlockup      

          lda            (253),y
          sta            numsave   
          jsr            writenum      
          rts

cont_ladr_scan

;Check if player is still falling
falling
          lda            #7        
          sta            53280
          inc            y1   
          lda            y1
          sta            53249     
          jsr            fall_wait 
          lda            #1        
          sta            gravity
at_bottm  rts

fall_wait
          lda            falldelay 
          cmp            #100       
          bcs            fdelayfin 
          inc            falldelay 
          rts
fdelayfin
          lda            #0        
          sta            falldelay 
          rts

on_floor
          lda            #2        
          sta            53280     
          lda            #0
          sta            gravity

climb_ladder
          cld
          lda            #13
          sta            53280     
          lda            #1
          sta            unlockdown
          sta            unlockup
          rts

ladchr          byte           13,14     ;40,41  
floortile       byte           19,20

findhallway
          lda            $d004     
          sec
          sbc            #16       
          lsr
          lsr
          lsr
          sta            xladder  
          lda            $d01f     
          and            #4        
          bne            noladderfnd
;The ladder was found!
          lda            #7        
          sta            $d020     
          sta            $d021         
          rts
noladderfnd
clrcol
          lda            #0        
          sta            unlockdown
          sta            unlockup
          sta            $d01f     
          sta            $d020     
          sta            $d021  
          rts

clearsound
          ldx            #24
clrsnd    lda            #0
          sta            54272,x
          dex
          bne            clrsnd
          rts
lasersound
          ;$DE00
          ;54276
          ;Triangle = 16
          ;Sawtooth = 33
          ;Pulse = 64
          ;Random noise = 129
          ;Noise waveform = 129 (Bit 7) - spaceship sound
         
          lda            #48
          sta            54272     ;s
          lda            #4
          sta            54273     ;s+1
          lda            #130      ;voice 1 frequency control (low byte)
          sta            54273
          lda            #0
          sta            54274     ;voice 1 pulse waveform (low nybble)
          lda            #8
          sta            54275     ;voice 1 pulse waveform (high nybble)
          lda            #16
          sta            54276
          lda            #240
          sta            54278     ;voice 1 sustain/release control register
          lda            #8
          sta            54282
          lda            #65
          sta            54283
          lda            #255
          sta            54285
          lda            #7
          sta            54296     ;sound volume 0-1
                                   ;loop (delay)
          rts
          ldy            #60
snd0      ldx            #135
snd1
          sta            54279     ;filter cutoff frequency (high byte)
          stx            54292     ;voice 3 sustain/release control
          lda            sndbytes,x
          ora            #128
          sta            54272     ;voice 1 frequency control (low byte)
          stx            54273     ;voice 1 frequency control (high byte)
          lda            sndbytes,x
          clc
          adc            #255
          sta            54274     ;voice 1 pulse waveform (low nybble)
          asl
          sta            54275     ;voice 1 pulse waveform (high nybble)
          ora            #16
          sta            54300     ;envelope generator 3 output
          dex
          stx            54296     ;volume
          bne            snd1
          lda            sndbytes,y
          ora            #16
          sta            54279     ;voice 2 frequency control (low byte)
          sta            54280     ;voice 2 frequency control (high byte
          lda            sndbytes,y
          ora            #2
          sty            54284     ;voice 2 attack/decay register
          lda            sndbytes,y
          sta            54285     ;voice 2 sustain/release control
          sta            54286     ;voice 3 frequency control (low byte)
          ora            #16
          sta            54287     ;voice 3 frequency control (high byte)
          lda            sndbytes,y
          ora            #64
          sta            54288     ;voice 3 pulse waveform width (low byte)
          sta            54289     ;voice 3 pulse waveform width (high byte)
          lda            #33
          sta            54290     ;voice 3 control register
          sta            54291     ;voice 3 attack/decay register
          lda            sndbytes,y
          sta            54291     ;voice 3 attack/decay register
          lda            sndbytes,x
          ora            #128
          sta            54293     ;bits 0-2 low portion of filter cutoff freq
          sta            54294     ;filter cutoff frequency (high byte)
          dey
          sty            54296     ;volume
          bne            snd0
          lda            #0
          sta            54296     ;kill sound
          rts

sndbytes byte 20,40,50,70,80,30,45,55,75,85,95,115,120,120,140,150,170,180,130,145,155,175,185,195,115,120

playershoot
          ldx            pointer
shtani    lda            shoot_table,x
          cpx            pointerlmt
          beq            resetshpnt
          sta            2040
          inc            charmovedelay
          lda            charmovedelay
          cmp            #10       ;if timer is not yet 40 secs
          bcc            xitsht    ;then don't move the alien
          lda            #0
          sta            charmovedelay
          inc            pointer
          jsr            clearsound
          jsr            lasersound
xitsht
          lda            #35
          sta            2043
          inc            53254
          inc            53255
          rts

resetshpnt
          ldx            #0        ;player facing to right
          stx            pointer
          rts
rstlft    ldx            #3
          stx            pointer
          rts

shoot_table             byte           41,42,43,44,45,46 

CLRMEM
          lda            #64       ;(8*256)=2048+(64) = 2112
          sta            251
          lda            #8
          sta            252       
          ldx            #6
DY        ldy            #0
          lda            #0
DS        sta            (251),y
          iny
          bne            DS
          inc            252       
          dex
          bne            DY
          rts

clearsprites
          ldx            #64
clrhardsp
          lda            #0        
          sta            3072,x
          sta            3136,X
          sta            3200,X
          STA            3264,X
          STA            3328,X
          STA            3392,X
          STA            3456,X
          STA            3520,X
          sta            3584,x
          sta            3648,x
          sta            3712,x
          sta            3776,x
          sta            3840,x
          sta            3904,x    
          sta            3968,x    
          sta            4032,x
          dex
          bne            clrhardsp
          rts

clearcharsprite
          ldx            #64   
clrhardcsp    
          lda            #0       
          STA            2112,X
          STA            2176,X
          STA            2240,X
          STA            2304,X
          STA            2368,X
          STA            2432,X
          STA            2496,X
          STA            2560,X
          STA            2624,X
          STA            2688,X
          STA            2752,X
          STA            2816,X
          STA            2880,X
          STA            2944,X
          STA            3008,X 
          STA            4032,X    
          STA            4096,X    
          STA            4160,X    
          STA            4224,X    
          STA            4288,X
          dex
          bne           clrhardcsp
          rts

newcharset1
          LDX            #0
rdc1      LDA            charset1,x
          STA            12288,x   ;0+48*256 = 12288
          INX
          CPX            #255
          BNE            rdc1      

          lda            #0
          sta            251
          lda            #48
          sta            252 

          ldx            #0        
          lda            charsetlo,x    ;charset1
          sta            253       

          lda            charsethi,x ;gmap1
          sta            254

          ldx            #3
cy2       ldy            #0
cs1       lda            (253),y        ;re
          sta            (251),y        ;read screen memory
          iny
          bne            cs1
          inc            252       
          inc            254
          dex
          bne            cy2
          rts

clearchsetbk
          ldx            #0    
clrbkgnd    
          lda            #0        
          sta            12544,x     
          inx
          bne            clrbkgnd  
          rts

resetmapdata
          ldx            #2                  
          lda            gscreenmaplo,x 
          sta            253
          lda            gscreenmaphi,x 
          sta            254       
          rts      

;Draw map across screen
fullscreenmap
          ldx            #0        
drawmap
          lda            gmap4,x
          sta            1024,x
          lda            gmap4+255,x
          sta            1064,x
          lda            gmap4+510,x
          sta            1104,x
          lda            gmap4+765,x
          sta            1144,x
          lda            gmap4+1020,x
          sta            1184,x
          lda            gmap4+1275,x
          sta            1224,x
          lda            gmap4+1530,x
          sta            1264,x
          lda            gmap4+1785,x
          sta            1304,x
          lda            gmap4+2040,x
          sta            1344,x
          lda            gmap4+2295,x
          sta            1384,x
          lda            gmap4+2550,x
          sta            1424,x
          lda            gmap4+2805,x
          sta            1464,x
          lda            gmap4+3060,x
          sta            1504,x
          lda            gmap4+3315,x
          sta            1544,x
          lda            gmap4+3570,x
          sta            1584,x
          lda            gmap4+3825,x
          sta            1624,x
          lda            gmap4+4080,x
          sta            1664,x
          lda            gmap4+4080,x
          sta            1703,x
          lda            gmap4+4335,x
          sta            1743,x    
          lda            gmap4+4590,x   
          sta            1783,x    
          lda            #13
          lda            55296,x
          sta            55297,x
          lda            55296+40,x
          sta            55297+40,x
          lda            55296+80,x
          sta            55297+80,x
          lda            55296+120,x
          sta            55297+120,x
          lda            55296+160,x
          sta            55297+160,x
          lda            55296+200,x
          sta            55297+200,x
          lda            55296+240,x
          sta            55297+240,x
          inx
          cpx            #40      
          bne            drwmap   
          rts

drwmap    jmp drawmap

;********************** PIXEL SCROLL RIGHT **********************
pixelscrollleft

          lda            $d016     
          and            #247      
          sta            $d016
     
;Player cannot walk off the map
;Keep him at position 41 since that is
;has far the screen draws (1024,x) when
;first displaying the map.

          lda            roomsave  
          bne            left_cont 
          lda            #0        
          sta            roomsave     
left_cont
                                   ;rts
;First create a delay from 0-3 (distancewait)
;After this is done then reset distancewait=0 (restart loop)
;and dec playerdistance.
;-----------------------------------------------------------
;Note: playerdistance keeps track of how far the player
;has walked before seeing a new alien.
;-----------------------------------------------------------
;Check for the correct room (roomsave) and then
;check the table value found in (alien2moveflag,x)
;to see how far the player has walked across the screen
;if the playerdistance < alien2moveflag,x (5, 230, 135)
;the skip over evalroom (which handles the new x,y position)
;of the aliens.

scrollmp_lf

;showalien=1 - alien commander is seen
;showalien=2 - snake alien is seen
;showalien=3 - dragon alien is seen

;This routine shows which alien appears
;based on the value found in showalien
;showalien = 1 - alien commander
;showalien = 2 - alien snake
;showalien = 3 - alien dragon

xit_lf 

checkforaliens_lf

checkplydis2

leftscroll
          lda            53270
          and            #248
          clc
          adc            scbyte         
          sta            53270
          inc            scbyte         ;scbyte=scbyte+1
          lda            scbyte
          cmp            #8             ;if scbyte=8 then reset scbyte=0
          beq            reset
          rts
reset
          lda            #0
          sta            scbyte
          lda            53270
          and            #248
          sta            53270
          jsr            charscroll
          rts
charscroll
          ldx            #38
looplft
          lda            1024,x
          sta            1025,x
          lda            1024+40,x
          sta            1025+40,x
          lda            1024+80,x
          sta            1025+80,x
          lda            1024+120,x
          sta            1025+120,x
          lda            1024+160,x
          sta            1025+160,x
          lda            1024+200,x
          sta            1025+200,x
          lda            1024+240,x
          sta            1025+240,x
          lda            1024+280,x
          sta            1025+280,x
          lda            1024+320,x
          sta            1025+320,x
          lda            1024+360,x
          sta            1025+360,x
          lda            1024+400,x
          sta            1025+400,x
          lda            1024+440,x
          sta            1025+440,x
          lda            1024+480,x
          sta            1025+480,x
          lda            1024+520,x
          sta            1025+520,x
          lda            1024+560,x
          sta            1025+560,x
          lda            1024+600,x
          sta            1025+600,x
          lda            1024+640,x
          sta            1025+640,x
          lda            1024+680,x
          sta            1025+680,x

leftdata
          lda            #1
          lda            55296,x
          sta            55297,x
          lda            55296+40,x
          sta            55297+40,x
          lda            55296+80,x
          sta            55297+80,x
          lda            55296+120,x
          sta            55297+120,x
          lda            55296+160,x
          sta            55297+160,x
          lda            55296+200,x
          sta            55297+200,x
          lda            55296+240,x
          sta            55297+240,x
          dex
          cpx            #255
          beq            scroll_left2
          jmp            loopleft
          jsr            scroll_left2
finsclf   rts
loopleft  jmp            looplft
                                    
scroll_left2
          dec            scrollthis

          ldx            #0
rd_lf_hi  
          ldy            scrollmap3   
rd_lf_low 
          lda            (253),y   ;gmap4,y
          ldy            #0
          sta            (251),y
          lda            251
          clc
          adc            #40
          sta            251

          lda            252
          adc            #0
          sta            252     

          lda            253 
          clc
          adc            #255
          sta            253

          lda            254
          adc            #0
          sta            254       
          inx
          cpx            #21       
          bne            rd_lf_hi
          rts
resetsclf
          rts

eint      rts

clearscreen
          lda            #0
          sta            251
          lda            #4
          sta            252
          ldx            #4
dy2       ldy            #0
ds1       lda            #32
          sta            (251),y
          iny
          bne            ds1
          inc            252
          dex
          bne            dy2
          rts

                                  
;********************** PIXEL SCROLL RIGHT **********************
pixelscrollright               

checkplydis                                     
          lda            $d016     
          and            #247      
          sta            $d016
scrollmp_rt

          ldy            #0
          inc            distancewait           ;disw=disw+1
          lda            distancewait           
          cmp            #3                     ;if disw>3
          bne            rightscroll            ;goto rightscroll
          lda            #0
          sta            distancewait
          inc            playerdistance         ;playd=playd+1
          ldx            roomsave
          lda            playerdistance         ;5,230,135
          cmp            alien2moveflag,x       ;playerdistance<135
          bcc            checkforaliens_rt      ;skip over alien switch

;showalien=1 - alien commander is seen  
;showalien=2 - snake alien is seen
;showalien=3 - dragon alien is seen
 
          inc            roomsave  
          jsr            evalroom
          ldx            roomsave              ;0,1,2,3,4 
          lda            showgamemsg,x         ;get 1,2,3, etc
          sta            showmsg
          lda            #0
          sta            playerdistance
          sta            distancewait
checkforaliens_rt

rightscroll
          lda            53270
          and            #248
          clc
          adc            scbyte
          sta            53270
          dec            scbyte         ;scbyte=scbyte-1
          lda            scbyte
          cmp            #255           ;if scbyte=255 then scbyte=7
          beq            reset_rg  
          rts
reset_rg
          lda            53270
          and            #248
          clc
          adc            #7
          sta            53270
          lda            #7
          sta            scbyte
          jsr            charscroll_rg
          rts
charscroll_rg
          lda            #6
          sta            679
          ldx            #0
          ldy            #0
looprg
          lda            1025,x
          sta            1024,x
          lda            1265,x
          sta            1264,x
          lda            1505,x
          sta            1504,x
          lda            1745,x
          sta            1744,x    

rightdata
          inx
          iny
          cpy            #39            ;get each line (x)
          bne            looprght
          inx                           ;go down a line (y)
          ldy            #0
          dec            679
          bne            looprght
          jsr            scroll_right2
          rts
looprght  jmp            looprg

chkalieninroom          byte 4,200,220,200,255
roomfound               byte 3,3,3,3,3,1 
showgamemsg             byte           0,1,2,3

                                   
scroll_right2        

;y - scroll through map data
;y will also increment screen position (causing gaps)

;The goal is to scroll through y data
;while keeping sta (251),y stable     


;-----------between here--------------------

reset_mpdata

          lda            #1        
          sta            scrolled_right

          lda            scrolled_down
          cmp            #1        
          beq            change_scrolldata1

          lda            scrolled_up
          cmp            #1        
          beq            change_scrolldata2
          jmp            scroll_default

;Keep track of up/down data
;since the screen shifted to the right

scroll_default
         jsr            resetmapdata


change_scrolldata1

         lda            scrolldown_low
         sta            253
         lda            scrolldown_hi
         sta            254

;scrolldown_low and scrolldown_hi work fine

change_scrolldata2   

skip_newscrndn       
          ldx            #0
rd_rt_hi   
          ldy            scrollmap3
rd_rt_low 
          lda            (253),y   ;gmap4,y
          ldy            #0
          sta            (251),y
          lda            251
          clc
          adc            #40
          sta            251       

          lda            252
          adc            #0
          sta            252     

          lda            253 
          clc
          adc            #255
          sta            253       
          sta            scrollright_low

          lda            254       
          adc            #0
          sta            254       
          sta            scrollright_hi
          inx
          cpx            #19       
          bne            rd_rt_hi  

          inc            scrollmap3
          rts     
;----------------------------------------
exit_scr_rt    

          rts
add40_map
          rts
          lda            253 
          clc
          adc            #255
          sta            253

          lda            254
          adc            #0
          sta            254       
          rts

calc_by_255
          rts
          lda            #0
          sta            y32_HI    

;==================================================================
;                       (scrollmap2)*8=88
;================================================================== 
          lda            movedown1char;011 - $2a8
             
          asl                      ;39x2 = 78,          78
          rol            y32_HI       
          asl                      ;39x4 = 156,         156
          rol            y32_HI    
          asl                      ;39x8 = 312,         56  (312-255)-1=56
          rol            y32_HI    

          asl                      ;39x16 = 624,        112 
          rol            y32_HI    

          asl                      ;39x32 = 1248,       224 
          rol            y32_HI    

;extra code
          asl
          rol            y32_HI    ;39x64
          sta            y32_LO    ;224

;==================================================================
;                       (251) * 8
;==================================================================
          lda            #0        
          sta            y8_HI       
          asl                      ;224x2 = 448         192
          rol            y8_HI                ;x2
          asl                      ;224x4 = 896,        128
          rol            y8_HI     ;x4
          asl                      ;224x8 = 1792        0
          rol            y8_HI     ;x8

;extra code
          asl                      ;x16
          rol            y8_HI      
          asl                      ;x32
          rol            y8_HI     
          asl                      ;x64
          rol            y8_HI     
          asl                      ;x128
          rol            y8_HI

          sta            y8_LO     ;movedown1char*8                

;==================================================================
;                       (251) * 40
;==================================================================
          clc
          lda            y32_LO                         ;224   
          adc            y8_LO                          ;0

          clc
          adc            #62

          sta            y40_LO                         ;224   

          lda            y32_HI                         ;4  
          adc            y8_HI     
          lda            y8_HI                          ;7
 
          adc            #1     
          sta            y40_HI    


;==================================================================
;                       (251) * 40 + Screen
;==================================================================
          lda            y40_LO                         ;224
          adc            253   
          sta            253                            ;7
       
          lda            y40_HI                         ;7  
          adc            254                            ;12 
          sta            254                            ;12    
          rts


;************************** PIXEL SCROLL DOWN ********************************
pixelscrolldown

;Initialize zero page map
          lda            #32            
          sta            251
          lda            #7            
          sta            252       ;sta 1824

;Start raster check

down0     lda            53266
          cmp            #252
          bne            down0

;Check how far map scrolls down
           
downscroll           
          lda            53265     
          and            #248           ;255-248=7 (start at 7)
          clc
          adc            scbyte
          sta            53265 
          dec            scbyte    
          lda            scbyte    
          cmp            #255        
          beq            reset_dn
          rts
                
reset_dn
          lda            #7      
          sta            scbyte    
          lda            53265     
          and            #248      
          sta            53265   
              
charscroll_dn
          lda #6
          sta            679   
          inc            mapscroll_range
          ldx            #0        
loopd1
          jsr            pxscrollup_disp

          inx
          cpx #39
          bne            lpdn     

;Index below map+255 to get new down scroll data

          lda            #1        
          sta            scrolled_down

          lda            scrolled_right
          cmp            #1        
          beq            change_scrolldata_dwn

          lda            scrolled_left
          cmp            #1        
          beq            change_scrolldata_dwn
          jmp            skip_newscrn_dwn

;Keep track of right/left data
;since the screen shifted down

change_scrolldata_dwn
          lda            scrolldown_low
          sta            253
          lda            scrolldown_hi
          sta            254


;Track data position (x,y) as the screen has scrolled down
;x - scrollmap3, y = 253,254
          lda            #1     
          sta            movedown1char  

nxt_chk2
                      
skip_newscrn_dwn

          ldx            #0
rd_dn_hi  ldy            scrollmap3    
rd_dn_low lda            (253),y        ;gmap4
          sta            (251),y        ;1024,x
          iny
          cpy            #40
          bne            rd_dn_low  

advance_dwn                       
          lda            253       
          clc
          adc            #255       
          sta            253 
          sta            scrolldown_low
       
          lda            254
          adc            #0        
          sta            scrolldown_hi
          inc            254       
          inx
nomapread

fin_dn    rts

lpdn      jmp            loopd1


pxscrollup_disp
          lda            1064,x    
          sta            1024,x
    
          lda            1104,x    
          sta            1064,x   
 
          lda            1144,x    
          sta            1104,x   
 
          lda            1184,x    
          sta            1144,x   
 
          lda            1224,x    
          sta            1184,x   
 
          lda            1264,x    
          sta            1224,x   
 
          lda            1304,x    
          sta            1264,x   
 
          lda            1344,x    
          sta            1304,x    

          lda            1384,x    
          sta            1344,x    

          lda            1424,x    
          sta            1384,x    

          lda            1464,x    
          sta            1424,x      
    
          lda            1504,x
          sta            1464,x    

          lda            1544,x
          sta            1504,x    

          lda            1584,x    
          sta            1544,x
        
          lda            1624,x    
          sta            1584,x 

          lda            1664,x    
          sta            1624,x    

          lda            1704,x    
          sta            1664,x    

          lda            1744,x    
          sta            1704,x    
        
          lda            1784,x    
          sta            1744,x    

          lda            1824,x    
          sta            1784,x    
          rts

;************************** PIXEL SCROLL UP **********************************
pixelscrollup

;Initialize zero page map
          lda            #0     ;0+4*256=1024           
          sta            251
          lda            #4        
          sta            252       ;sta 1984
         

up0       lda            53266
          cmp            #252
          bne            up0

;check if map scrolled to the top
          lda            scrollmap 
          cmp            #0
          bcs            upscroll  
          rts
           
upscroll              
          lda            53265     
          and            #248 
          clc
          adc            scbyte ;0-7 pixels
          sta            53265 
          inc            scbyte  ;scroll 1 pixel  
          lda            scbyte    
          cmp            #8        
          beq            reset_up
          rts
                
reset_up
          lda            #0        
          sta            scbyte    
          lda            53265     
          and            #248      
          sta            53265      
       
charscroll_up
          lda            #6        
          sta            679

;Index below map+255 to get new down scroll data

cont_mapscrollup
          dec            mapscroll_range
        
          ldx            #0        
          ldy            scrollmap      ;vert
loopu1       
          lda            1784,x    
          sta            1824,x

          lda            1744,x    
          sta            1784,x    

          lda            1704,x    
          sta            1744,x    

          lda            1664,x    
          sta            1704,x    
         
          lda            1624,x    
          sta            1664,x    

          lda            1584,x    
          sta            1624,x    

          lda            1544,x    
          sta            1584,x    

          lda            1504,x    
          sta            1544,x    

          lda            1464,x    
          sta            1504,x    

          lda            1424,x    
          sta            1464,x    

          lda            1384,x    
          sta            1424,x    

          lda            1344,x    
          sta            1384,x    

          lda            1304,x    
          sta            1344,x    

          lda            1264,x    
          sta            1304,x    

          lda            1224,x    
          sta            1264,x    

          lda            1184,x    
          sta            1224,x    

          lda            1144,x    
          sta            1184,x    

          lda            1104,x    
          sta            1144,x    

          lda            1064,x    
          sta            1104,x    

          lda            1024,x    
          sta            1064,x

;;To make this work, use zero page 253,254 addressing
;;clc adc #255 sta 252
          inx
          iny
          cpy #40
          bne            lpup 

          lda            #1        
          sta            scrolled_up

          lda            scrolled_right
          cmp            #1        
          beq            change_scrolldata_up

          lda            scrolled_left
          cmp            #1        
          beq            change_scrolldata_up
          jmp            skip_newscrn_up

change_scrolldata_up
          lda            scrollright_low
          sta            253   
          sta            53280
          lda            scrollright_hi
          sta            254
;================================================================
skip_newscrn_up
          lda            #3        
          sta            53280
          ldx            #0
rduhi     ldy            scrollmap3   
rdulow    lda           (253),y        ;gmap4
          sta           (251),y        ;1024,x
          iny
          cpy            #39
          bne            rdulow  
                                   ;inc            252           ;get next hi byte 

          lda            253 
          adc            #0 
          sta            253    
          sta            scrollup_low
                     
          lda            254
          clc
          adc            #255           ;up incrementer   
          sta            254       
          sta            scrollup_hi
          inx            
fin_up    rts

lpup      jmp loopu1
fin_un    rts

showspaceship
          lda            #51               ;80 (64 - start)      
          sta            2040      
          rts

spacebattle
          lda            joy       ;get joystick
          and            #4        ;check it for bit 4
          beq            mvshiplft     ;move player to the left
          lda            joy
          and            #8        ;check it for bit 8
          beq            mvshiprgt       ;move player to the right
          lda            joy
          and            #2        ;check it for bit 2
          beq            mvshipdwn     ;move player down
          ;lda           joy
          ;and           #1        ;check it for bit 1
          ;beq           mvpup     ;move player up
          jmp            spacebattle
          rts

mvshiplft
          dec PNT
          lda PNT       
          sta 2040   
          jsr            gamedelay
          jsr            pause
          jmp            spacebattle
mvshiprgt
          inc PNT
          lda PNT
          sta 2040   
          jsr            gamedelay
          jsr            pause
          jmp            spacebattle
          rts

mvshipdwn
          rts

gamedelay
          inc            charmovedelay
          lda            charmovedelay
          cmp            #255               ;if timer is not yet 40 secs
          bcc            gamedelay         ;then don't move the alien
          lda            #0
          sta            charmovedelay
skipdelay
          rts

shownum
div10:
          ldx            #$11
          lda            #$00
          clc
loopn     rol
          cmp            #$0A
          bcc            skip
          sbc            #$0A
skip      rol            div_lo
          rol            div_hi
          dex
          bne            loopn
          rts

div_lo          byte    0
div_hi          byte    0

viewnum
         lda number             ;you want to use (either low or hi byte)
         pha
         lsr
         lsr
         lsr
         lsr
         tax
         lda            convtable,x
         sta            1979
         lda            #15
         sta            55206
         pla
         and            #$0f
         tax
         lda            convtable,x
         sta            1980
         lda            #15
         sta            55297
         rts

convtable       byte $30,$31,$32,$33,$34,$35,$36,$37
                byte $38,$39,$01,$02,$03,$04,$05,$06 

  ;http://cronodon.com/Programming/machine_code.html
  
  ;screen memory 1039, 55311  
  ;This routine is used to print numbers to the screen
  ;such as may be used for a game score.
  
writenum
          ldx            #1
          lda            numsave           ;enappeartm 
          cmp            #200
          bcc            lpd1
          sbc            #200
          pha       
lpd1      clc
          cmp            #100
          bcc            jovr
          sbc            #100
          pha
          lda            #49
          sta            1949              ;screen memory location 1
          lda            #7                ;color memory for
          sta            56221             ;one place
lpd2      pla
jovr      clc
          ldy            #0
          cmp            #10
          bcc            lpd3
lpd3      iny
          sbc            #10
          cmp            #10
          bcs            lpd3
          pha
          tya
          adc            #48
          sta            1950              ;screen memory location 2
          lda            #7                ;color memory for 
          sta            56222             ;tenths place
          pla
          adc            #48
          sta            1951              ;screen meyory location 3
          lda            #7                ;color memory for
          stx            56223             ;hundredths place
          rts

pause
          LDX            #150                                     ;KEEP OUR DEMO
          LDY            #25                                     ;AT A STEADY PACE
DL        DEY
          BNE            DL
          DEX
          BNE            DL
          RTS

scrollmap               byte           0
scrollmap2              byte           0
scrollmap3              byte           0

mapscroll_range         byte           0
movedown1char           byte           0

mapdata                 byte           39
loopmap                 byte           0


scrolldown_low          byte           0
scrolldown_hi           byte           0

scrollup_low            byte           0
scrollup_hi             byte           0


scrollright_low         byte           0
scrollright_hi             byte           0


scrolled_up             byte           0
scrolled_down           byte           0
scrolled_right          byte           0
scrolled_left           byte           0


readgmaponce            byte           0

scbyte                  byte           0
charmovedelay           byte           0
flipalien               byte           0
vertposbkgnd            byte           0
spriteframe_ldr         byte           0

x1                      byte           110         ;170
y1                      byte           185

gravity                 byte           1
falldelay               byte           8

screen_value            byte           0
xchar                   byte           0
ychar                   byte           0
screen_tempx            byte           0
screen_tempy            byte           0
screen_temp_total       byte                  0
screen_temp_ylow        byte           0
screen_temp_yhigh       byte           0

ychar2                  byte           0

unlockup                byte           0
unlockdown              byte           0
xladder                 byte           0
writepos                word 1989,1989,1989,1989

numsave                 byte           0
numsave2                byte           0
alien_x1                byte           0
savea                   byte           0
savex                   byte           0
savey                   byte           0
showmsg                 byte           0
msgalert                byte 0,0,0,0

gmes0     text           'Spaced Out, Steve Morrow (c) 2015'
gmes1     text           '  An alien is approaching        '
gmes2     text           '  Beware of the snake alien      '
gmes3     text           '  You see an alien dragon        '

rlines  byte 100,101,102,103,104,105 


timer
        byte $08,$08,$08,$08,$08,$08
        byte $08,$01
        byte $08,$08,$08,$08,$08,$08
        byte $08,$01
        byte $08,$08,$08,$08,$08,$08
        byte $08,$01
        byte $08,$08,$08,$08,$08,$08
        byte $08,$01

colorast
        byte $01,$00,$01,$00,$01,$00
        byte $01,$00
        byte $01,$00,$01,$00,$01,$00
        byte $01,$00
        byte $01,$00,$01,$00,$01,$00
        byte $01,$00
        byte $01,$00,$01,$00,$01,$00
        byte $01,$00

;DATA TABLES FOR COLOURS

COLOUR  BYTE $09,$09,$02,$02,$08
        BYTE $08,$0A,$0A,$0F,$0F
        BYTE $07,$07,$01,$01,$01
        BYTE $01,$01,$01,$01,$01
        BYTE $01,$01,$01,$01,$01
        BYTE $01,$01,$01,$07,$07
        BYTE $0F,$0F,$0A,$0A,$08
        BYTE $08,$02,$02,$09,$09
        BYTE $00,$00,$00,$00,$00

;DATA FOR TEXT MESSAGE

MESSAGE TEXT "RICHARD BAYLISS'"
        TEXT " COLOUR SCROLLER"
        TEXT " ACTIVE......"
        TEXT "  

txtmsg1         byte           1,1,1,1
rastmap         
                byte           190,191,192,193,194,195,196,197,198,199,200 

firstRaster     byte           225       ; End of Blue, start of black
secondRaster            
                byte           300       ; End of black line
thirdRaster             
                byte           345       ; start of yellow

fourthRaster    byte           240,241,242,243,244,245,246,247,248 
sixthRaster     byte 223                 ; End of bottom black line
seventhRaster   byte           350       ;
eighthRaster    byte           348

countrast       byte           0

oneRaster               byte 295,296,297,298,299,300,301,302,303,304,305

gscreenmaplo    byte    <gmap4, <gmap4, <gmap4, <gmap4
gscreenmaphi    byte    >gmap4, >gmap4, >gmap4, >gmap4

mapdatalo       byte <gmap4
mapdatahi       byte >gmap4

gamemsgdisplaylo    byte    <gmes0, <gmes1, <gmes2, <gmes3
gamemsgdisplayhi    byte    >gmes0, >gmes1, >gmes2, >gmes3

charsetlo       byte    <charset1
charsethi       byte    >charset1

gamescreenlo    byte    <gmap4, <gmap4, <gmap4, <gmap4, <gmap4, <gmap4          
gamescreenhi    byte    >gmap4, >gmap4, >gmap4, >gmap4, >gmap4, >gmap4


charsprite
incbin    "charsprite6.raw"

aliensprite
incbin    "alienspriteset3.raw"

gmap4
incbin    "mapscrollroom10a.raw"              

title
incbin    "maptitle1.raw"          

charset1
incbin  "spacedoutcharset10a.raw"
