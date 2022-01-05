; Welcome to Wagner Baseball Version 8-4!!
; Written by Professor David Wagner
; 
; Improved by:
; 	Group D
; 	Student Members:
; 		2014002888  KIM JEONG HWAN
;		2018006144  KOO YOU JUNG
;
; A project for the 8051 Microcontroller

;=================================================================
;=================================================================
;                       DEFINED VALUES
;=================================================================
;=================================================================


;*********************************************************************
; Variables
; Addresses in 8051 Memory of the Variables used by this program
;********************************************************************


; ================== LCD Variables ==================

LCD-Instr-Mem         EQU     30H    ; The instruction to be sent to the LCD
LCD-Data-Mem          EQU     31H    ; The data to be sent to the LCD

LCD-Cursor-Row        EQU     32H    ; The row number where a character will be displayed on the LCD
LCD-Cursor-Col        EQU     33H    ; The column number where a character will be displayed on the LCD

Char-Loc-Offset       EQU     34H    ; The offset location of the character in memory
Char-value            EQU     35H    ; The ASCII code of the character to display

Message-Lo-Byte       EQU     36H    ; The low byte of the message for DPTR
Message-Hi-Byte       EQU     37H    ; The high byte of the message for DPTR
Font-Lo-Byte          EQU     38H    ; Low Byte of the font for DPTR
Font-Hi-Byte          EQU     39H    ; High Byte of the font for DPTR

Font-Row-Num          EQU     3AH    ; Font Row Number
CGRAM-Address         EQU     3BH    ; CGRAM Address of Character in LCD

Message-Length        EQU     3CH    ; The size of the message in memory

; ================== Key Pad Variables ==================

Key-Col-Num           EQU     40H    ; The column number of the key pressed
Key-Row-Num           EQU     41H    ; The row number of the key pressed
Key-Num               EQU     42H    ; The number of the key pressed

Key-Col-Pattern       EQU     43H    ; The pattern of all columns to scan
Key-Row-Pattern       EQU     44H    ; The pattern of all rows which were scannned

; ================== Game Data Variables ==================

Score-Player1         EQU     45H    ; The score of Player 1
Score-Player2         EQU     46H    ; The score of Player 2

Ball-Count            EQU     47H    ; The number of Balls
Strike-Count          EQU     48H    ; The number of Strikes
Out-Count             EQU     49H    ; The number of Outs

Player-At-Bat         EQU     4AH    ; The number of the player at bat (1 or 2)

Inning-Number         EQU     4BH    ; The Inning Number

Base1-Occupied        EQU     4CH    ; 1 if First Base is Occupied, O if not
Base2-Occupied        EQU     4DH    ; 1 if Second Base is Occupied, O if not
Base3-Occupied        EQU     4EH    ; 1 if Third Base is Occupied, O if not

; ================== Dot Matrix Variables ==================

Dot-Current-Row       EQU     50H    ; Which Dot Matrix Row is currently Displayed
Dot-Current-Bits      EQU     51H    ; Bit Pattern with bit in currently Displayed Row

Ball-Dot-Row          EQU     52H    ; The Row of the baseball on the dot matrix 
Ball-Row-Bits         EQU     53H    ; The Bit Pattern of the baseball row

Dot-Saved-Row         EQU     54H    ; The Row Number in the Dot Matrix which will be saved

Dot-Red-Saved         EQU     55H    ; The saved Bit Pattern from the red dot matrix row
Dot-Green-Saved       EQU     56H    ; The saved Bit Pattern from the green dot matrix row

Dot-Red-Matrix        EQU     60H    ; The Red Dot Matrix data is stored 50H-57H
Dot-Green-Matrix      EQU     68H    ; The Green Dot Matrix data is stored 58H-5FH

;*********************************************************************
; Hangul Code Variables
; Addresses in 8051 Data Memory where the Character Codes for each 
; Hangul can be found
;********************************************************************
Char-Code-AN          EQU     70H
Char-Code-YO          EQU     71H
Char-Code-NYEONG      EQU     72H
Char-Code-HA          EQU     73H
Char-Code-SE          EQU     74H
Char-Code-IN          EQU     75H
Char-Code-GU          EQU     76H
Char-Code-GEU         EQU     77H
Char-Code-WA          EQU     78H
Char-Code-YA          EQU     79H
Char-Code-NEO         EQU     7AH
Char-Code-JUNG	      EQU     7BH
Char-Code-HWAN        EQU     7CH
Char-Code-YOU         EQU     7DH
Char-Code-BLANK       EQU     7EH

;*****************************************************************
; Constants
;*****************************************************************

; =================== Key Pad Constants ===================

NoNumberPressed EQU 50H ; The Key-Number if no number is pressed
BallKeyPress EQU 00H ; The Key-Number if Ball is pressed
StrikeKeyPress EQU 01H ; The Key-Number if Strike is pressed
OutKeyPress EQU 02H ; The Key-Number if Strike is pressed

; ================== Dot Matrix Constants ==================

DotOnDelay1 EQU 010H ; The wait time while the Dot Matrix is On
DotOnDelay2 EQU 010H ; The wait time while the Dot Matrix is On

BallOnDelay1 EQU 060H ; The wait time while the Ball is Displayed
BallOnDelay2 EQU 060H ; The wait time while the Ball is Displayed

InitPitchRow EQU 4 ; The initial row of the baseball on the field
InitPitchBits EQU 00010000B ; The initial bit pattern on the field 
InitBallRow EQU 2 ; The initial row of a ball on Home Plate
InitBallBits EQU 00000001B ; The initial bit pattern on Home Plate
InitStrikeRow EQU 0 ; The initial row of a strike on Home Plate
InitStrikeBits EQU 00000001B ; The initial bit pattern on Home Plate
BallLastRow EQU 8 ; The final Row of the ball after throwing it

Base1Row EQU 2 ; The row number of first base
Base1Bits EQU 01000000B  ; The bit pattern of first base
Base2Row EQU 2 ; The row number of second base
Base2Bits EQU 00000100B  ; The bit pattern of second base
Base3Row EQU 6 ; The row number of third base
Base3Bits EQU 00000100B  ; The bit pattern of third base

; ========================= LCD Constants ========================= 

Line1-Address           EQU     00H  ; Address in LCD DDRAM memory where Line 1 begins
Line2-Address           EQU     40H  ; Address in LCD DDRAM memory where Line 2 begins
Font-Row-Max            EQU     08H  ; The Number of Rows in a Character Font

;*****************************************************************
; Device Registers
;*****************************************************************

; ============== Memory Locations of LCD Registers ==============

LCD-Write-Instr        EQU     0FFE0H     ; The register in the LCD which receives instructions
LCD-Write-Data         EQU     0FFE1H     ; The register in the LCD which receives data

LCD-Read-Instr         EQU     0FFE2H     ; The register in the LCD which sends instructions
LCD-Read-Data          EQU     0FFE3H     ; The register in the LCD which sends data


; ===== Memory Locations of the Dot Matrix Rows and Columns =====

DotMatrix-Green        EQU     0FFC5H     ; Which columns are colored green
DotMatrix-Red          EQU     0FFC6H     ; Which columns are colored red
DotMatrix-Row          EQU     0FFC7H     ; Which rows are turned on

; ========= Memory Locations of KeyPad Rows and Columns =========

Keypad-Data-Out        EQU     0FFF0H    ; Write which columns to consider
Keypad-Data-In         EQU     0FFF1H     ; Read the rows 

; ====== Memory Locations of LED 7-Segment Array Registers ======

LED-7-Segment1         EQU     0FFC3H    ; The first and second LED Numbers
LED-7-Segment2         EQU     0FFC2H    ; The third and fourth LED Numbers
LED-7-Segment3         EQU     0FFC1H    ; The fifth and sixth LED Numbers

Buzzer		       EQU     0FFEFH

; ========== Memory Location of Seven Segment Display ==========

Seven-Segment 	       EQU     0FFC0H    ; The location of the 7-segment display

;*****************************************************************
; LCD Instructions
; Instruction Codes for each of the LCD Commands
;*****************************************************************

Clear-LCD              EQU     01H   ; Clear the LCD Screen entirely
Cursor-Home-LCD        EQU     02H   ; Move the Cursor to the Home position

; Does the cursor address increase or decrease after printing a character?
; and does the screen shift?
Cur-Dec-LCD            EQU     04H   ; Move the Cursor Address Left after writing
Cur-DecShift_LCD       EQU     05H   ; Move the Cursor Address Left and Shift the Screen after writing
Cur-Inc-LCD            EQU     06H   ; Move the Cursor Address Right after writing
Cur-IncShift_LCD       EQU     07H   ; Move the Cursor Address Right and Shift the Screen after writing

; Turn on or off the Display, the Cursor, and the Blinking Cursor
DCB-Off-LCD            EQU     08H    ;Display(OFF) ,Cursor(OFF) ,Blink(OFF)
Blink-On-LCD           EQU     09H    ;Display(OFF) ,Cursor(OFF) ,Blink(ON)
Cursor-On-LCD          EQU     0AH    ;Display(OFF) ,Cursor(ON) ,Blink(OFF)
CurBlink-On-LCD        EQU     0BH    ;Display(OFF) ,Cursor(ON) ,Blink(ON)
Display-On-LCD         EQU     0CH    ;Display(ON) ,Cursor(OFF) ,Blink(OFF)
DispBlink-On-LCD       EQU     0DH    ;Display(ON) ,Cursor(OFF) ,Blink(ON)
DispCur-On-LCD         EQU     0EH    ;Display(ON) ,Cursor(ON) ,Blink(OFF)
DCB-On-LCD             EQU     0FH    ;Display(ON) ,Cursor(ON) ,Blink(ON)

;Set the number of Rows and the Font Size
OneRow_5X7-LCD         EQU     30H    ;8-Bit 1 Row 5*7  , 1/16 duty
OneRow_5X10-LCD        EQU     34H    ;8-Bit 1 Row 5*10 , 1/16 duty
TwoRows-5X7-LCD        EQU     38H    ;8-Bit 2 Row 5*7  , 1/16 duty
TwoRows-5X10-LCD       EQU     3CH    ;8-Bit 2 Row 5*10 , 1/16 duty

;Set the CG RAM Address
Start-CGRAM-LCD        EQU     40H    ;0 100 0000 : CG-RAM Address Setting

;Set the DD RAM Address, which moves the cursor to that address
Move-Cursor-LCD        EQU     80H    ;1 000 0000 : Data Entry Start Address



;=================================================================
;=================================================================
;               MAIN PROGRAM EXECUTION LOOP
;=================================================================
;=================================================================

;*****************************************************************
; Main Loop
;*****************************************************************

	ORG 8000H
MAIN:
	CALL InitProgram
	CALL ShowMsgPlayYagu
MainLoop:
	CALL WaitForKeyPress
	CALL KeyPressAction
	CALL WaitForKeyRel
	JMP MainLoop

;*****************************************************************
; This function waits until a key is pressed
;*****************************************************************

WaitForKeyPress:
	CALL DisplayDotMatrix
	CALL ScanKeyboard
	CALL GetKeyNum
	MOV A, Key-Num
	CJNE A, #NoNumberPressed, KeyPressFound
	JMP WaitForKeyPress
KeyPressFound:
	RET

;*****************************************************************
; This function waits until all keys are released
;*****************************************************************

WaitForKeyRel:
	CALL DisplayDotMatrix
	CALL ScanKeyboard
	CALL GetKeyNum
	MOV A, Key-Num
	CJNE A, #NoNumberPressed, WaitForKeyRel
	RET

;=================================================================
;=================================================================
;               INITIALIZATION PROCEDURES 
;=================================================================
;=================================================================

;*****************************************************************
; Initialize the program
;*****************************************************************

InitProgram:
	CALL InitVariables
	CALL InitDisplays
	RET


;*****************************************************************
; Initialize the variables
;*****************************************************************

InitVariables:
	MOV A, #0
	MOV Score-Player1, A
	MOV Score-Player2, A
	MOV Ball-Count, A
	MOV Strike-Count, A
	MOV Out-Count, A
	MOV Base1-Occupied, A
	MOV Base2-Occupied, A
	MOV Base3-Occupied, A
	MOV A, #1
	MOV Player-At-Bat, A
	MOV Inning-Number, A
	RET

;*****************************************************************
; Initialize the displays
;*****************************************************************

InitDisplays:
	CALL InitDotMatrix
	CALL InitLCD
	CALL DisplayAll
	RET

;*****************************************************************
; Initialize the Dot Matrix
;*****************************************************************

InitDotMatrix:
	MOV Dot-Current-Row, #0
	MOV Dot-Current-Bits, #00000001b
	RET

;*****************************************************************
; Initialize the LCD Display
;*****************************************************************

InitLCD:
	MOV LCD-Instr-Mem, #TwoRows-5X7-LCD
	CALL WriteInstToLCD
	MOV LCD-Instr-Mem, #DispCur-On-LCD
	CALL WriteInstToLCD
	MOV LCD-Instr-Mem, #Clear-LCD
	CALL WriteInstToLCD
	MOV LCD-Instr-Mem, #Cur-Inc-LCD
	CALL WriteInstToLCD
	RET

;=================================================================
;=================================================================
;                       KEY PRESS ACTION
;=================================================================
;=================================================================

;*****************************************************************
; Perform the Actions when a key is pressed
;*****************************************************************

KeyPressAction:
;	CALL DisplayAll
	MOV A, Key-Num
CheckBall: CJNE A, #BallKeyPress, CheckStrike
	CALL BallAction
	JMP FinishKeyCheck
CheckStrike: CJNE A, #StrikeKeyPress, CheckOut
	CALL StrikeAction
	JMP FinishKeyCheck
CheckOut: CJNE A, #OutKeyPress, CheckSingle
	CALL OutAction
	JMP FinishKeyCheck
CheckSingle: CJNE A, #4, CheckDouble
	CALL SingleAction
	JMP FinishKeyCheck
CheckDouble: CJNE A, #5, CheckTriple
	CALL DoubleAction
	JMP FinishKeyCheck
CheckTriple: CJNE A, #6, CheckHomerun
	CALL TripleAction
	JMP FinishKeyCheck
CheckHomerun: CJNE A, #7, CheckSteal
	CALL HomerunAction
	JMP FinishKeyCheck
CheckSteal: CJNE A, #8, CheckSacHit
	CALL StealAction
	JMP FinishKeyCheck
CheckSacHit: CJNE A, #9, FinishKeyCheck 
	CALL SacHitAction

FinishKeyCheck:
	CALL DisplayAll
	RET

;*****************************************************************
; Perform the Actions for a Ball
;*****************************************************************

BallAction:
	CALL ThrowBall
	CALL ShowMsgBall
	CALL IncBallCount
	RET

;*****************************************************************
; Perform the Actions for a Strike
;*****************************************************************

StrikeAction:
	CALL ThrowStrike
	CALL ShowMsgStrike
	CALL IncStrikeCount
	RET

;*****************************************************************
; Perform the Actions for an Out
;*****************************************************************

OutAction:
	CALL ThrowStrike
	CALL ShowMsgOut
	CALL IncOutCount
	RET

;*****************************************************************
; Perform the Actions for a Single
;*****************************************************************

SingleAction:
	CALL ThrowStrike
	CALL HitBall
	CALL ShowMsgSingle
	CALL ApplySingle
	CALL FinishHit
	RET

;*****************************************************************
; Perform the Actions for a Double
;*****************************************************************

DoubleAction:
	CALL ThrowStrike
	CALL HitBall
	CALL ShowMsgDouble
	CALL ApplyDouble
	CALL FinishHit
	RET

;*****************************************************************
; Perform the Actions for a Triple
;*****************************************************************

TripleAction:
	CALL ThrowStrike
	CALL HitBall
	CALL ShowMsgTriple
	CALL ApplyTriple
	CALL FinishHit
	RET

;*****************************************************************
; Perform the Actions for a Homerun
;*****************************************************************

HomerunAction:
	CALL ThrowStrike
	CALL HitBall
	CALL ShowMsgHomerun
	CALL ApplyHomerun
	CALL FinishHit
	RET

;*****************************************************************
; Perform the Actions for a Steal (Base1->Base2 Only)
;	(Only work when Base2 is empty and Base1 is load.)
;*****************************************************************

StealAction:
	CALL ApplySteal
	RET

;*****************************************************************
; Perform the Actions for a Sacrifice Hit
;   Rule: (Batter->Out / Base runner->Advance one base)
;*****************************************************************

SacHitAction:
	CALL ThrowStrike
	CALL ShowMsgSacHit
	CALL ApplySacHit
	CALL FinishHit
	RET

;=================================================================
;=================================================================
;                   GAME LOGIC - ADD STRIKE OR BALL
;=================================================================
;=================================================================

;*****************************************************************
; Increase the number of balls
;*****************************************************************

IncBallCount:
	MOV A, Ball-Count
	INC A
	CJNE A, #4, FinishBallInc
	CALL ShowMsgWalk
	CALL WalkBatter
	MOV A, #0

FinishBallInc:
	MOV Ball-Count, A
	CALL DispBallStrkOut
	RET

;*****************************************************************
; Walk the batter
;*****************************************************************

WalkBatter:
	MOV A, Base1-Occupied
	CJNE A, #1, OccupyBase1
	MOV A, Base2-Occupied
	CJNE A, #1, OccupyBase2
	MOV A, Base3-Occupied
	CJNE A, #1, OccupyBase3
	CALL IncScore
	JMP FinishWalk

OccupyBase1:
	MOV Base1-Occupied, #1
	CALL CopyBasesToDotM
	JMP FinishWalk

OccupyBase2:
	MOV Base2-Occupied, #1
	CALL CopyBasesToDotM
	JMP FinishWalk

OccupyBase3:
	MOV Base3-Occupied, #1
	CALL CopyBasesToDotM
	JMP FinishWalk

FinishWalk:
	MOV A, #0
	MOV Ball-Count, #0
	MOV Strike-Count, #0
	CALL DisplayScores
	RET

;*****************************************************************
; Increase One score each time (Walk/Single/Double/Triple/Homerun)
;*****************************************************************

IncScore:
	MOV A, Player-At-Bat
	CJNE A, #1, IncP2Score

IncP1Score:
	MOV A, Score-Player1
	INC A
	DA A
	MOV Score-Player1, A
	RET

IncP2Score:
	MOV A, Score-Player2
	INC A
	DA A
	MOV Score-Player2, A
	RET
;*****************************************************************
; Increase the number of strikes
;*****************************************************************

IncStrikeCount:
	MOV A, Strike-Count
	INC A
	CJNE A, #3, FinishStrikeInc
	CALL ShowMsgStrikeOut
	CALL IncOutCount
	MOV A, #0

FinishStrikeInc:
	MOV Strike-Count, A
	CALL DispBallStrkOut
	RET

;*****************************************************************
; Increase the number of outs
;*****************************************************************

IncOutCount:
	MOV A, Out-Count
	INC A
	CJNE A, #3, FinishOutInc
	CALL SwitchPlayers
	MOV A, #0

FinishOutInc:
	MOV Out-Count, A
	MOV A, #0
	MOV Ball-Count, #0
	MOV Strike-Count, #0
	CALL DispBallStrkOut
	RET

;*****************************************************************
; Switch Players
;*****************************************************************

SwitchPlayers:
	CALL UnoccupyBases
	MOV A, Player-At-Bat
	INC A
	CJNE A, #3, FinishSwitch
	CALL IncInning
	MOV A, #1

FinishSwitch:
	MOV Player-At-Bat, A
	CALL ClearLCD
	CALL DisplayAtBat
	RET

;*****************************************************************
; Remove the Runners from all the bases
;*****************************************************************

UnoccupyBases:
	MOV Base1-Occupied, #0
	MOV Base2-Occupied, #0
	MOV Base3-Occupied, #0
	CALL CopyBasesToDotM
	RET

;*****************************************************************
; Increase the inning
;*****************************************************************

IncInning:
	MOV A, Inning-Number
	INC A
	CJNE A, #10, FinishInningInc
	CALL GameOver

FinishInningInc:
	MOV Inning-Number, A
	CALL DisplayInningNum
	RET

;*****************************************************************
; Game Over
;*****************************************************************

GameOver:
	CALL ShowMsgGameOver
GameOverLoop:
	CALL DisplayDotMatrix
	JMP GameOverLoop


;=================================================================
;=================================================================
;                       THROW THE BASEBALL
;=================================================================
;=================================================================

;*****************************************************************
; Throw a ball
;*****************************************************************

ThrowBall:
	CALL ThrowPitch
	CALL DisplayPlate
	CALL ThrowBallPlate
	CALL DisplayBases
	RET

;*****************************************************************
; Throw a strike
;*****************************************************************

ThrowStrike:
	CALL ThrowPitch
	CALL DisplayPlate
	CALL ThrowStrikePlate
	CALL DisplayBases
	RET

;*****************************************************************
; Throw a pitch on the field
;*****************************************************************

ThrowPitch:
	CALL InitBallPitch
	JMP ThrowLoop

;*****************************************************************
; Throw a ball over Home Plate
;*****************************************************************

ThrowBallPlate:
	CALL InitBallPlate
	JMP ThrowLoop

;*****************************************************************
; Throw a strike over Home Plate
;*****************************************************************

ThrowStrikePlate:
	CALL InitStrikePlate
	JMP ThrowLoop

;*****************************************************************
; Display a moving baseball on the dot matrix
;*****************************************************************

ThrowLoop:
	CALL ShowBall
	CALL DelayBallOn
	CALL HideBall
	CALL AdvanceBall
	MOV A, Ball-Dot-Row
	CJNE A, #BallLastRow, ThrowLoop
	RET

;*****************************************************************
; Initialize the baseball location before throwing it on the field
;*****************************************************************

InitBallPitch:
	MOV Ball-Dot-Row, #InitPitchRow
	MOV Ball-Row-Bits, #InitPitchBits
	RET

;*****************************************************************
; Initialize the location before throwing a ball over Home Plate
;*****************************************************************

InitBallPlate:
	MOV Ball-Dot-Row, #InitBallRow
	MOV Ball-Row-Bits, #InitBallBits
	RET

;*****************************************************************
; Initialize the location before throwing a strike over Home Plate
;*****************************************************************

InitStrikePlate:
	MOV Ball-Dot-Row, #InitStrikeRow
	MOV Ball-Row-Bits, #InitStrikeBits
	RET

;*****************************************************************
; Show or Hide the baseball on the Dot Matrix
;*****************************************************************

ShowBall:
	CALL SaveBallDotRow
	CALL WriteBall
	RET

HideBall:
	CALL RestoreDotRow
	RET

;*****************************************************************
; Wait while the Ball is displayed
;*****************************************************************

DelayBallOn:
	MOV R4, #BallOnDelay1
DelayBall1:
	MOV R5, #BallOnDelay2
DelayBall2:
	DJNZ R5, DelayBall2
	CALL DisplayDotMatrix
	DJNZ R4, DelayBall1
	RET

;*****************************************************************
; Advance the baseball to the next position
;*****************************************************************

AdvanceBall:
	CALL IncrementBallRow
	CALL RotateBallBits
	RET

IncrementBallRow:
	MOV A, Ball-Dot-Row
	INC A
	MOV Ball-Dot-Row, A
	RET

RotateBallBits:
	MOV A, Ball-Row-Bits
	RL A
	MOV Ball-Row-Bits, A
	RET

;*****************************************************************
; Save the row on the dot matrix before writing the baseball
;*****************************************************************

SaveBallDotRow:
	MOV A, Ball-Dot-Row
	MOV Dot-Saved-Row, A
	CALL SaveDotRow
	RET

;*****************************************************************
; Save the row on the dot matrix before overwriting it
;*****************************************************************

SaveDotRow:
	CALL SaveRedRow
	CALL SaveGreenRow
	RET

SaveRedRow:
	MOV A, #Dot-Red-Matrix
	ADD A, Dot-Saved-Row
	MOV R1, A
	MOV Dot-Red-Saved, @R1
	RET

SaveGreenRow:
	MOV A, #Dot-Green-Matrix
	ADD A, Dot-Saved-Row
	MOV R1, A
	MOV Dot-Green-Saved, @R1
	RET

;*****************************************************************
; Restore the row on the dot matrix which had been overwritten
;*****************************************************************

RestoreDotRow:
	CALL RestoreRedRow
	CALL RestoreGreenRow
	RET

RestoreRedRow:
	MOV A, #Dot-Red-Matrix
	ADD A, Ball-Dot-Row
	MOV R1, A
	MOV @R1, Dot-Red-Saved
	RET

RestoreGreenRow:
	MOV A, #Dot-Green-Matrix
	ADD A, Ball-Dot-Row
	MOV R1, A
	MOV @R1, Dot-Green-Saved
	RET

;*****************************************************************
; Write the baseball on the Dot Matrix
;*****************************************************************

WriteBall:
	CALL WriteBallRed
	CALL EraseBallGreen
	RET

WriteBallRed:
	MOV A, #Dot-Red-Matrix
	ADD A, Ball-Dot-Row
	MOV R1, A
	MOV A, @R1
	ORL A, Ball-Row-Bits
	MOV @R1, A
	RET

EraseBallGreen:
	MOV A, #Dot-Green-Matrix
	ADD A, Ball-Dot-Row
	MOV R1, A
	MOV A, @R1
	CPL A
	ORL A, Ball-Row-Bits
	CPL A
	MOV @R1, A
	RET


;=================================================================
;=================================================================
;                     	HIT THE BASEBALL
;			(APPLY THE HIT)			
;=================================================================
;=================================================================

;*****************************************************************
; Display Hitting and other
;*****************************************************************

Hitball:
	RET

;*****************************************************************
; Apply Single
;*****************************************************************

ApplySingle:

CheckBase3_S:
	MOV A, Base3-Occupied
	CJNE A, #1, CheckBase2_S
	CALL IncScore
	
CheckBase2_S:
	MOV A, Base2-Occupied
	CJNE A, #1, UnoccupyBase3_S
	CALL OccupyBase3_Hit

CheckBase1_S:
	MOV A, Base1-Occupied
	CJNE A, #1, UnoccupyBase2_S
	CALL OccupyBase2_Hit

FinishApplyS:
	CALL OccupyBase1_Hit
	CALL CopyBasesToDotM
	RET


UnoccupyBase3_S:
	MOV Base3-Occupied, #0
	JMP CheckBase1_S
UnoccupyBase2_S:
	MOV Base2-Occupied, #0
	JMP FinishApplyS


;*****************************************************************
; Apply Double
;*****************************************************************

ApplyDouble:

CheckBase3_D:
	MOV A, Base3-Occupied
	CJNE A, #1, CheckBase2_D
	CALL IncScore

CheckBase2_D:
	MOV A, Base2-Occupied
	CJNE A, #1, CheckBase1_D
	CALL IncScore

CheckBase1_D:
	MOV A, Base1-Occupied
	CJNE A, #1, UnoccupyBase3_D
	CALL Occupybase3_Hit
	JMP FinishApplyD

UnoccupyBase3_D:
	MOV Base3-Occupied, #0

FinishApplyD:
	CALL OccupyBase2_Hit
	MOV Base1-Occupied, #0
	CALL CopyBasesToDotM
	RET
	

;*****************************************************************
; Apply Triple
;*****************************************************************

ApplyTriple:

CheckBase3_T:
	MOV A, Base3-Occupied
	CJNE A, #1, CheckBase2_T
	CALL IncScore

CheckBase2_T:
	MOV A, Base2-Occupied
	CJNE A, #1, CheckBase1_T
	CALL IncScore

CheckBase1_T:
	MOV A, Base1-Occupied
	CJNE A, #1, FinishApplyT
	CALL IncScore

FinishApplyT:
	CALL ApplyBases_T
	CALL CopyBasesToDotM
	RET

;*****************************************************************
; Apply Homerun (with Buzzer)
;*****************************************************************

ApplyHomerun:
	CALL IncScore

CheckBase3_H:
	MOV A, Base3-Occupied
	CJNE A, #1, CheckBase2_H
	CALL IncScore

CheckBase2_H:
	MOV A, Base2-Occupied
	CJNE A, #1, CheckBase1_H
	CALL IncScore

CheckBase1_H:
	MOV A, Base1-Occupied
	CJNE A, #1, FinishApplyH
	CALL IncScore

FinishApplyH:
	CALL ApplyBases_H
	CALL CopyBasesToDotM

;	CALL DotMatrixEmpty

	MOV A, #10000000B
	CALL BUZ
	CALL DELAY

	MOV A, #00000000B
	CALL BUZ

	CALL DisplayDotMatrix

	RET


;*****************************************************************
; Base Occupying (Triple, Homerun)
;*****************************************************************


ApplyBases_T:
	MOV Base3-Occupied, #1
	MOV Base2-Occupied, #0
	MOV Base1-Occupied, #0
	RET

ApplyBases_H:
	MOV Base3-Occupied, #0
	MOV Base2-Occupied, #0
	MOV Base1-Occupied, #0
	RET

OccupyBase1_Hit:
	MOV Base1-Occupied, #1
	CALL CopyBasesToDotM
	RET
OccupyBase2_Hit:
	MOV Base2-Occupied, #1
	CALL CopyBasesToDotM
	RET
OccupyBase3_Hit:
	MOV Base3-Occupied, #1
	CALL CopyBasesToDotM
	RET


;*****************************************************************
; Finish Hit (Clear Ball & Strike count)
;*****************************************************************

FinishHit:
	MOV A, #0
	MOV Ball-Count, #0
	MOV Strike-Count, #0
;	CALL DisplayScores
	RET

;*****************************************************************
; Buzzer (& Delay)
;*****************************************************************

BUZ:	MOV DPTR, #BUZZER
	MOVX @DPTR, A
	RET

DELAY: 	MOV R7, #0FFH
DELAY1:	MOV R6, #0FFH
DELAY2:	DJNZ R6, DELAY2
	CALL DisplayDotMatrix
	DJNZ R7, DELAY1
	RET

;*****************************************************************
; Apply Steal
;*****************************************************************

ApplySteal:
	MOV A, Base1-Occupied
	CJNE A, #1, CannotSteal
	MOV A, Base2-Occupied
	CJNE A, #0, CannotSteal
	
	CALL ShowMsgSteal
	
	MOV Base2-Occupied, #1
	MOV Base1-Occupied, #0
	RET

CannotSteal:
	
	CALL ShowMsgCantSteal
	RET

;*****************************************************************
; Apply Sacrifice Hit
;*****************************************************************

ApplySacHit:
	MOV A, Out-Count
	CJNE A, #2, CheckBase3_SH
	CALL IncOutCount
	RET

CheckBase3_SH:
	MOV A, Base3-Occupied
	CJNE A, #1, CheckBase2_SH
	CALL IncScore

CheckBase2_SH:
	MOV A, Base2-Occupied
	CJNE A, #1, UnoccupyBase3_SH
	MOV Base3-Occupied, #1

CheckBase1_SH:
	MOV A, Base1-Occupied
	CJNE A, #1, UnoccupyBase2_SH
	MOV Base2-Occupied, #1

FinishApplySH:
	MOV Base1-Occupied, #0
	MOV A, Out-Count
	INC A
	MOV Out-Count, A
	RET

UnoccupyBase3_SH:
	MOV Base3-Occupied, #0
	JMP CheckBase1_SH

UnoccupyBase2_SH:
	MOV Base2-Occupied, #0
	JMP FinishApplySH


;=================================================================
;=================================================================
;                    Keyboard Procedures
;=================================================================
;=================================================================

;*****************************************************************
; Scan the keyboard for any keys pressed
;*****************************************************************

ScanKeyboard:
	MOV Key-Col-Num, #1
	MOV Key-Col-Pattern, #11101111B

ScanOneCol:
	CALL ScanKeyPadCol
	MOV A, Key-Row-Num
	CJNE A, #0, KeyPressed 

	CALL IncrementCol
	MOV A, Key-Col-Num
	CJNE A, #6, ScanOneCol

	MOV Key-Col-Num, #0
	RET

KeyPressed:
	RET

;*****************************************************************
; Increment the number of the column and rotate the pattern
; Input
;	Key-Col-Num
;	Key-Col-Pattern
;*****************************************************************

IncrementCol:

	MOV A, Key-Col-Num
	INC A,
	MOV Key-Col-Num, A
	MOV A, Key-Col-Pattern
	RR A
	MOV Key-Col-Pattern, A
	RET


;*****************************************************************
; Scan One Column And Assign the row to Key-Row-Num
; Input
;	Key-Col-Pattern
;*****************************************************************

ScanKeyPadCol:
	MOV A, Key-Col-Pattern
	MOV DPTR, #Keypad-Data-Out
	MOVX @DPTR, A

	MOV DPTR, #Keypad-Data-In
	MOVX A, @DPTR
	MOV Key-Row-Pattern, A

ScanRow1: CJNE A, #11101111b ScanRow2
	MOV Key-Row-Num, #1
	RET
ScanRow2: CJNE A, #11110111b ScanRow3
	MOV Key-Row-Num, #2
	RET
ScanRow3: CJNE A, #11111011b ScanRow4
	MOV Key-Row-Num, #3
	RET
ScanRow4: CJNE A, #11111101b ScanRow5
	MOV Key-Row-Num, #4
	RET
ScanRow5: CJNE A, #11111110b NoKeyPressed
	MOV Key-Row-Num, #5
	RET
NoKeyPressed:
	MOV Key-Row-Num, #0
	RET


;*****************************************************************
; Store the number of the key pressed into Key-Num
;*****************************************************************

GetKeyNum:
	MOV A, Key-Row-Num
	
GetRow1: CJNE A, #1, GetRow2
	MOV A, Key-Col-Num
	SUBB A, #1
	CJNE A, #4, SaveKeyNum
	JMP NoKeyPressed
GetRow2: CJNE A, #2, GetRow3
	MOV A, Key-Col-Num
	ADD A, #3
	CJNE A, #8, SaveKeyNum
	JMP NoKeyPressed
GetRow3: CJNE A, #3, GetRow4
	MOV A, Key-Col-Num
	ADD A, #7
	CJNE A, #12, SaveKeyNum
	JMP NoKeyPressed
GetRow4: CJNE A, #4, NoNumPressed
	MOV A, Key-Col-Num
	ADD A, #11
	CJNE A, #16, SaveKeyNum
	JMP NoKeyPressed
NoNumPressed:
	MOV A, #NoNumberPressed
		
SaveKeyNum:
	MOV Key-Num, A
	RET

;=================================================================
;=================================================================
;                    DOT MATRIX PROCEDURES
;=================================================================
;=================================================================

;*****************************************************************
; DotMatrix should not disply a line while the buzzer is ringing
;*****************************************************************

DotMatrixEmpty:
	MOV R0, #00H
	MOV R1, #00H
	MOV R2, #00H
	
	CALL EmptyGreen
	CALL EmptyRed

	RET

EmptyGreen:
	MOV DPTR, #DotMatrix-Green
	MOV A, R1
	MOVX @DPTR, A
	MOV DPTR, #DotMatrix-Row 
	MOV A, R0
	MOVX @DPTR, A
	RET
EmptyRed:
	MOV DPTR, #DotMatrix-Red
	MOV A, R2
	MOVX @DPTR, A
	MOV DPTR, #DotMatrix-Row
	MOV A, R0
	MOVX @DPTR, A
	RET


;*****************************************************************
; Display a Row of the Dot Matrix
;*****************************************************************

DisplayDotMatrix:
	CALL ClearDOTROW
	CALL IncrementRow
	CALL AssignDOTGreen
	CALL AssignDOTRed
	CALL AssignDOTROW
	CALL DelayDotOn
	RET

;*****************************************************************
; Advance to the next row of the Dot Matrix
;*****************************************************************

IncrementRow:
	MOV A, Dot-Current-Bits
	RL A
	MOV Dot-Current-Bits, A
	MOV A, Dot-Current-Row
	INC A
	CJNE A, #8, FinishIncRow
	MOV A, #0
FinishIncRow:
	MOV Dot-Current-Row, A
	RET

;*****************************************************************
; Assign the green pattern to the DotMatrix-Green Register
;*****************************************************************

AssignDOTGreen:
	MOV A, #Dot-Green-Matrix
	ADD A, Dot-Current-Row
	MOV R1, A
	MOV A, @R1
	
	MOV DPTR, #DotMatrix-Green
	MOVX @DPTR, A
	RET

;*****************************************************************
; Assign the red pattern to the DotMatrix-Red Register
;*****************************************************************

AssignDOTRed:
	MOV A, #Dot-Red-Matrix
	ADD A, Dot-Current-Row
	MOV R1, A
	MOV A, @R1

	MOV DPTR, #DotMatrix-Red
	MOVX @DPTR, A
	RET

;*****************************************************************
; Assign the row pattern to the DotMatrix-Row Register
;*****************************************************************

AssignDOTROW:
	MOV DPTR, #DotMatrix-Row
	MOV A, Dot-Current-Bits
	MOVX @DPTR, A
	RET

;*****************************************************************
; Clear the row pattern to the DotMatrix-Row Register
;*****************************************************************

ClearDOTROW:
	MOV DPTR, #DotMatrix-Row
	MOV A, #0
	MOVX @DPTR, A
	RET

;*****************************************************************
; Wait while the Dot Matrix is Displayed
;*****************************************************************

DelayDotOn:
	MOV R2, #DotOnDelay1
DelayDot1:	MOV R3, #DotOnDelay2
DelayDot2:	DJNZ R3, DelayDot2
	DJNZ R2, DelayDot1
	RET

;*****************************************************************
; These are the Green Dots of the Baseball Field on the Dot Matrix
;*****************************************************************

FieldGreenDots:

DB 11111111b
DB 11111111b
DB 10111011b
DB 11111111b
DB 11111111b
DB 11111111b
DB 10111011b
DB 11111111b

;*****************************************************************
; These are the Red Dots of the Baseball Field on the Dot Matrix
;*****************************************************************

FieldRedDots:

DB 01000000b
DB 01111100b
DB 00111010b
DB 01000110b
DB 01010110b
DB 01000110b
DB 11111011b
DB 01000000b

;*****************************************************************
; These are the Green Dots of the Baseball Field on the Dot Matrix
;*****************************************************************

PlateGreenDots:

DB 11111111b
DB 11111111b
DB 11111111b
DB 11111111b
DB 11111111b
DB 11111111b
DB 11111111b
DB 11111111b

;*****************************************************************
; These are the Red Dots of the Baseball Field on the Dot Matrix
;*****************************************************************

PlateRedDots:

DB 00100000b
DB 01110000b
DB 11100000b
DB 11010000b
DB 10111010b
DB 00110111b
DB 00001110b
DB 00011100b


;=================================================================
;=================================================================
;                      DISPLAY PROCEDURES
;=================================================================
;=================================================================

DisplayAll:
	CALL DisplayBases
	CALL DisplayScores
	CALL DispBallStrkOut
	CALL DisplayAtBat
	CALL DisplayInningNum
	RET

;*****************************************************************
; Copy the Dots showing the Bases to the Dot Matrix
;*****************************************************************

DisplayBases:
	CALL CopyFieldToDotM
	CALL CopyBasesToDotM
	RET

CopyFieldToDotM:
	CALL CopyFieldToGreen
	CALL CopyFieldToRed
	RET
	
;*****************************************************************
; Copy the Green Dots showing the Field to the Dot Matrix 
;*****************************************************************

CopyFieldToGreen:
	MOV DPTR, #FieldGreenDots
	MOV R1, #Dot-Green-Matrix
	CALL CopyMemoryToDotM
	RET

;*****************************************************************
; Copy the Red Dots showing the Field to the Dot Matrix
;*****************************************************************

CopyFieldToRed:
	MOV DPTR, #FieldRedDots
	MOV R1, #Dot-Red-Matrix
	CALL CopyMemoryToDotM
	RET

;*****************************************************************
; Copy the Dots showing Home Plate to the Dot Matrix
;*****************************************************************

DisplayPlate:
	CALL CopyPlateToGreen
	CALL CopyPlateToRed
	RET
	
;*****************************************************************
; Copy the Green Dots showing Home Plate to the Dot Matrix 
;*****************************************************************

CopyPlateToGreen:
	MOV DPTR, #PlateGreenDots
	MOV R1, #Dot-Green-Matrix
	CALL CopyMemoryToDotM
	RET

;*****************************************************************
; Copy the Red Dots showing Home Plate to the Dot Matrix
;*****************************************************************

CopyPlateToRed:
	MOV DPTR, #PlateRedDots
	MOV R1, #Dot-Red-Matrix
	CALL CopyMemoryToDotM
	RET

;*****************************************************************
; Copy from Memory to the Dot Matrix 
; Input: DPTR = Source Memory Address
;        R1 = Dot Matrix Memory Address
;*****************************************************************

CopyMemoryToDotM:
	MOV R2, #0
CopyDotMatrixRow:
	MOV A, R2
	MOVC A, @A+DPTR
	MOV @R1, A
	INC R1
	INC R2
	CJNE R2, #8, CopyDotMatrixRow
	RET

;*****************************************************************
; Display Scores
;*****************************************************************

DisplayScores:
	MOV A, Score-Player1
	MOV DPTR, #LED-7-Segment1
	MOVX @DPTR, A
	MOV A, #0FFh
	MOV DPTR, #LED-7-Segment2
	MOVX @DPTR, A
	MOV A, Score-Player2
	MOV DPTR, #LED-7-Segment3
	MOVX @DPTR, A
	RET

	
;*****************************************************************
; Display the number of Balls, Strikes, and Outs on the Port Diode
;*****************************************************************

DispBallStrkOut:
	MOV R0, Ball-Count
	MOV R1, Strike-Count
	MOV R2, Out-Count

	MOV A, #0

CountBalls:
	CJNE R0, #0, AddBall1
	JMP CountStrikes
AddBall1:	
	ORL A, #01000000b
	CJNE R0, #1, AddBall2
	JMP CountStrikes
AddBall2:	
	ORL A, #00100000b
	CJNE R0, #2, AddBall3
	JMP CountStrikes
AddBall3:	
	ORL A, #00010000b

CountStrikes:
	CJNE R1, #0, AddStrike1
	JMP CountOuts
AddStrike1:	
	ORL A, #00001000b
	CJNE R1, #1, AddStrike2
	JMP CountOuts
AddStrike2:	
	ORL A, #00000100b

CountOuts:	
	CJNE R2, #0, AddOut1
	JMP FinishBSO
AddOut1:	
	ORL A, #00000010b
	CJNE R2, #1, AddOut2
	JMP FinishBSO
AddOut2:	
	ORL A, #00000001b

FinishBSO:

	XRL A, #11111111b
	MOV P1, A
	RET

;*****************************************************************
; Display Player At Bat
;*****************************************************************

DisplayAtBat:
	MOV A, Player-At-Bat
	CJNE A, #1, Player2AtBat
	CALL ShowMsgPlayerJH
	RET
Player2AtBat:
	CALL ShowMsgPlayerYJ
	RET


;*****************************************************************
; Display Inning Number
;*****************************************************************

DisplayInningNum:
	MOV A, Inning-Number
	
CheckInning1:
	CJNE A, #1, CheckInning2
	MOV A, #0F9h
	JMP FinishInnDisp
CheckInning2:
	CJNE A, #2, CheckInning3
	MOV A, #0A4h
	JMP FinishInnDisp
CheckInning3:
	CJNE A, #3, CheckInning4
	MOV A, #0B0h
	JMP FinishInnDisp
CheckInning4:
	CJNE A, #4, CheckInning5
	MOV A, #099h
	JMP FinishInnDisp
CheckInning5:
	CJNE A, #5, CheckInning6
	MOV A, #092h
	JMP FinishInnDisp
CheckInning6:
	CJNE A, #6, CheckInning7
	MOV A, #082h
	JMP FinishInnDisp
CheckInning7:
	CJNE A, #7, CheckInning8
	MOV A, #0D8h
	JMP FinishInnDisp
CheckInning8:
	CJNE A, #8, CheckInning9
	MOV A, #080h
	JMP FinishInnDisp
CheckInning9:
	CJNE A, #9, CheckInning0
	MOV A, #090h
	JMP FinishInnDisp
CheckInning0:
	MOV A, #0C0h

FinishInnDisp:
	MOV DPTR, #Seven-Segment
	MOVX @DPTR, A
	RET


;*****************************************************************
; Copy the Bases to the Dot Matrix
;*****************************************************************

CopyBasesToDotM:
	CALL CopyBase1ToDotM
	CALL CopyBase2ToDotM
	CALL CopyBase3ToDotM
	RET

CopyBase1ToDotM:
	MOV A, Base1-Occupied
	CJNE A, #0, AddBase1
	JMP RemoveBase1
CopyBase2ToDotM:
	MOV A, Base2-Occupied
	CJNE A, #0, AddBase2
	JMP RemoveBase2
CopyBase3ToDotM:
	MOV A, Base3-Occupied
	CJNE A, #0, AddBase3
	JMP RemoveBase3

;*****************************************************************
; Add or Remove a runner at Base1,2,3
;*****************************************************************

AddBase1:
	CALL WriteBase1Red
	RET
RemoveBase1:
	CALL EraseBase1Red
	RET

AddBase2:
	CALL WriteBase2Red
	RET
RemoveBase2:
	CALL EraseBase2Red
	RET

AddBase3:
	CALL WriteBase3Red
	RET
RemoveBase3:
	CALL EraseBase3Red
	RET

;*****************************************************************
; Write or Erase the Red or Green colors of Base1,2,3
;*****************************************************************

WriteBase1Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base1Row
	MOV R1, A
	MOV A, @R1
	ORL A, #Base1Bits
	MOV @R1, A
	RET
EraseBase1Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base1Row
	MOV R1, A
	MOV A, @R1
	CPL A
	ORL A, #Base1Bits
	CPL A
	MOV @R1, A
	RET

WriteBase2Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base2Row
	MOV R1, A
	MOV A, @R1
	ORL A, #Base2Bits
	MOV @R1, A
	RET
EraseBase2Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base2Row
	MOV R1, A
	MOV A, @R1
	CPL A
	ORL A, #Base2Bits
	CPL A
	MOV @R1, A
	RET

WriteBase3Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base3Row
	MOV R1, A
	MOV A, @R1
	ORL A, #Base3Bits
	MOV @R1, A
	RET
EraseBase3Red:
	MOV A, #Dot-Red-Matrix
	ADD A, #Base3Row
	MOV R1, A
	MOV A, @R1
	CPL A
	ORL A, #Base3Bits
	CPL A
	MOV @R1, A
	RET


;=================================================================
;=================================================================
;                       LCD Procedures
;=================================================================
;=================================================================


;*****************************************************************
; Clear the LCD.
; The message location is in Message-Lo-Byte & Message-Hi-Byte
; Input: None
;*****************************************************************

ClearLCD:
	MOV LCD-Instr-Mem, #Clear-LCD
	CALL WriteInstToLCD
	RET

;*****************************************************************
; Write a message to the LCD.
; The message location is in Message-Lo-Byte & Message-Hi-Byte
; Input:
;	Message-Lo-Byte
;	Message-Hi-Byte
;	Message-Length
;*****************************************************************

WriteMsgToLCD:
	MOV Char-Loc-Offset, #00H

WriteLCDLoop:

	CALL WriteMsgCharLCD
	MOV A, Char-Loc-Offset
	INC A
	MOV Char-Loc-Offset, A
	CJNE A, Message-Length, WriteLCDLoop

	RET

;*****************************************************************
; Write a character to the LCD at the cursor position
; Input:
;	Message-Lo-Byte
;	Message-Hi-Byte
;	Char-Loc-Offset
;*****************************************************************

WriteMsgCharLCD:
	MOV DPL, Message-Lo-Byte
	MOV DPH, Message-Hi-Byte
	MOV A, Char-Loc-Offset
	MOVC A, @A+DPTR
	MOV LCD-Data-mem, A

	CALL WriteDataToLCD

	RET

;*********************************************************************
; Print one Character on the LCD Screen. 
; The address of the character is stored in R1
; Input: R1
;*********************************************************************
WriteR1CharToLCD:
            MOV     LCD-Data-Mem, @R1
            CALL    WriteDataToLCD
            RET


;*****************************************************************
; Move the cursor 
; Input:
;	LCD-Cursor-Row
;	LCD-Cursor-Col
;*****************************************************************

MoveCurTopLeft:
	MOV LCD-Cursor-Row, #1
	MOV LCD-Cursor-Col, #4
	CALL MoveCursor
	RET

MoveCurTopCenter:
	MOV LCD-Cursor-Row, #1
	MOV LCD-Cursor-Col, #9
	CALL MoveCursor
	RET

MoveCurTopRight:
	MOV LCD-Cursor-Row, #1
	MOV LCD-Cursor-Col, #12
	CALL MoveCursor
	RET

MoveCurBotLeft:
	MOV LCD-Cursor-Row, #2
	MOV LCD-Cursor-Col, #2
	CALL MoveCursor
	RET

;*************************************************************************
; Move the cursor to the Row and Column numbers given in memory
; Input:
;	LCD-Cursor-Row
;	LCD-Cursor-Col
;*************************************************************************  
MoveCursor:
	MOV     A ,#Move-Cursor-LCD
	MOV     R2,LCD-Cursor-Row
	CJNE    R2,#01H, MoveToRow2
MoveToRow1:
	ADD     A ,#Line1-Address
	JMP     MoveToColumn                 
MoveToRow2:
	ADD     A ,#Line2-Address
MoveToColumn: 
	ADD     A ,LCD-Cursor-Col
	MOV     LCD-Instr-Mem,A 
	CALL    WriteInstToLCD
	RET

;*****************************************************************
; Write an Instruction from memory to the LCD 
; Input:
; 	LCD-Instr-mem
;*****************************************************************

WriteInstToLCD:
	CALL WaitForLCDReady ; Delay until LCD is ready

	MOV DPTR, #LCD-Write-Instr
	MOV A, LCD-Instr-mem
	MOVX @DPTR, A
	
	RET

;*****************************************************************
; Write from Data in memory to the LCD Data Register
; Input:
; 	LCD-Data-mem
;*****************************************************************

WriteDataToLCD:
	CALL WaitForLCDReady ; Delay until LCD is ready

	MOV DPTR, #LCD-Write-Data
	MOV A, LCD-Data-mem
	MOVX @DPTR, A

	RET

;*****************************************************************
; Wait until the LCD is ready
; Read from the LCD Instruction Register to the Accumulator
; If the LCD is not ready then read again.  Continue reading
; until the LCD is Ready
;*****************************************************************

WaitForLCDReady:
	    MOV DPTR, #LCD-Read-Instr
            MOVX A, @DPTR

            JB ACC.7, WaitForLCDReady
            RET



;=================================================================
;=================================================================
;                 Show Messages on the LCD
;=================================================================
;=================================================================


;*****************************************************************
; LCD Display Messages 
;*****************************************************************

ShowMsgPlayer1Up:
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlayer1Up
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #11
	CALL WriteMsgToLCD
	RET

ShowMsgPlayer2Up:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlayer2Up
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #11
	CALL WriteMsgToLCD
	RET

ShowMsgPlayerJH:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlayer
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #7
	CALL WriteMsgToLCD
	CALL ShowMsgJunghwan
	RET

ShowMsgPlayerYJ:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlayer
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #7
	CALL WriteMsgToLCD
	CALL ShowMsgYoujung
	RET

ShowMsgBall:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageBall
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #4
	CALL WriteMsgToLCD
	RET

ShowMsgWalk:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageWalk
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #6
	CALL WriteMsgToLCD
	RET

ShowMsgStrike:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageStrike
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #7
	CALL WriteMsgToLCD
	RET

ShowMsgStrikeOut:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageStrikeOut
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #12
	CALL WriteMsgToLCD
	RET

ShowMsgOut:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageOut
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #6
	CALL WriteMsgToLCD
	RET

ShowMsgPlayYagu:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlay
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #5
	CALL WriteMsgToLCD
	CALL ShowMsgYagu
	RET

ShowMsgSingle:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageSingle
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #7
	CALL WriteMsgToLCD
	RET

ShowMsgDouble:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageDouble
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #8
	CALL WriteMsgToLCD
	RET

ShowMsgTriple:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageTriple
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #9
	CALL WriteMsgToLCD
	RET

ShowMsgHomerun:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageHomerun
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #11
	CALL WriteMsgToLCD
	RET

ShowMsgSteal:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageSteal
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #6
	CALL WriteMsgToLCD
	RET

ShowMsgCantSteal:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageCantSteal
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #13
	CALL WriteMsgToLCD
	RET

ShowMsgSacHit:
	CALL ClearBottomMsg
	CALL MoveCurBotLeft
	MOV DPTR, #MessageSacHit
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #14
	CALL WriteMsgToLCD
	RET

ShowMsgPlayBall:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessagePlayBall
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #10
	CALL WriteMsgToLCD
	RET

ShowMsgGameOver:
	CALL ClearTopMsg
	CALL MoveCurTopLeft
	MOV DPTR, #MessageGameOver
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #11
	CALL WriteMsgToLCD
	RET

ClearTopMsg:
	CALL MoveCurTopLeft
	MOV DPTR, #MessageClear
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #12
	CALL WriteMsgToLCD
	RET

ClearBottomMsg:
	CALL MoveCurBotLeft
	MOV DPTR, #MessageClear
	MOV Message-Lo-Byte, DPL
	MOV Message-Hi-Byte, DPH
	MOV Message-Length, #12
	CALL WriteMsgToLCD
	RET



;***************************************************************
;*         Define Messages
;***************************************************************

MessagePlayer1Up:
DB 'P', 'l', 'a', 'y', 'e', 'r', ' ', '1', ' ', 'U', 'p'

MessagePlayer2Up:
DB 'P', 'l', 'a', 'y', 'e', 'r', ' ', '2', ' ', 'U', 'p'

MessagePlayer:
DB 'P', 'l', 'a', 'y', 'e', 'r', ' '

MessagePlay:
DB 'P', 'L', 'A', 'Y', ' '

MessageBall:
DB 'B', 'A', 'L', 'L'

MessageWalk:
DB 'W', 'A', 'L', 'K', '!', '!'

MessageStrike:
DB 'S', 'T', 'R', 'I', 'K', 'E', '!'

MessageStrikeout:
DB 'S', 'T', 'R', 'I', 'K', 'E', 'O', 'U', 'T', '!', '!', '!'

MessageOut:
DB 'O', 'U', 'T', '!', '!', '!'

MessageSingle:
DB 'S', 'I', 'N', 'G', 'L', 'E', '!'

MessageDouble:
DB 'D', 'O', 'U', 'B', 'L', 'E', '!', '!'

MessageTriple:
DB 'T', 'R', 'I', 'P', 'L', 'E', '!', '!', '!'

MessageHomerun:
DB 'H', 'O', 'M', 'E', 'R', 'U', 'N', '!', '!', '!', '!'

MessageSteal:
DB 'S', 'T', 'E', 'A', 'L', '!'

MessageCantSteal:
DB 'C', 'A', 'N', ' ', 'N', 'O', 'T', ' ', 'S', 'T', 'E', 'A', 'L'

MessageSacHit:
DB 'S', 'A', 'C', 'R', 'I', 'F', 'I', 'C', 'E', ' ', 'H', 'I', 'T', '!'

MessagePlayBall:
DB 'P', 'L', 'A', 'Y', ' ', 'B', 'A', 'L', 'L', '!'

MessageGameOver:
DB 'G', 'A', 'M', 'E', ' ', 'O', 'V', 'E', 'R', '!', ' '

MessageClear:
DB ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '



;=================================================================
;=================================================================
;                 Show Hangul on the LCD
;=================================================================
;=================================================================

;*********************************************************************
; Print a Hangul Message
;*********************************************************************

ShowMsgAnyeong:
	CALL StartHangul
	CALL InstallAnyeong
	CALL MoveCurTopCenter
	CALL PrintAnyeong
	RET

ShowMsgYagu:
	CALL StartHangul
	CALL InstallYagu
	CALL MoveCurTopCenter
	CALL PrintYagu
	RET

ShowMsgWagner:
	CALL StartHangul
	CALL InstallWagner
	CALL MoveCurTopRight
	CALL PrintWagner
	RET

ShowMsgJunghwan:
	CALL StartHangul
	CALL InstallJunghwan
	CALL MoveCurTopRight
	CALL PrintJunghwan
	RET

ShowMsgYoujung:
	CALL StartHangul
	CALL InstallYoujung
	CALL MoveCurTopRight
	CALL PrintYoujung
	RET

;*********************************************************************
; Prepare for Displaying Hangul on the LCD
;*********************************************************************

StartHangul:
	CALL    Send-CGRAM-Start
	MOV     CGRAM-Address, #00H
	RET

;*********************************************************************
; Prepare to send font data to CGRAM memory in the LCD
; Set the CGRAM Address to the beginning of CGRAM Memory
;*********************************************************************
Send-CGRAM-Start:
            MOV     LCD-Instr-Mem, #Start-CGRAM-LCD 
            CALL    WriteInstToLCD          
            RET


;*********************************************************************
; Install each Hangul Character into the LCD Screen
; There can be a maxiumum of 8 character fonts installed
;*********************************************************************

InstallAnyeong:
	CALL Install-AN
	CALL Install-YO
	CALL Install-NYEONG
	CALL Install-HA
	CALL Install-SE
	RET

InstallYagu:
	CALL Install-YA
	CALL Install-GU
	RET

InstallWagner:
	CALL Install-WA
	CALL Install-GEU
	CALL Install-NEO
	RET

InstallJunghwan:
	CALL Install-JUNG
	CALL Install-HWAN
	RET

InstallYoujung:
	CALL Install-YOU
	CALL Install-JUNG
	RET

;*********************************************************************
; Print a Hangul Message
;*********************************************************************

PrintAnyeong:
	MOV     R1,#Char-Code-AN
	CALL    WriteR1CharToLCD
	MOV     R1,#Char-Code-NYEONG
	CALL    WriteR1CharToLCD
	MOV     R1,#Char-Code-HA
	CALL    WriteR1CharToLCD
	MOV     R1,#Char-Code-SE
	CALL    WriteR1CharToLCD
	MOV     R1,#Char-Code-YO
	CALL    WriteR1CharToLCD
	RET

PrintYagu:
	MOV	R1,#Char-Code-YA
	CALL	WriteR1CharToLCD
	MOV	R1,#Char-Code-GU
	CALL	WriteR1CharToLCD
	RET

PrintWagner:
	MOV	R1,#Char-Code-WA
	CALL	WriteR1CharToLCD
	MOV	R1,#Char-Code-GEU
	CALL	WriteR1CharToLCD
	MOV	R1,#Char-Code-NEO
	CALL	WriteR1CharToLCD
	RET

PrintJunghwan:
	MOV	R1,#Char-Code-JUNG
	CALL	WriteR1CharToLCD
	MOV	R1,#Char-Code-HWAN
	CALL	WriteR1CharToLCD
	RET

PrintYoujung:
	MOV	R1,#Char-Code-YOU
	CALL	WriteR1CharToLCD
	MOV	R1,#Char-Code-JUNG
	CALL	WriteR1CharToLCD
	RET

;*********************************************************************
; Install a hangul character into LCD CGRAM Memory
; Input: CGRAM-Address
;*********************************************************************
Install-AN:
            MOV DPTR, #FONT_AN
            MOV R1, #Char-Code-AN
            JMP InstallChar
Install-IN:
            MOV DPTR, #FONT_IN
            MOV R1, #Char-Code-IN
            JMP InstallChar
Install-YO:
            MOV DPTR, #FONT_YO
            MOV R1, #Char-Code-YO
            JMP InstallChar
Install-NYEONG:
            MOV DPTR, #FONT_NYEONG
            MOV R1, #Char-Code-NYEONG
            JMP InstallChar
Install-HA:
            MOV DPTR, #FONT_HA
            MOV R1, #Char-Code-HA
            JMP InstallChar
Install-SE:
            MOV DPTR, #FONT_SE
            MOV R1, #Char-Code-SE
            JMP InstallChar
Install-GU:
            MOV DPTR, #FONT_GU
            MOV R1, #Char-Code-GU
            JMP InstallChar
Install-GEU:
            MOV DPTR, #FONT_GEU
            MOV R1, #Char-Code-GEU
            JMP InstallChar
Install-WA:
            MOV DPTR, #FONT_WA
            MOV R1, #Char-Code-WA
            JMP InstallChar
Install-YA:
            MOV DPTR, #FONT_YA
            MOV R1, #Char-Code-YA
            JMP InstallChar
Install-NEO:
            MOV DPTR, #FONT_NEO
            MOV R1, #Char-Code-NEO
            JMP InstallChar

Install-JUNG:
            MOV DPTR, #FONT_JUNG
            MOV R1, #Char-Code-JUNG
            JMP InstallChar

Install-HWAN:
            MOV DPTR, #FONT_HWAN
            MOV R1, #Char-Code-HWAN
            JMP InstallChar

Install-YOU:
            MOV DPTR, #FONT_YOU
            MOV R1, #Char-Code-YOU
            JMP InstallChar

Install-BLANK:
            MOV DPTR, #FONT_BLANK
            MOV R1, #Char-Code-BLANK
            JMP InstallChar

InstallChar:
           MOV @R1, CGRAM-Address
           INC CGRAM-Address
           CALL InstalCharFont
           RET
;*********************************************************************
; Install one character font into the LCD CGRAM Memory
; Input: DPTR
;*********************************************************************
InstalCharFont:
            MOV     Font-Lo-Byte, DPL
            MOV     Font-Hi-Byte, DPH
            MOV     Font-Row-Num, #00H
NextRow:
            CALL    Install-FONT_ROW     
            INC     Font-Row-Num         
            MOV     A, Font-Row-Num
            CJNE    A, #Font-Row-Max, NextRow
            RET

;*********************************************************************
; Install one row of a character font into the LCD Screen
; Input: Font-Lo-Byte, Font-Hi-Byte, Font-Row-Num
;*********************************************************************
Install-FONT_ROW:
                  MOV     DPL, Font-Lo-Byte
                  MOV     DPH, Font-Hi-Byte
                  MOV     A, Font-Row-Num
                  MOVC    A, @A+DPTR                 
                  MOV     LCD-Data-Mem, A
                  CALL    WriteDataToLCD
                  RET        

;***************************************************************
;*         Define Fonts
;***************************************************************


FONT_AN:
            DB 00000010B
            DB 00001010B
            DB 00010110B
            DB 00001011B
            DB 00000010B
            DB 00010010B
            DB 00010000B
            DB 00011110B
FONT_YO:
            DB 00000000B
            DB 00000100B
            DB 00001010B
            DB 00000100B
            DB 00001010B
            DB 00001010B
            DB 00011111B
            DB 00000000B
FONT_WA:
            DB 00001001B
            DB 00010101B
            DB 00001001B
            DB 00000001B
            DB 00011111B
            DB 00001001B
            DB 00001011B
            DB 00000001B
FONT_GA:
            DB 00000010B
            DB 00011110B
            DB 00000110B
            DB 00000111B
            DB 00001010B
            DB 00010010B
            DB 00000010B
            DB 00000000B
FONT_NYEONG:
            DB 00000001B
            DB 00010001B
            DB 00010011B
            DB 00011101B
            DB 00000011B
            DB 00000101B
            DB 00001010B
            DB 00000100B
FONT_HA:
            DB 00000010B
            DB 00001010B
            DB 00011110B
            DB 00001011B
            DB 00010110B
            DB 00001010B
            DB 00000010B
            DB 00000000B
FONT_SE:
            DB 00000001B
            DB 00000011B
            DB 00001011B
            DB 00001111B
            DB 00001011B
            DB 00010111B
            DB 00000011B
            DB 00000001B
FONT_IN:
            DB 00000001B
            DB 00001001B
            DB 00010101B
            DB 00001001B
            DB 00000001B
            DB 00010000B
            DB 00011110B
            DB 00000000B
FONT_GU:
            DB 00000000B
            DB 00011110B
            DB 00000010B
            DB 00000000B
            DB 00011111B
            DB 00000100B
            DB 00000100B
            DB 00000000B
FONT_GEU:
            DB 00000000B
            DB 00011110B
            DB 00000010B
            DB 00000010B
            DB 00000010B
            DB 00000000B
            DB 00011111B
            DB 00000000B
FONT_YA:
            DB 00000000B
            DB 00000010B
            DB 00001011B
            DB 00010110B
            DB 00001011B
            DB 00000010B
            DB 00000010B
            DB 00000000B
FONT_NEO:
            DB 00000000B
            DB 00000001B
            DB 00010001B
            DB 00010011B
            DB 00010001B
            DB 00011101B
            DB 00000001B
            DB 00000000B

FONT_JUNG:
            DB 00011101B
            DB 00001011B
            DB 00001101B
            DB 00010101B 
            DB 00010101B
            DB 00000110B
            DB 00001001B
            DB 00000110B

FONT_HWAN:
            DB 00001100B
            DB 00011110B
            DB 00001010B
            DB 00010111B
            DB 00001010B
            DB 00011110B
            DB 00001000B
            DB 00001111B

FONT_YOU:
            DB 00000000B
            DB 00001110B
            DB 00011011B
            DB 00001110B
            DB 00000000B
            DB 00011111B
            DB 00001010B
            DB 00001010B

FONT_BLANK:
            DB 00000000B
            DB 00000000B
            DB 00000000B
            DB 00000000B
            DB 00000000B
            DB 00000000B
            DB 00000000B
            DB 00000000B


END
