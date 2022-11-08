INCLUDE Irvine32.inc

LoadSelection  PROTO pTable:PTR DWORD, pNumberOfEntries:DWORD, pEntrySize:DWORD, pChar:BYTE
Display        PROTO pGrid:PTR DWORD, pColSize:DWORD,      pRowSize:DWORD,  pAliveChar:BYTE
Wrap           PROTO pColSize:WORD,   pRowSize:WORD,       pGrid:PTR DWORD
MarkRowGut     PROTO pDeadChar:BYTE,  pAliveChar:BYTE,     pColSize:WORD,   pRowSize:WORD,   pGrid:PTR DWORD, ptempGrid:PTR DWORD 
MarkColGut     PROTO pDeadChar:BYTE,  pAliveChar:BYTE,     pColSize:WORD,   pRowSize:WORD,   pGrid:PTR DWORD, ptempGrid:PTR DWORD  
CountNeighbors PROTO pXPos:WORD,      pYPos:WORD,          pAliveChar:BYTE, pGrid:PTR DWORD
CopyGrid       PROTO pGridSize:DWORD, ptempGrid:PTR DWORD, pGrid:PTR DWORD 


.386
.model flat, stdcall
.stack 4096
ExitProcess PROTO, deExitCode:DWORD 

.data 
Tracker BYTE 0

.code 
main PROC

	;Changing these values would change the size before run
	RowSize = 55
	ColSize = 70

	;237
	
	.data 
	grid	      BYTE RowSize * ColSize Dup('0')
	tempGrid      BYTE RowSize * ColSize Dup('0')

	ActualRowSize BYTE RowSize - 2;
	ActualColSize BYTE ColSize - 2;

	AliveChar     BYTE '1'
	DeadChar      BYTE '0'
	
	currentRow    WORD ?
	currentCol    WORD ?

	promptLoad    BYTE "1)Blinker  2)Toad  3)Beacon  4)Pulsar  5)Pentadecathlon  6)Glider  7)Heavyweight starship  8)Self Created Choas", 0 

	table         BYTE '1'
				  DWORD Blinker
	EntrySize = ($ - table)
				  BYTE '2'
				  DWORD Toad 
				  BYTE '3'
				  DWORD Beacon 
				  BYTE '4'
				  DWORD Pulsar 
				  BYTE '5' 
				  DWORD Pentadecathlon
				  BYTE '6'
				  DWORD Glider
				  BYTE '7'
				  DWORD HeavyweightStarship
				  Byte '8'
				  DWORD Init
	NumberOfEntries = ($ - table)/ EntrySize
;------------------------------------------------------
	.code 

	mov eax, 0
	mov ebx, 0 


	mov edx, OFFSET promptLoad
	call WriteString
	call ReadChar
	call ClrScr
	
	movzx edx,al
	INVOKE LoadSelection, ADDR table , NumberOfEntries, EntrySize, dl

;------------------------------------------------------

	Run:
		INVOKE Display, ADDR grid, ColSize, RowSize, AliveChar
		INVOKE Wrap, ColSize, RowSize, ADDR grid 

		;Marking row gutters 
		INVOKE MarkRowGut, DeadChar, AliveChar, ColSize, RowSize, ADDR grid, ADDR tempGrid
		INVOKE MarkColGut, DeadChar, AliveChar, ColSize, RowSize, ADDR grid, ADDR tempGrid

		movzx ecx, ActualRowSize
		mov ax, 1
		mov bx, 1

		GoThrRow:
			push ecx
			movzx ecx, ActualColSize

			GoThrCol:
				push ecx

				mov currentRow, ax
				mov currentCol, bx
				INVOKE CountNeighbors, currentRow, currentCol, AliveChar, ADDR grid
				mov ax, currentRow
				mov bx, currentCol 


				;call CountNeighbors
				mov esi, OFFSET tempGrid			
				mov edi, OFFSET grid				
				push esi						;temp
				push edi						;grid 
				call MarkTemp

				inc bx
				pop ecx
			loop GoThrCol
		
			pop ecx
			inc ax
			mov ebx, 1 
		loop GoThrRow

		;copy tempGrid to actual display grid 
		INVOKE CopyGrid, (RowSize * ColSize), ADDR tempGrid, ADDR grid 
		
		jmp Run

	Invoke ExitProcess, 0 

main ENDP

;------------------------------------------------------

;PROCEDURES 

;----------------------------
LoadSelection Proc, pTable:PTR DWORD , pNumberOfEntries:DWORD, pEntrySize:DWORD, pChar:BYTE
	
	mov al, pChar
	mov ebx, OFFSET table 
	mov ecx, NumberOfEntries

	L1:
		cmp al, [ebx]
		jne L2 
		call NEAR PTR [ebx + 1]
		jmp L3

	L2:
		add ebx, pEntrySize 
		loop L1 

	L3:

	ret
LoadSelection ENDP


;----------------------------
;-----------------
;1st: temp grid 28
;2nd: grid      24
;3rd: Row       20
;4th: Col       16
;5th: Alive     12
;6th: Dead      8 
;-----------------
MarkColGut Proc, pDeadChar:BYTE, pAliveChar:BYTE, pColSize:WORD, pRowSize:WORD, pGrid:PTR DWORD, ptempGrid:PTR DWORD  
	
	;MARKING COLUMN ZERO
	mov ax, 1 
	mov bx, 0 
	
	mov ecx, 2 
	push ecx

	movzx ecx, pRowSize						;Rows 
	sub ecx, 2

	L1:
		mov dl, 0
		push ecx 
		call Read 
		cmp cl, pAliveChar
		jne Cont 
		dec dl 

		Cont:
			mov ecx, 3
			push eax
			push ebx 
			dec ax 
		L2:	
			push ebx 
			push ecx
			mov ecx, 2

			L3:
				push ecx
				mov esi, pGrid			;Grid offset
				call Read
				cmp cl, pAliveChar			;Alive char
				jne next
				inc dl 

				next:
					inc bx					
					pop ecx
			loop L3
			
			pop ecx
			pop ebx 
			inc ax 
		loop L2 
			pop ebx 
			pop eax

			;check dl value
			mov esi, ptempGrid
			mov edi, pGrid
			push esi
			push edi
			call MarkTemp
			inc ax							;move to next row 
		pop ecx 
		loop L1
	
	pop ecx 
	dec ecx
	jecxz Quit

	LastCol: 
		;Marking Right most Column 
		push ecx
		mov ax, 1
		mov bx, pColSize						;Amount of columns 
		sub bx, 2
		movzx ecx, pRowSize						;Row (going down)
		sub ecx, 2
		jmp L1

	Quit:


	ret 
MarkColGut ENDP

;---------------------------------

;MarkRowGut Procedure 
;-----------------
;Marks the pieces onto temp for both gutter rows
;1st: temp grid 28
;2nd: grid      24
;3rd: Row       20
;4th: Col       16
;5th: Alive     12
;6th: Dead      8 
;-----------------
MarkRowGut Proc, pDeadChar:BYTE, pAliveChar:BYTE, pColSize:WORD, pRowSize:WORD, pGrid:PTR DWORD, ptempGrid:PTR DWORD 
	
	;Marking row 0
	mov ax, 0 
	mov bx, 1 
	
	mov ecx, 2							;used for looping through first row and then last row (2)
	push ecx 

	movzx ecx, pColSize
	sub ecx, 2

	L1:
		mov dl, 0
		push ecx 
		call Read 
		cmp cl, pAliveChar
		jne Cont 
		dec dl 

		Cont:
			mov ecx, 2
			push eax
			push ebx 
			dec bx 
		L2:	
			push ebx 
			push ecx
			mov ecx, 3 

			L3:
				push ecx
				mov esi, pGrid
				call Read
				cmp cl, pAliveChar
				jne next
				inc dl 

				next:
					inc bx
					pop ecx
				loop L3
			
			pop ecx
			pop ebx 
			inc ax 
			loop L2 
		pop ebx 
		pop eax

		;check dl value
		mov esi, ptempGrid
		mov edi, pGrid
		push esi
		push edi
		call MarkTemp
		

		inc bx
		pop ecx 
	loop L1

	pop ecx 
	dec ecx
	jecxz Quit
	
	;Marking Bottom Row 
	LastRow:
		push ecx
		mov ax, pRowSize
		sub ax, 2
		mov bx, 1
		movzx ecx, pColSize
		sub ecx, 2
		jmp L1 

	Quit:

	ret 
MarkRowGut ENDP


;Wrap Procedure 
;---------------
;1st: offset of grid first 
;2nd: RowSize
;3rd: ColSize 
;---------------
Wrap Proc, pColSize:WORD, pRowSize:WORD, pGrid:PTR DWORD
	
	;COPYING Actual TOP TO BOTTOM
	;SETTING UP ESI FOR FIRST ROW (COPIED)
	mov esi, pGrid					
	movzx ecx, pColSize				
	sub ecx, 2
	mov ax, 1
	mov bx, 1
	mov dx, pColSize			
	mul dx
	add ax, bx
	add esi, eax
	
	;SETTING UP EDI FOR LAST ROW (COPY TO)
	mov ax, pRowSize			
	sub ax, 1
	mov edi, pGrid		
	mov dx, pColSize	
	mul dx
	add ax, bx 
	add edi, eax 

	CopyTop2Bot:
		push ecx 
		mov cl, [esi]
		mov [edi], cl
		inc esi 
		inc edi 
		pop ecx
		loop CopyTop2Bot

	;COPYING BOTTOM TO TOP
	mov ax, pRowSize				
	sub ax, 2
	mov bx, 1
	movzx ecx, pColSize					
	sub ecx, 2
	mov esi, pGrid		
	mov dx, pColSize	
	mul dx 
	add ax, bx
	add esi, eax

	mov ax, 0 
	mov bx, 1
	mov edi, pGrid	
	add ax, bx 
	add edi, eax

	CopyBot2Top:
		push ecx
		mov cl, [esi]
		mov [edi], cl
		inc esi
		inc edi
		pop ecx
		loop CopyBot2Top


	;Copying left col to right col 
	mov ax, 2
	mov bx, 1
	mov esi, pGrid	
	mov dx, pColSize	
	mul dx 
	add ax, bx 
	add esi, eax

	mov ax, 2
	mov bx, pColSize
	dec bx
	mov edi, pGrid	
	mov dx, pColSize	
	mul dx
	add ax, bx
	add edi, eax
	movzx ecx, pRowSize
	sub ecx, 4	
	

	CopyLeft2Right:
		push ecx
		mov cl, [esi]
		mov [edi], cl

		mov ax, pColSize
		add esi, eax
		add edi, eax
		pop ecx
		loop CopyLeft2Right

	;Copying Right to left 
	mov ax, 2
	mov bx, pColSize
	sub bx, 2
	mov esi, pGrid	
	mov dx, pColSize	
	mul dl 
	add ax, bx 
	add esi, eax

	mov ax, 2
	mov bx, 0
	mov edi, [ebp + 16]
	mov dx, pColSize	
	mul dl 
	add ax, bx 
	add edi, eax

	movzx ecx, pRowSize
	sub ecx, 4	

	CopyRight2Left:
		push ecx
		mov cl, [esi]
		mov [edi], cl

		mov ax, pColSize
		add esi, eax
		add edi, eax

		pop ecx
		loop CopyRight2Left

	ret 
Wrap ENDP 

;-----------------------------------------

;CopyGrid Procedure 
;---------------
;ESI = grid copied to
;EDI = temp grid
;---------------
CopyGrid Proc, pGridSize:DWORD, ptempGrid:PTR DWORD, pGrid:PTR DWORD 
	
	mov ecx, pGridSize
	mov edi, ptempGrid
	mov esi, pGrid

	L1:
		mov dl, BYTE PTR [edi]
		mov BYTE PTR [esi], dl
		inc esi
		inc edi 

	loop L1

	ret
CopyGrid ENDP 

;-----------------------------------------

;CountNeighbors Procedure 
;---------------
;AX = row, BX = col 
;return number of neighbors in dl  
;---------------
CountNeighbors PROC USES eax ebx, pXPos:WORD, pYPos:WORD, pAliveChar:BYTE, pGrid:PTR DWORD
				LOCAL Counter:BYTE
	

	.code 
	mov Counter, 0
	mov ax, pXPos
	mov bx, pYPos

	mov dl, 0 					;set neighbor counte to 0
	mov esi, pGrid
	call Read
	cmp cl, pAliveChar 
	jne Start
	dec dl

	Start:

	;moves to top left of current piece
	dec ax
	dec bx
	mov ecx, 9					

	L1:
		cmp Counter, 3			
		je NextLine
		
		push ecx
		mov esi, pGrid
		call Read
		cmp cl, pAliveChar 
		je AddNeighbor

		pop ecx 
		inc Counter 
		inc bx 

		loop L1 
	
	jmp Quit
	
	NextLine:
		sub bx, 3
		inc ax 
		mov Counter, 0
		jecxz Quit
		jmp L1 
	

	AddNeighbor:
		inc dl				;add neighbor counter (will be returned at end of procedure)
		inc bx				;mov to next column
		inc Counter			
		pop ecx				;restore ecx counter(loop)
		dec ecx				
		jecxz Quit
		jmp L1
	

	Quit:
		
	ret
CountNeighbors ENDP

;-----------------------------------------

;-------------
;1st: Temp Grid
;2nd: Grid 
;-------------
MarkTemp Proc 
	
	push ebp
	mov ebp, esp

	cmp dl, 1 
	jbe Die
	cmp dl, 4 
	jae Die 
	cmp dl, 2
	je StaySame 
	cmp dl, 3
	je Alive 

	StaySame:
		mov esi, [ebp + 8]
		call Read
		COMMENT %
		cmp cl, AliveChar 
		jne LeaveCheck
		%
					
		mov esi, [ebp + 12]
		call Write 
		jmp LeaveCheck
					
	Alive:
		mov esi, [ebp + 12] 
		mov cl, AliveChar 
		call Write
		jmp LeaveCheck

	Die:
		mov esi, [ebp + 12]
		mov cl, DeadChar
		call Write

		LeaveCheck:

		pop ebp 
	ret 8
MarkTemp ENDP

;-----------------------------------------

;Read Procedure 
;--------------
;AL = row, BL = col
;Return CL (char)
;---------------
Read PROC USES esi eax ebx edx
	
	;esi + ((row * ColSize) + col)
	mov dl, ColSize
	mul dl
	add ax, bx
	add esi, eax
	mov cl, [esi]

	ret 
Read ENDP 

;-----------------------------------------

;Write Procedure 
;---------------
;ESI = grid array
;AL = row, BL = col
;CL = character
;---------------
Write PROC
	
	pushad

	;esi + ((row * ColSize) + col) 
	mov edx, ColSize 
	mul edx
	add ax, bx
	add esi, eax
	mov BYTE PTR [esi], cl
	
	popad

	ret
Write ENDP 

;-----------------------------------------

;Display Procedure 
;---------------
Display PROC, pGrid:PTR DWORD, pColSize:DWORD, pRowSize:DWORD, pAliveChar:BYTE
	
	pushad 

	mov esi, pGrid 
	mov ecx, pRowSize 

	Row:
		push ecx 
		mov ecx, pColSize 
		
		Column:
			mov al, [esi]

			push eax 
			cmp al, pAliveChar 
			jne Dead
			mov eax, 14 + (14 * 16)
			call SetTextColor 
			jmp Cont 
			
			Dead:
				mov eax, 0
				call SetTextColor

			Cont:
			pop eax 
	
			call WriteChar

			inc esi
		loop Column

			pop ecx 
			cmp ecx, 0
			je LeaveProc
			call Crlf 
		loop Row

	LeaveProc:
		mov dh, 0
		mov dl, 0
		call GotoXY

	popad 
	ret
Display ENDP 

;-----------------------------------------

;Procedures for loading specific patterns 
;------------------------------------------------------

Blinker PROC 

	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 3
	mov ebx, 3
	call Write 
	mov eax, 4
	mov ebx, 3
	call Write 
	mov eax, 5
	mov ebx, 3
	call Write 

	ret
Blinker ENDP

;------------------------------------------------------
	
Toad Proc 
	
	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 5
	mov ebx, 5
	call Write 
	inc ebx 
	call Write 
	inc ebx 
	call Write 
	mov eax, 4
	mov ebx, 6
	call Write 
	inc ebx
	call Write 
	inc ebx 
	call Write 

	ret
Toad ENDP

;------------------------------------------------------

Beacon Proc 

	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 10 
	mov ebx, 10 
	call Write 
	inc ebx 
	call Write 
	dec ebx 
	inc eax 
	call Write 
	mov eax, 13
	mov ebx, 12
	call Write 
	inc ebx
	call Write 
	dec eax
	call Write

	ret
Beacon ENDP 

;------------------------------------------------------	

Pulsar Proc
	
	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

;Top Left
	mov al, 4
	mov bl, 5
	call Write 
	inc bx
	call Write 
	inc bx
	call Write 
	mov al, 6
	mov bl, 3
	call Write 
	mov al, 7
	call Write 
	mov al, 8
	call Write
	mov al, 6
	mov bl, 8
	call Write 
	mov al, 7
	call Write 
	mov al, 8
	call Write 
	mov al, 9
	mov bl, 5
	call Write 
	mov bl, 6 
	call Write
	mov bl, 7
	call Write 

	;--------------------
	;Bottom Left
	mov al, 11
	mov bx, 5
	call Write 
	inc bx
	call Write 
	inc bx 
	call Write
	mov ax, 12
	mov bx, 3
	call Write
	inc ax
	call Write
	inc ax
	call Write
	mov ax, 12
	mov bx, 8
	call Write 
	inc ax
	call Write 
	inc ax
	call Write 
	mov ax, 16
	mov bx, 5
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 

	;--------------------
	;Top right 
	mov ax, 4
	mov bx, 11
	call Write 
	inc bx
	call Write 
	inc bx 
	call Write
	mov ax, 6
	mov bx, 10
	call Write 
	inc ax
	call Write 
	inc ax 
	call Write 
	mov ax, 6
	mov bx, 15
	call Write 
	inc ax
	call Write 
	inc ax 
	call Write 
	mov ax, 9
	mov bx, 11
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 

	;--------------------
	;Bottom Right 
	mov ax, 11
	mov bx, 11
	call Write
	inc bx 
	call Write
	inc bx 
	call Write
	mov ax, 12
	mov bx, 10
	call Write 
	inc ax 
	call Write 
	inc ax 
	call Write
	mov ax, 12
	mov bx, 15
	call Write
	inc ax
	call Write
	inc ax 
	call Write
	mov ax, 16
	mov bx, 11	
	call Write
	inc bx 
	call Write
	inc bx 
	call Write

	ret
Pulsar ENDP

;------------------------------------------------------

Pentadecathlon Proc

	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 25
	mov ebx, 30 
	call Write 
	inc ebx
	call Write 
	inc ebx 
	call Write 
	inc ebx 
	add eax, 2
	call Write 
	inc eax
	call Write 
	add eax, 2
	dec ebx 
	call Write 
	dec ebx 
	call Write 
	dec ebx 
	call Write 
	dec ebx 
	sub eax, 2
	call Write 
	dec eax
	call Write 
	sub eax, 10
	inc ebx 
	call Write 
	inc ebx 
	call Write 
	inc ebx 
	call Write 
	inc ebx 
	add eax, 2
	call Write 
	inc eax
	call Write 
	add eax, 2
	dec ebx
	call Write 
	dec ebx
	call Write 
	dec ebx
	call Write 
	dec ebx 
	sub eax, 2
	call Write 
	dec eax
	call Write

	ret
Pentadecathlon ENDP

;------------------------------------------------------

Glider Proc 
	
	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 20 
	mov ebx, 31
	call Write 
	inc ebx 
	inc eax 
	call Write 
	inc eax 
	call Write 
	dec ebx 
	call Write 
	dec ebx
	call Write

	ret
Glider ENDP

;------------------------------------------------------

HeavyweightStarship Proc 
	
	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov eax, 25
	mov ebx, 43
	call Write 
	inc ebx 
	call Write 
	add ebx, 2
	inc eax 
	call Write 
	inc eax
	inc ebx 
	call Write 
	inc eax 
	call Write 
	inc eax 
	call Write 
	dec ebx 
	call Write 
	dec ebx 
	call Write
	dec ebx 
	call Write
	dec ebx 
	call Write
	dec ebx 
	call Write
	dec ebx 
	dec eax 
	call Write 
	sub eax, 2
	call Write 

	ret
HeavyweightStarship ENDP
				
;-----------------------------------------

;Self-Created Choas 
;Init Procedure 
;---------------
Init Proc 

	mov eax, 0 
	mov ebx, 0
	mov esi, OFFSET grid 
	mov cl, AliveChar

	mov ax, 20
	mov bx, 30
	call Write	
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write
	inc ax
	call Write
	inc ax
	call Write 
	inc ax 
	dec bx 
	call Write 
	sub bx, 2
	inc ax 
	call Write 
	dec bx 
	call Write 
	dec ax 
	sub bx, 2
	call Write 
	sub ax, 2
	call Write 

	;-----------------------------------------------------

	mov ax, 20
	mov bx, 15
	call Write	
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write
	
	inc ax
	call Write
	inc ax
	call Write 

	inc ax 
	dec bx 
	call Write 

	sub bx, 2
	inc ax 
	call Write 
	dec bx 
	call Write 
	dec ax 
	sub ax, 4
	call Write 
	sub ax, 2
	call Write 

	;-----------------------------------------------------
	;Top Left
	mov al, 40
	mov bl, 50
	call Write 
	inc bx
	call Write 
	inc bx
	call Write 

	mov ax, 42
	mov bx, 48
	call Write 
	inc ax
	call Write 
	inc ax
	call Write 

	mov al, 42
	mov bl, 47
	call Write 
	inc ax
	call Write 
	inc ax
	call Write 

	mov al, 45
	mov bl, 44
	call Write 
	inc bx 
	call Write
	inc bx 
	call Write 

	;Glider
	;-----------------------------------------------------
	mov al, 24
	mov bl, 25
	mov cl, AliveChar 
	call Write

	mov al, 25
	mov bl, 25
	call Write 
	
	mov al, 26
	mov bl, 25
	call Write 

	mov al, 26
	mov bl, 24
	call Write 

	mov al, 25
	mov bl, 23
	call Write 
	;-----------------------------------------------------
	
	;Pulsar 
	;-----------------------------------------------------
	;Top Left
	mov al, 4
	mov bl, 5
	mov cl, AliveChar 
	call Write 
	inc bx
	call Write 
	inc bx
	call Write 

	mov al, 6
	mov bl, 3
	call Write 
	mov al, 7
	call Write 
	mov al, 8
	call Write 

	mov al, 6
	mov bl, 8
	call Write 
	mov al, 7
	call Write 
	mov al, 8
	call Write 

	mov al, 9
	mov bl, 5
	call Write 
	mov bl, 6 
	call Write
	mov bl, 7
	call Write 

	;--------------------
	;Bottom Left
	mov al, 11
	mov bx, 5
	call Write 
	inc bx
	call Write 
	inc bx 
	call Write 

	mov ax, 12
	mov bx, 3
	call Write
	inc ax
	call Write
	inc ax
	call Write

	mov ax, 12
	mov bx, 8
	call Write 
	inc ax
	call Write 
	inc ax
	call Write 

	mov ax, 16
	mov bx, 5
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 

	;--------------------
	;Top right 
	mov ax, 4
	mov bx, 11
	call Write 
	inc bx
	call Write 
	inc bx 
	call Write

	mov ax, 6
	mov bx, 10
	call Write 
	inc ax
	call Write 
	inc ax 
	call Write 

	mov ax, 6
	mov bx, 15
	call Write 
	inc ax
	call Write 
	inc ax 
	call Write 

	mov ax, 9
	mov bx, 11
	call Write 
	inc bx 
	call Write 
	inc bx 
	call Write 

	;--------------------
	;Bottom Right 
	mov ax, 11
	mov bx, 11
	call Write
	inc bx 
	call Write
	inc bx 
	call Write

	mov ax, 12
	mov bx, 10
	call Write 
	inc ax 
	call Write 
	inc ax 
	call Write

	mov ax, 12
	mov bx, 15
	call Write
	inc ax
	call Write
	inc ax 
	call Write

	mov ax, 16
	mov bx, 11	
	call Write
	inc bx 
	call Write
	inc bx 
	call Write
	;-----------------------------------------------------

	ret 
Init ENDP 

;-----------------------------------------

END main 